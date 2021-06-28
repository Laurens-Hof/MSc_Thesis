
#SETUP  ----------------------------------------------------------------------------------

#hygiene
rm(list = ls())
#setup
library(tidyverse)
library(modelr)
setwd("~/School/Msc Economics Tilburg/Thesis/Data")

#define lists of all the countries I have CA data on
used_abbrev <- c("AT", "BE", "HR", "CZ", "FR", "DE", "EL", "HU", "IE", "IT",
                 "LV", "MT", "NL", "PL", "SK", "SE", "US", "LT", "ES")
used_names <- c("Austria", "Belgium", "Croatia", "Czech Republic", "France",
                "Germany", "Greece", "Hungary", "Ireland", "Italy",
                "Latvia", "Malta", "Netherlands", "Poland", "Slovakia",
                "Sweden", "United States", "Lithuania", "Spain")
#define a list of the industries I am going to use
used_industries <- c("A", "B", "C10-C12", "C13-C15", "C16-C18",
                     "C19", "C20", "C21", "C22_C23", "C24_C25",
                     "C26", "C27", "C28", "C29_C30", "C31-C33",
                     "D", "E", "F", "G", "H", "I",
                     "J58-J60", "J61", "J62_J63", "K", "M_N",
                     "R_S")

#PROGRAMMATIC POSTION------------------------------------------------------------------

#import data on party positions
party_pos <- read_csv("MPDataset.csv")
#make dates workable
party_pos$edate <- parse_date(party_pos$edate, format = "%d/%m/%Y")

#I will create a statistic of the average per403 score of parties,
#weighted by their vote shares in the legislature
#I will also keep only 21st-century elections
position <- party_pos %>%
  filter(edate > as.Date("1995-01-01")) %>%
  group_by(countryname, edate) %>%
  summarise(per403 = weighted.mean(per403, pervote, na.rm = TRUE),
            per404 = weighted.mean(per404, pervote, na.rm = TRUE),
            per505 = weighted.mean(per505, pervote, na.rm = TRUE)) %>%
  mutate(year = as.numeric(str_sub(edate, 1, 4))) %>%
  rename(country = countryname)

#BUDGETS-------------------------------------------------------------------------------

#import data on Competition Authorities' budgets
CA_budgets <- read_csv("CA_budgets.csv")
CA_budgets <- select(CA_budgets, country, year, budget)

CA_budgets <- CA_budgets %>%
  left_join(position, by = c("country", "year"))%>%
  fill(per403, per404, per505)

#MERGERS-------------------------------------------------------------------------------

CA_mergers <- read_csv("CA_mergers.csv")

#create the fraction of mergers not cleared in phase 1
CA_mergers <- CA_mergers %>%
  mutate(mergerrate = `phase 2 or not cleared`/(`appr pre phase 2` + `phase 2 or not cleared`))%>%
  mutate(totmerger = `appr pre phase 2` + `phase 2 or not cleared`) %>%
  select(country, year, mergerrate, totmerger)

CA_budgets <- left_join(CA_budgets, CA_mergers, by = c("country", "year"))

#PRODUCTIVITY--------------------------------------------------------------------------

#import the national accounts data
euklems_NA <- read_csv("Statistical_National-Accounts.csv")

#extract country-level GDP
GDP <- euklems_NA %>%
  #take gross output
  filter(var == "GO") %>%
  #take gross output for the total economy
  filter(code == "TOT") %>%
  #move years to rows
  gather(`1995`:`2017`, key = "year", value = "GDP") %>%
  mutate(year = as.numeric(year)) %>%
  #keep used countries
  filter(country %in% used_abbrev) %>%
  select(country, year, GDP)

GDP$country <- plyr::mapvalues(GDP$country, used_abbrev, used_names)

#preparation for industry-level producivity calculation
euklems_NA <- euklems_NA %>%
  #move years into rows
  gather(`1995`:`2017`, key = "year", value = "value") %>%
  #move variables into columns
  spread(key = "var", value = "value") %>%
  #grab the variables I need:
  #id, value added, hours worked, and gross output.
  select(country, code, year, H_EMP, VA, GO) %>%
  #create labor productivity column
  mutate(LP = VA/H_EMP) %>%
  #keep only countries with CA data
  filter(country %in% used_abbrev) %>%
  #keep industries of interest
  filter(code %in% used_industries)

#import growth accounts
euklems_G <- read_csv("Statistical_Growth-Accounts.csv")

euklems_G <- euklems_G %>%
  #repeat the reshaping, identical to the NA table
  gather(`1995`:`2017`, key = "year", value = "value") %>%
  spread(key = "var", value = "value") %>%
  #keep identifiers and TFP
  select(country, code, year, CAP, LAB) %>%
  #discard unnecessary rows
  filter(country %in% used_abbrev) %>%
  filter(code %in% used_industries)

industries <- left_join(euklems_NA, euklems_G, by = c("country", "code", "year"))

#country-industry indicator for later convenience
industries <- unite(industries, country, code, col = "cou_ind", remove = FALSE)


#look at the patterns in capital payment NA's, exlcuding those that are missing
#due to being the first year
browseNAs <- industries[is.na(industries$CAP) == TRUE & industries$year > 1995,]  
#Turns out that there's a bunch of industries where all the data is missing.
#We will drop those completely. There are virtually no "scattered" NA's

industries <- industries %>%
  #leave out first year
  filter(year > 1995) %>%
  #leave out the problematic sectors
  filter(!(cou_ind %in% browseNAs$cou_ind)) %>%
  #replace negative entries with 1 to enable logs
  mutate(CAP = if_else(CAP < 0, 0.1, CAP)) %>%
  mutate(LAB = if_else(LAB < 0, 0.1, LAB)) %>%
  mutate(VA = if_else(VA < 0, 0.1, VA))

#calculate the tfp proper 
industries <- industries %>%
  #we allow differing weights per country-industry by grouping before running the
  #solow regression
  group_by(cou_ind) %>%
  #extract the residuals and save them in a column named TFP
  mutate(TFP = residuals(lm(log(VA) ~ log(CAP) + log(LAB)))) %>%
  ungroup()

#define frontiers for each industry-year
industries <- industries %>%  
  group_by(code, year) %>%
  mutate(TFPfrontier = max(TFP)) %>%
  mutate(LPfrontier = max(LP, na.rm = TRUE)) %>%
  ungroup()

#replace country codes with English names, to better fit with other data sources
industries$country <- plyr::mapvalues(industries$country, used_abbrev, used_names)
#coerce year to numeric
industries$year <- as.numeric(industries$year)

#combine into one dataframe, select only useful columns, and do some renaming
combined <- industries %>%
  left_join(CA_budgets, by = c("country", "year")) %>%
  left_join(GDP, by = c("country", "year")) %>%
  select(country, code, year,
         VA, LP, LPfrontier,
         TFP, TFPfrontier, mergerrate,
         totmerger, budget, per403,
         per404, per505,
         GO, GDP) %>%
  rename(Output = GO)

#EDUCATION------------------------------------------------------------------------------

#education
euklems_L <- read_csv("Statistical_Labour.csv",
                      col_types = "ccciciiiddddddddddddddddddddddd")

euklems_L <- euklems_L %>%
  filter(var == "H_shares") %>%
  #move years to rows
  gather(`1995`:`2017`, key = "year", value = "percentage") %>%
  #keep used industries
  filter(code %in% used_industries | code == "C" | code == "J") %>%
  #keep years with data
  filter(year > 2007) %>%
  #move gender data columns, then sum them. We don't care about gender subdivisions
  spread(gender, percentage) %>%
  mutate(percentage = `1` + `2`, .keep = "unused") %>%
  #do the same for ages
  spread(age, percentage) %>%
  mutate(pct_hi_edu = `1` + `2` + `3`, .keep = "unused") %>%
  #keep only the percentages highly educated
  filter(edu == 1) %>%
  select(country, code, year, pct_hi_edu) %>%
  #make year into numeric
  mutate(year = as.numeric(year)) %>%
  filter(country %in% used_abbrev) %>%
  #we will assume that the percentage of highly educated workers is constant
  #across industries. To compute it, move the industries to columns again
  spread(code, pct_hi_edu) %>%
  #now "create" subsectors manually
  mutate(`C10-C12` = C,
         `C13-C15` = C,
         `C16-C18` = C,
         `C19` = C,
         `C20` = C,
         `C21` = C,
         `C22_C23` = C,
         `C24_C25` = C,
         `C26` = C,
         `C27` = C,
         `C28` = C,
         `C29_C30` = C,
         `C31-C33` = C,
         `J58-J60` = J,
         `J61` = J,
         `J62_J63` = J,
         .keep = "unused") %>%
  gather(`A`:`J62_J63`, key = "code", value = "pct_hi_edu")

euklems_L$country <- plyr::mapvalues(euklems_L$country, used_abbrev, used_names)

combined <- left_join(combined, euklems_L, by = c("country", "year", "code"))

#TRADE OPENNESS----------------------------------------------------------------------------

#trade openness
trade <- read_csv("Trade.csv")

trade <- trade %>%
  #select useful columns          
  select(TIME, GEO, NACE_R2, Value) %>%
  #bring names in line with other tables
  rename(year = TIME, country = GEO, code = NACE_R2, imports = Value) %>%
  #keep only useful countries
  filter(country %in% used_names) %>%
  #make the values into doubles
  mutate(imports = str_replace_all(imports, " ", "")) %>%
  mutate(imports = as.numeric(imports))

#since there are no natural zeroes, we can sub in zeroes for NA, aggregate
#columns into the EUKLEMS subdivision, and then change remaining zeroes into NA again
trade$imports[is.na(trade$imports)] <- 0

trade <- trade %>%
  #move industry codes to columns for aggregation using mutate()
  spread(key = code, value = imports) %>%
  #
  mutate(`C10-C12` = C10 + C11 + C12,
         `C13_C15` = C13 + C14 + C15,
         `C16-C18` = C16 + C17 + C18,
         `C22_C23` = C22 + C23,
         `C24_C25` = C24 + C25,
         `C29_C30` = C29 + C30,
         `C31-C33` = C31 + C32 + C33,
         `J58-J60` = J58 + J59 + J60,
         `J62_J63` = J62 + J63,
         `M_N` = M + N,
         `R_S` = R + S,
         .keep = "unused") %>%
  #move the industries back to long format
  gather(A:R_S, key = "code", value = "imports")

#reintroduce the NA's where none of the subdivisions had any data
trade$imports[trade$imports == 0] <- NA

#join with main data
combined <- left_join(combined, trade, by = c("country", "year", "code")) %>%
  #create trade intensity variable
  mutate(openness = imports/Output)

#RnD intensity-----------------------------------------------------------------------

rd <- read_csv("rd.csv", col_types =  "dcccdc")

rd <- rd %>%
  filter(GEO %in% used_names) %>%
  spread(NACE_R2, Value) %>%
  mutate(`C22_C23` = C22 + C23,
         `C24_C25` = C24 + C25,
         `C29_C30` = C29 + C30,
         `C31-C33` = C31 + C32 + C33,
         `J62_J63` = J62 + J63,
         .keep = "unused") %>%
  gather(A:`J62_J63`, key = "code", value = "BERD") %>%
  select(-UNIT, -`Flag and Footnotes`) %>%
  rename(year = TIME, country = GEO)

combined <- left_join(combined, rd, by = c("country", "year", "code")) %>%
  mutate(RD_intensity = BERD/Output)




#SAVE--------------------------------------------------------------------------------
write_csv(combined, "finaldata.csv")