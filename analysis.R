#Setup-----------------------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(plm)
library(Formula)
library(broom)

setwd("~/School/Msc Economics Tilburg/Thesis/Data")

data <- read_csv("finaldata.csv")

#Final Transformations-------------------------------------------------------------------
data <- data %>%
  unite(country, code, col = "cou_ind", remove = FALSE) %>%
  filter(year > 2005) %>%
  unite(cou_ind, year, col = "fullid", remove = FALSE)

#for some reason double observations have crept in and are not removed by the unique()
#function, so I will have to do it manually:
prev <- c()
data$first <- NA
#iterate over the full ids and check if they're the first instance
for(i in 1:nrow(data)){
  #if we have not seen this id previously,
  if(!(data$fullid[i] %in% prev)){
    #store this id in prev
    prev <- c(prev, data$fullid[i])
    #set "first" to 1
    data$first[i] <- 1
  } else {
    data$first[i] <- 0
  }
}
#select only the unique values
data <- filter(data, first == 1)

#deflate budgets and merger volumen by GDP
data <- data %>%
  mutate(budgets_def = budget/GDP) %>%
  mutate(mergers_def = totmerger/GDP)


#create a function that lags the panel correctly, taking into account the groups
lag_groups <- function(frame, series, name){
  lagged <- frame %>%
    #select only group identifier, year identifier and series to be lagged
    select(cou_ind, year, series) %>%
    #move the groups to the columns
    pivot_wider(names_from = cou_ind, values_from = series) %>%
    #apply the lag() function to all columns (groups) except year id
    mutate(across(.cols = -year, dplyr::lag)) %>%
    #move the groups to the rows again, call the resulting column "name"
    pivot_longer(-year, names_to = "cou_ind", values_to = name)
  #join the resulting dataframe on the main data, and return the updated table
  return(left_join(frame, lagged, by = c("cou_ind", "year")))
}

#lag the three activity indicators
data <- lag_groups(data, "mergerrate", "mergerrate_lag")
data <- lag_groups(data, "mergers_def", "mergers_def_lag")
data <- lag_groups(data, "budgets_def", "budgets_def_lag")

#lag productivity so we can compute first differences
data <- lag_groups(data, "TFP", "TFP_lag")
data <- lag_groups(data, "LP", "LP_lag")
data <- lag_groups(data, "LPfrontier", "LPfrontier_lag")
data <- lag_groups(data, "TFPfrontier", "TFPfrontier_lag")

#productivity transformations
data <- data %>%
  #compute the change in productivity
  mutate(dTFP = TFP - TFP_lag,
         dLP = LP - LP_lag,
         #compute the change in frontier productivity
         dTFPfrontier = TFPfrontier - TFPfrontier_lag,
         dLPfrontier = LPfrontier - LPfrontier_lag,
         #compute distance to frontier
         TFPdist = TFPfrontier/TFP,
         LPdist = LPfrontier/LP)

#lag the instruments
data <- lag_groups(data, "per403", "per403_lag")
data <- lag_groups(data, "per404", "per404_lag")
data <- lag_groups(data, "per505", "per505_lag")

#Trimming Fucntion--------------------------------------------------------------------------------

#create function to trim the NA's stepwise, since they're strongly concentrated
stepwise_trim <- function(var, cut1 = 0.9, cut2 = 0.4, cut3 = 0.2){
  
  #step 1: first pass over years
  #create table indicating proportion missing by year
  nas_y_1 <- data %>%
    group_by(year) %>%
    summarize(prop = sum(is.na(.data[[var]])/length(.data[[var]])))
  #keep only those years where the proportion of NA's is below cutoff 1
  frame1 <- filter(data, year %in% nas_y_1$year[nas_y_1$prop < cut1])
  
  #step 2: first pass over country-industries
  #create the na table
  nas_c_1 <- frame1 %>%
    group_by(cou_ind) %>%
    summarize(prop = sum(is.na(.data[[var]]))/length(.data[[var]]))
  #filter out the industries where prop is greater than cutoff 1
  frame2 <- filter(frame1, cou_ind %in% nas_c_1$cou_ind[nas_c_1$prop < cut1])
  
  #step 3: second pass over years, this time using cutoff 2
  nas_y_2 <- frame2 %>%
    group_by(year) %>%
    summarize(prop = sum(is.na(.data[[var]]))/length(.data[[var]]))
  frame3 <- filter(frame2, year %in% nas_y_2$year[nas_y_2$prop < cut2])
  
  #step 4: second pass over country-industries, using a possibly different cutoff 3
  nas_c_2 <- frame3 %>%
    group_by(cou_ind) %>%
    summarize(prop = sum(is.na(.data[[var]]))/length(.data[[var]]))
  frame4 <- filter(frame3, cou_ind %in% nas_c_2$cou_ind[nas_c_2$prop < cut3])
  
  return(frame4)
}

#Descriptives---------------------------------------------------------------------------

descriptives <- data %>%
  #describe the independent variables,
  summarize(across(c(mergerrate_lag, mergers_def_lag, budgets_def_lag,
                     #the dependent variables,
                     dTFP, dLP,
                     #and the instruments
                     per403_lag, per404_lag, per505_lag),
                   ~summary(.x, na.rm = TRUE))) %>%
  mutate(stats = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "NA's")) %>%
  relocate(stats)

#write_csv(descriptives, "results/descriptives.csv")

#IVs----------------------------------------------------------------------------

#mergerrate on TPF
rate_t <- stepwise_trim("mergerrate_lag")
rate_t <- select(rate_t, cou_ind, year,
                 #independent variable 
                 mergerrate_lag,
                 #dependent variable 
                 dTFP,
                 #controls 
                 dTFPfrontier, TFPdist, pct_hi_edu,
                 #instruments 
                 per403_lag, per404_lag, per505_lag)
rate_t <- pdata.frame(rate_t, c("cou_ind", "year"))

iv_rate_t <- tidy(plm(dTFP ~ mergerrate_lag + dTFPfrontier + TFPdist + pct_hi_edu |
                   .-mergerrate_lag + per403_lag + per404_lag + per505_lag,
                 data = rate_t, model = "within"))
iv_rate_t$dep_var <- "TFP change"
ivs <- iv_rate_t[1,]

#mergerrate on LP

rate_l <- stepwise_trim("mergerrate_lag")
rate_l <- select(rate_l, cou_ind, year,
                 mergerrate_lag,
                 dLP,
                 dLPfrontier, LPdist, pct_hi_edu,
                 per403_lag, per404_lag, per505_lag)
rate_l <- pdata.frame(rate_l, c("cou_ind", "year"))

iv_rate_l <- tidy(plm(dLP ~ mergerrate_lag + dLPfrontier + LPdist + pct_hi_edu |
                        .-mergerrate_lag + per403_lag + per404_lag + per505_lag,
                      data = rate_l, model = "within"))
iv_rate_l$dep_var <- "LP change"
ivs <- bind_rows(ivs, iv_rate_l[1,])

#number of mergers on TFP

num_t <- stepwise_trim("mergers_def_lag")
num_t <- select(num_t, cou_ind, year,
                 mergers_def_lag,
                 dTFP,
                 dTFPfrontier, TFPdist, pct_hi_edu,
                 per403_lag, per404_lag, per505_lag)
num_t <- pdata.frame(num_t, c("cou_ind", "year"))

iv_num_t <- tidy(plm(dTFP ~ log(mergers_def_lag) + dTFPfrontier + TFPdist + pct_hi_edu |
                       .-log(mergers_def_lag) + per403_lag + per404_lag + per505_lag,
                     data = num_t, model = "within"))
iv_num_t$dep_var <- "TFP change"
ivs <- bind_rows(ivs, iv_num_t[1,])

#number of mergers on LP

num_l <- stepwise_trim("mergers_def_lag")
num_l <- select(num_l, cou_ind, year,
                mergers_def_lag,
                dLP,
                dLPfrontier, LPdist, pct_hi_edu,
                per403_lag, per404_lag, per505_lag)
num_l <- pdata.frame(num_l, c("cou_ind", "year"))

iv_num_l <- tidy(plm(dLP ~ log(mergers_def_lag) + dLPfrontier + LPdist + pct_hi_edu |
                        .-log(mergers_def_lag) + per403_lag + per404_lag + per505_lag,
                      data = num_l, model = "within"))
iv_num_l$dep_var <- "LP change"
ivs <- bind_rows(ivs, iv_num_l[1,])

#budget on TFP

bud_t <- stepwise_trim("budgets_def_lag")
bud_t <- select(bud_t, cou_ind, year,
                budgets_def_lag,
                dTFP,
                dTFPfrontier,TFPdist, pct_hi_edu,
                per403_lag, per404_lag, per505_lag)
bud_t <- pdata.frame(bud_t, c("cou_ind", "year"))

iv_bud_t <- tidy(plm(dTFP ~ log(budgets_def_lag) + dTFPfrontier + TFPdist + pct_hi_edu |
                        .-log(budgets_def_lag) + per403_lag + per404_lag + per505_lag,
                      data = bud_t, model = "within"))
iv_bud_t$dep_var <- "TFP change"
ivs <- bind_rows(ivs, iv_bud_t[1,])

#budget on LP

bud_l <- stepwise_trim("budgets_def_lag")
bud_l <- select(bud_l, cou_ind, year,
                budgets_def_lag,
                dLP,
                dLPfrontier, LPdist, pct_hi_edu,
                per403_lag, per404_lag, per505_lag)
bud_l <- pdata.frame(bud_l, c("cou_ind", "year"))

iv_bud_l <- tidy(plm(dLP ~ log(budgets_def_lag) + dLPfrontier + LPdist + pct_hi_edu |
                       .-log(budgets_def_lag) + per403_lag + per404_lag + per505_lag,
                     data = bud_l, model = "within"))
iv_bud_l$dep_var <- "LP change"
ivs <- bind_rows(ivs, iv_bud_l[1,])

write_csv(ivs, "results/iv_regs.csv")

#OLS---------------------------------------------------------------------------------------

#mergerrate on TFP
ols_rate_t <- tidy(plm(dTFP ~ mergerrate_lag + dTFPfrontier + TFPdist + pct_hi_edu,
                       data = rate_t, model = "within"))
ols_rate_t$dep_var <- "TFP change"
ols_regs <- ols_rate_t[1,]

#mergerrate on LP
ols_rate_l <- tidy(plm(dLP ~ mergerrate_lag + dLPfrontier + LPdist + pct_hi_edu,
                       data = rate_l, model = "within"))
ols_rate_l$dep_var <- "LP change"
ols_regs <- bind_rows(ols_regs, ols_rate_l[1,])

#number of mergers on TFP
ols_num_t <- tidy(plm(dTFP ~ log(mergers_def_lag) + dTFPfrontier + TFPdist + pct_hi_edu,
                      data = num_t, model = "within"))
ols_num_t$dep_var <- "TFP change"
ols_regs <- bind_rows(ols_regs, ols_num_t[1,])

#number of mergers on LP
ols_num_l <- tidy(plm(dLP ~ log(mergers_def_lag) + dLPfrontier + LPdist + pct_hi_edu,
                      data = num_l, model = "within"))
ols_num_l$dep_var <- "LP change"
ols_regs <- bind_rows(ols_regs, ols_num_l[1,])

#budgets on TFP
ols_bud_t <- tidy(plm(dTFP ~ log(budgets_def_lag) + dTFPfrontier + TFPdist + pct_hi_edu,
                      data = bud_t, model = "within"))
ols_bud_t$dep_var <- "TFP change"
ols_regs <- bind_rows(ols_regs, ols_bud_t[1,])

ols_bud_l <- tidy(plm(dLP ~ log(budgets_def_lag) + dLPfrontier + LPdist + pct_hi_edu,
                      data = bud_l, model = "within"))
ols_bud_l$dep_var <- "LP change"

ols_regs <- bind_rows(ols_regs, ols_bud_l[1,])

#write_csv(ols_regs, "results/ols_regs.csv")