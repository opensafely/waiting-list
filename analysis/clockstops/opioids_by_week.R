###############################################################
# This script plots opioid prescribing per 100 people by week
# in 6 months prior to WL start, during WL, and 6 months after WL end
###############################################################


# For running locally only #
#setwd("C:/Users/aschaffer/OneDrive - Nexus365/Documents/GitHub/waiting-list")
#getwd()


## Import libraries
library('tidyverse')
library('lubridate')
library('here')
library('dplyr')
library('ggplot2')
library('zoo')
library('reshape2')
library('fs')

## Rounding function
source(here("analysis", "custom_functions.R"))

## Create directories if needed
dir_create(here::here("output", "clockstops"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "measures"), showWarnings = FALSE, recurse = TRUE)


# Load data
any_opioid_rx <- read_csv(here::here("output", "measures", "measures_any_opioid.csv"),
                          col_types = cols(interval_start = col_date(format="%Y-%m-%d"))) %>%
              mutate(opioid_type = "Any opioid")

long_opioid_rx <- read_csv(here::here("output", "measures", "measures_long_opioid.csv"),
                          col_types = cols(interval_start = col_date(format="%Y-%m-%d"))) %>%
              mutate(opioid_type = "Long-acting opioid")

short_opioid_rx <- read_csv(here::here("output", "measures", "measures_short_opioid.csv"),
                          col_types = cols(interval_start = col_date(format="%Y-%m-%d"))) %>%
              mutate(opioid_type = "Short-acting opioid")

weak_opioid_rx <- read_csv(here::here("output", "measures", "measures_weak_opioid.csv"),
                          col_types = cols(interval_start = col_date(format="%Y-%m-%d"))) %>%
  mutate(opioid_type = "Weak opioid")

strong_opioid_rx <- read_csv(here::here("output", "measures", "measures_strong_opioid.csv"),
                          col_types = cols(interval_start = col_date(format="%Y-%m-%d"))) %>%
  mutate(opioid_type = "Strong opioid")

####

opioid_rx <- rbind(any_opioid_rx, long_opioid_rx, short_opioid_rx, weak_opioid_rx, strong_opioid_rx) %>%
              mutate(
                # Convert interval start date to number of weeks 
                week = as.numeric(((interval_start - as.Date("2000-01-01"))/7)) + 1,
                period = ifelse(grepl("pre", measure), "Pre-WL", 
                                ifelse(grepl("post", measure), "Post-WL", "During WL")),
                
                admitted = as.character(admitted),
                admitted = ifelse(admitted == "TRUE", "Admitted", "Not admitted"),
                
                opioid_rx = numerator
              )  %>%
              dplyr::select(c(opioid_type, opioid_rx, denominator, opioid_type, admitted,
                              week, period, routine)) %>%
              arrange(opioid_type, routine, period, week)

opioid_rx <- opioid_rx[,c("opioid_type", "routine", "admitted", "period", 
                           "week", "opioid_rx", "denominator")]

write.csv(opioid_rx, file = here::here("output", "clockstops", "opioid_by_week.csv"),
          row.names = FALSE)


#####


strat_opioid_rx <- read_csv(here::here("output", "measures", "measures_opioid_stratified.csv"),
                                 col_types = cols(interval_start = col_date(format="%Y-%m-%d"))) %>%
  mutate(opioid_type = "Any opioid",
         prior_opioid_rx = as.character(prior_opioid_rx),
         category = coalesce(prior_opioid_rx, age_group, imd_decile, sex, region, ethnicity),
         
         variable = ifelse(!is.na(prior_opioid_rx), "Prior opioid Rx",
                           ifelse(!is.na(age_group), "Age",
                                  ifelse(!is.na(imd_decile), "IMD decile", 
                                      ifelse(!is.na(region), "Region",
                                            ifelse(!is.na(ethnicity), "Ethnicity", 
                                                   "Sex"))))),
         
         # Convert interval start date to number of weeks 
         week = as.numeric(((interval_start - as.Date("2000-01-01"))/7)) + 1,
         period = ifelse(grepl("pre", measure), "Pre-WL", 
                         ifelse(grepl("post", measure), "Post-WL", "During WL")),
         
         admitted = as.character(admitted),
         admitted = ifelse(admitted == "TRUE", "Admitted", "Not admitted"),
         
         opioid_rx = numerator
  )  %>%
  dplyr::select(c(opioid_type, opioid_rx, denominator, opioid_type, admitted,
                  week, variable, category, period, routine)) %>%
  arrange(opioid_type, routine, variable, category, period, week)

strat_opioid_rx <- strat_opioid_rx[,c("opioid_type", "routine", "admitted", "period", 
                          "variable", "category", "week", "opioid_rx", "denominator")]

write.csv(strat_opioid_rx, file = here::here("output", "clockstops", "opioid_by_week_stratified.csv"),
          row.names = FALSE)

