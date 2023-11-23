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


hi_opioid_rx <- read_csv(here::here("output", "measures", "measures_hi_opioid.csv"),
                         col_types = cols(interval_start = col_date(format="%Y-%m-%d"))) %>%
              mutate(opioid_type = "High dose opioid", var = "Full cohort", category = "Full cohort") %>%
              dplyr::select(c(numerator, denominator, opioid_type, var, category,
                              interval_start,measure)) 

any_opioid_rx <- read_csv(here::here("output", "measures", "measures_any_opioid.csv"),
                          col_types = cols(interval_start = col_date(format="%Y-%m-%d"))) %>%
              mutate(opioid_type = "Any opioid",
              
              category = coalesce(as.character(prior_opioid_rx), imd_decile, age_group,
                                  ethnicity, sex), 
              category = ifelse(is.na(category), "Full cohort", category),
      
              var = ifelse(!is.na(prior_opioid_rx), "Prior opioid Rx",
                           ifelse(!is.na(imd_decile), "IMD decile",
                                  ifelse(!is.na(age_group), "Age group",
                                         ifelse(!is.na(ethnicity), "Ethnicity",
                                                ifelse(!is.na(sex), "Sex",
                                                    "Full cohort")))))
              )  %>%
              dplyr::select(c(numerator, denominator, opioid_type, var, category,
                              interval_start, measure)) 

opioid_rx <- rbind(any_opioid_rx, hi_opioid_rx) %>%
  mutate(
    # Convert interval start date to number of weeks 
    week = as.numeric(((interval_start - as.Date("2000-01-01"))/7)) + 1,
    period = ifelse(grepl("pre", measure), "Pre-WL", 
                    ifelse(grepl("post", measure), "Post-WL", "During WL")),
    
    # Rounding
    opioid_rx = rounding(numerator),
    denominator = rounding(denominator),
    
    # Rx per 100 people
    rate = opioid_rx / denominator * 100,
    rate_lci = rate - (1.96 * sqrt( ( rate * (100 - rate) ) / denominator)),
    rate_uci = rate + (1.96 * sqrt( ( rate * (100 - rate) ) / denominator)),
    
    rate = ifelse((var == "Prior opioid Rx" & period == "Pre-WL"), 0, rate),
    rate_lci = ifelse((var == "Prior opioid Rx" & period == "Pre-WL"), 0, rate_lci),
    rate_uci = ifelse((var == "Prior opioid Rx" & period == "Pre-WL"), 0, rate_lci)
  )%>% 
  dplyr::select(!c(interval_start, numerator, measure)) %>%
  arrange(opioid_type, var, category, period, week)

write.csv(opioid_rx, file = here::here("output", "clockstops", "opioid_by_week.csv"),
          row.names = FALSE)

