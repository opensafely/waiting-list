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


opioid_rx <- rbind(any_opioid_rx, long_opioid_rx, short_opioid_rx, weak_opioid_rx, strong_opioid_rx) %>%
              mutate(category = coalesce(as.character(prior_opioid_rx), imd_decile, age_group, sex), 
                category = ifelse(is.na(category), "Full cohort", category),
                var = ifelse(!is.na(prior_opioid_rx), "Prior opioid Rx",
                           ifelse(!is.na(imd_decile), "IMD decile",
                                  ifelse(!is.na(age_group), "Age group",
                                                ifelse(!is.na(sex), "Sex",
                                                    "Full cohort")))),
                # Convert interval start date to number of weeks 
                week = as.numeric(((interval_start - as.Date("2000-01-01"))/7)) + 1,
                period = ifelse(grepl("pre", measure), "Pre-WL", 
                                ifelse(grepl("post", measure), "Post-WL", "During WL")),
                
                opioid_rx = numerator
              )  %>%
              dplyr::select(c(opioid_type, opioid_rx, denominator, opioid_type, 
                              var, week, category, period, routine)) %>%
              arrange(opioid_type, routine, var, category, period, week)

opioid_rx <- opioid_rx[,c("opioid_type", "routine", "period", "var", "category", "opioid_rx", "denominator")]

write.csv(opioid_rx, file = here::here("output", "clockstops", "opioid_by_week.csv"),
          row.names = FALSE)

