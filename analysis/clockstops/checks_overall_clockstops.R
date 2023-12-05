###############################################################
# This script checks for incorrect values in dates
# for all people with a closed RTT pathway (May21-May22)
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
library(stringr)

## Rounding function
source(here("analysis", "custom_functions.R"))

## Create directories if needed
dir_create(here::here("output", "clockstops"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "measures"), showWarnings = FALSE, recurse = TRUE)


# Check overall counts over time to compare with published data
overall <- read_csv(here::here("output", "measures", "measures_checks.csv"),
                       col_types = cols(interval_start = col_date(format="%Y-%m-%d"),
                                        interval_end = col_date(format="%Y-%m-%d"))) %>%
  rename(month = interval_start) %>%
  mutate(count = numerator,
         admit_type = ifelse(grepl("_not_admit_", measure, fixed = TRUE), "Not admitted", 
                             ifelse(grepl("_admit_", measure, fixed = TRUE), "Admitted",
                                    "Total")),
         trt_func = ifelse(grepl("_ortho", measure, fixed = TRUE), "Orthopaedic",
                           ifelse(grepl("_total", measure, fixed = TRUE), "Total",
                                  paste0("Treatment_function_",str_sub(measure, start= -3))))) %>%
  dplyr::select(!c(ratio, denominator, interval_end, numerator, measure)) %>%
  pivot_wider(names_from = trt_func, values_from = count) 

write.csv(overall, here::here("output", "clockstops", "check_overall_month.csv"), row.names = FALSE)

