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


############## Data cleaning ############

opioid_rx <- rbind(any_opioid_rx, long_opioid_rx, short_opioid_rx, weak_opioid_rx, strong_opioid_rx) %>%
              mutate(
                # Convert interval start date to number of weeks 
                week = as.numeric(((interval_start - as.Date("2000-01-01"))/7)) + 1,
                period = ifelse(grepl("pre", measure), "Pre-WL", 
                                ifelse(grepl("post", measure), "Post-WL", 
                                       "During WL")),
                
                opioid_rx = numerator,
                prior_opioid_rx = ifelse(is.na(prior_opioid_rx), "Full cohort",
                                       ifelse(prior_opioid_rx == TRUE, "Prior opioid Rx", 
                                              "Opioid naive")),
                
                wait_gp = ifelse(is.na(wait_gp), "All", wait_gp)
              )  %>%
              dplyr::select(c(opioid_type, opioid_rx, denominator, wait_gp,
                              week, period, prior_opioid_rx)) %>%
              filter(wait_gp == "All" |  period == "Post-WL")

#%>% subset(!(wait_gp != "All" & opioid_type != "Any opioid"))

opioid_rx <- opioid_rx[,c("opioid_type", "period",  "prior_opioid_rx", "wait_gp", 
                          "week", "opioid_rx", "denominator")]


############## Split into multiple files #################

# Full cohort only
opioid_rx_full <- opioid_rx %>%
  subset(prior_opioid_rx == "Full cohort") %>%
  arrange(opioid_type, prior_opioid_rx, period, wait_gp, week)

write.csv(opioid_rx_full, file = here::here("output", "clockstops", "opioid_by_week_full.csv"),
          row.names = FALSE)


# By prior opioid prescribing
opioid_rx_prior <- opioid_rx %>%
  subset(prior_opioid_rx != "Full cohort" & wait_gp == "All") %>%
  arrange(opioid_type, prior_opioid_rx, period, wait_gp, week)


write.csv(opioid_rx_prior, file = here::here("output", "clockstops", "opioid_by_week_prior.csv"),
          row.names = FALSE)


# By wait time
opioid_rx_wait <- opioid_rx %>%
  subset(prior_opioid_rx != "Full cohort" & wait_gp != "All") %>%
  filter(opioid_type == "Any opioid" | prior_opioid_rx == "Prior opioid Rx") %>%
  arrange(opioid_type, prior_opioid_rx, period, wait_gp, week)


write.csv(opioid_rx_wait, file = here::here("output", "clockstops", "opioid_by_week_wait.csv"),
          row.names = FALSE)


#####





