###############################################################
# This script plots opioid prescribing per 100 people by week
# in 6 months prior to WL start, during WL, and 12 months after WL end
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
library('readr')

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

moderate_opioid_rx <- read_csv(here::here("output", "measures", "measures_moderate_opioid.csv"),
                          col_types = cols(interval_start = col_date(format="%Y-%m-%d"))) %>%
              mutate(opioid_type = "Moderate opioid")

strong_opioid_rx1 <- read_csv(here::here("output", "measures", "measures_strong_opioid1.csv"),
                             col_types = cols(interval_start = col_date(format="%Y-%m-%d"))) %>%
  mutate(opioid_type = "Strong opioid 1")

strong_opioid_rx2 <- read_csv(here::here("output", "measures", "measures_strong_opioid2.csv"),
                             col_types = cols(interval_start = col_date(format="%Y-%m-%d"))) %>%
  mutate(opioid_type = "Strong opioid 2")


############## Data cleaning ############

opioid_rx <- rbind(any_opioid_rx, long_opioid_rx, short_opioid_rx, weak_opioid_rx, strong_opioid_rx1,
                   strong_opioid_rx2, moderate_opioid_rx) %>%
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
                
                num_weeks2 = ifelse(num_weeks>52, 52, num_weeks),
                
                wait_gp = ifelse(num_weeks <= 18, "<=18 weeks",
                                 ifelse(num_weeks > 52, ">52 weeks",
                                        "19-52 weeks"))
                #wait_gp = ifelse(is.na(wait_gp), "All", wait_gp)
              )  %>%
              dplyr::select(c(opioid_type, opioid_rx, denominator, wait_gp,
                              week, period, prior_opioid_rx, oa_diagnosis, hip_hrg, knee_hrg)) 

opioid_rx <- opioid_rx[,c("opioid_type", "period",  "prior_opioid_rx", "wait_gp", "oa_diagnosis", "hip_hrg", "knee_hrg",
                          "week", "opioid_rx", "denominator")]


############## Split into multiple files #################

# Full cohort only
opioid_rx_full <- opioid_rx %>%
  group_by(week, period, opioid_type) %>%
  summarise(opioid_rx = rounding(sum(opioid_rx)),
            denominator = rounding(sum(denominator))) %>%
  arrange(opioid_type, period, week) %>%
  ungroup() %>%
  mutate(prior_opioid_rx = "Full cohort" )

opioid_rx_full <- opioid_rx_full[,c("prior_opioid_rx", "opioid_type", 
                                    "period", "week", "opioid_rx", "denominator")]

write.csv(opioid_rx_full, file = here::here("output", "clockstops", "opioid_by_week_full.csv"),
          row.names = FALSE)


# By prior opioid prescribing
opioid_rx_prior <- opioid_rx %>%
  group_by(week, period, prior_opioid_rx, opioid_type) %>%
  summarise(opioid_rx = rounding(sum(opioid_rx)),
            denominator = rounding(sum(denominator))) %>%
  arrange(prior_opioid_rx, opioid_type, period, week) %>%
  ungroup() 


opioid_rx_prior <- opioid_rx_prior[,c("prior_opioid_rx", "opioid_type", 
                                    "period", "week", "opioid_rx", "denominator")]

write.csv(opioid_rx_prior, file = here::here("output", "clockstops", "opioid_by_week_prior.csv"),
          row.names = FALSE)


# By wait time
opioid_rx_wait <- opioid_rx %>%
  group_by(week, period, prior_opioid_rx, opioid_type, wait_gp) %>%
  summarise(opioid_rx = rounding(sum(opioid_rx)),
            denominator = rounding(sum(denominator))) %>%
  arrange(prior_opioid_rx, wait_gp, opioid_type, period, week) %>%
  subset(!(prior_opioid_rx == "Opioid naive" & opioid_type != "Any opioid")) %>%
  subset(!(period == "During WL" & week == 52))

write.csv(opioid_rx_wait, file = here::here("output", "clockstops", "opioid_by_week_wait.csv"),
          row.names = FALSE)


# People with osteoarthritis only
opioid_rx_oa <- opioid_rx %>%
  group_by(week, period, prior_opioid_rx, opioid_type, oa_diagnosis) %>%
  summarise(opioid_rx = rounding(sum(opioid_rx)),
            denominator = rounding(sum(denominator))) %>%
  arrange(oa_diagnosis, prior_opioid_rx,  opioid_type, period, week) %>%
  subset(opioid_type %in% c("Weak opioid", "Strong opioid", "Any opioid"))

write.csv(opioid_rx_oa, file = here::here("output", "clockstops", "opioid_by_week_oa.csv"),
          row.names = FALSE)


# People with hip procedure
opioid_rx_hip <- opioid_rx %>%
  group_by(week, period, prior_opioid_rx, opioid_type, hip_hrg) %>%
  summarise(opioid_rx = rounding(sum(opioid_rx)),
            denominator = rounding(sum(denominator))) %>%
  arrange(hip_hrg, prior_opioid_rx,  opioid_type, period, week) %>%
  subset(opioid_type %in% c("Weak opioid", "Strong opioid", "Any opioid"))

write.csv(opioid_rx_hip, file = here::here("output", "clockstops", "opioid_by_week_hip.csv"),
          row.names = FALSE)


# People with knee procedure
opioid_rx_knee <- opioid_rx %>%
  group_by(week, period, prior_opioid_rx, opioid_type, knee_hrg) %>%
  summarise(opioid_rx = rounding(sum(opioid_rx)),
            denominator = rounding(sum(denominator))) %>%
  arrange(knee_hrg, prior_opioid_rx, opioid_type, period, week) %>%
  subset(opioid_type %in% c("Weak opioid", "Strong opioid", "Any opioid"))

write.csv(opioid_rx_knee, file = here::here("output", "clockstops", "opioid_by_week_knee.csv"),
          row.names = FALSE)

#####
#test




