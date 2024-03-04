###############################################################
# This script creates summary statistics for all key variables
# for all people with a closed RTT pathway (May21-Apr22)
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
dir_create(here::here("output", "data"), showWarnings = FALSE, recurse = TRUE)


## Load data ##
ortho_routine_final <- read_csv(here::here("output", "data", "cohort_ortho_routine_clockstops.csv.gz"),
  col_types = cols(rtt_start_date = col_date(format="%Y-%m-%d"),
                    rtt_end_date = col_date(format="%Y-%m-%d"),
                    reg_end_date = col_date(format="%Y-%m-%d"),
                    dod = col_date(format="%Y-%m-%d"),
                    end_date = col_date(format="%Y-%m-%d"),
                    rtt_start_month =  col_date(format="%Y-%m-%d"),
                    rtt_end_month =  col_date(format="%Y-%m-%d"))) 
  

############# Plot start/end dates by month #################

rtt_month <- ortho_routine_final %>%
  mutate(month = pmax(rtt_start_month, as.Date("2020-01-01")),
         type = "RTT start date") %>%
  group_by(month, type) %>%
  summarise(count = n()) %>%
  bind_rows(
    ortho_routine_final %>%
      rename(month = rtt_end_month) %>%
      mutate(type = "RTT end date") %>%
      group_by(month, type) %>%
      summarise(count = n())
  ) %>%
  mutate(count = rounding(count), cohort = "Orthopaedic - Routine/Admitted") 

write.csv(rtt_month, here::here("output", "clockstops", "rtt_dates_ortho.csv"),
          row.names = FALSE)


############### Waiting time distribution #################

quantile <- scales::percent(c(.1,.25,.5,.75,.9,.95,.99))

# Percentiles
wait_pcent <- ortho_routine_final %>% 
  summarise(p10 = quantile(wait_time, .1, na.rm=TRUE),
            p25 = quantile(wait_time, .25, na.rm=TRUE),
            p50 = quantile(wait_time, .5, na.rm=TRUE),
            p75 = quantile(wait_time, .75, na.rm=TRUE),
            p90 = quantile(wait_time, .9, na.rm=TRUE)) %>%
  mutate(cohort = "Orthopaedic - Routine/Admitted",
         prior_opioid_rx = NA)


wait_pcent_prior <- ortho_routine_final %>% 
  group_by(prior_opioid_rx) %>%
  summarise(p10 = quantile(wait_time, .1, na.rm=TRUE),
            p25 = quantile(wait_time, .25, na.rm=TRUE),
            p50 = quantile(wait_time, .5, na.rm=TRUE),
            p75 = quantile(wait_time, .75, na.rm=TRUE),
            p90 = quantile(wait_time, .9, na.rm=TRUE)) %>%
  mutate(cohort = "Orthopaedic - Routine/Admitted")

wait_pcent_combined <- rbind(wait_pcent, wait_pcent_prior)

wait_pcent_combined <- wait_pcent_combined[,c("cohort", "prior_opioid_rx", "p10", "p25", "p50", "p75", "p90")]

write.csv(wait_pcent_combined, here::here("output", "clockstops", "wait_time_pcent_ortho.csv"),
          row.names = FALSE)

# By week
wait_time <- ortho_routine_final %>%
  mutate(total = n()) %>%
  group_by(week52, total) %>%
  summarise(count = n()) %>%
  mutate(count = rounding(count),
         total = rounding(total),
         prior_opioid_rx = "Full cohort",
         cohort = "Orthopaedic - Routine/Admitted") %>%
  arrange(cohort, week52, total)

# By week and prior opioid Rx
wait_time_prior <- ortho_routine_final %>%
  group_by(prior_opioid_rx) %>%
  mutate(total = n()) %>%
  group_by(week52, prior_opioid_rx, total) %>%
  summarise(count = n()) %>%
  mutate(count = rounding(count),
         total = rounding(total),
         cohort = "Orthopaedic - Routine/Admitted",
         prior_opioid_rx = ifelse(prior_opioid_rx == TRUE, "Yes" , "No")) %>%
  arrange(cohort, week52, total)

wait_time_both <- rbind(wait_time, wait_time_prior)

wait_time_both <- wait_time_both[,c("cohort", "prior_opioid_rx", "week52", "count",  "total")]

write.csv(wait_time_both, file = here::here("output", "clockstops", "wait_time_ortho.csv"),
          row.names = FALSE)

# Stratified by demographics
wait_gp <- function(gp, name){

  ortho_routine_final %>%
    group_by({{gp}}) %>%
    mutate(total = n(),
           p25 = quantile(wait_time, .25, na.rm=TRUE),
           p50 = quantile(wait_time, .5, na.rm=TRUE),
           p75 = quantile(wait_time, .75, na.rm=TRUE)) %>%
    group_by({{gp}}, wait_gp, total, p25, p50, p75) %>%
    summarise(count = n()) %>%
    mutate(count = rounding(count),
           total = rounding(total),
           var = name,
           cohort = "Orthopaedic - Routine/Admitted",
           p25 = ifelse(total <= 32, NA, p25),
           p50 = ifelse(total <= 32, NA, p50),
           p75 = ifelse(total <= 32, NA, p75),
           name = ifelse(wait_gp == "<=18 weeks", 1,
                         ifelse(wait_gp == "19-52 weeks", 2, 3))) %>%
    rename(category = {{gp}}) %>%
    ungroup() %>%
    arrange(var, category, name) %>%
    pivot_wider(id_cols = c("cohort","var","category","total","p25","p50","p75"),
                          values_from = "count", 
                          names_prefix = "wait_gp")
  
}

wait_by_group <- rbind(
  wait_gp(age_group, "Age group"),
  wait_gp(sex, "Sex"),
  wait_gp(imd10, "IMD decile"),
  wait_gp(ethnicity6, "Ethnicity"),
  wait_gp(region, "Region"),
  wait_gp(prior_opioid_rx, "Prior opioid Rx")
  ) %>% 
  arrange(var, category) 

wait_by_group <- wait_by_group[,c( "cohort", "var", "category",
                                  "wait_gp1", "wait_gp2", "wait_gp3", "total",
                                  "p25", "p50", "p75")]

write.csv(wait_by_group, here::here("output", "clockstops", "wait_by_group_ortho.csv"),
  row.names = FALSE) 


