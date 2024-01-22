###############################################################
# This script creates summary statistics for all key variables
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

## Rounding function
source(here("analysis", "custom_functions.R"))

## Create directories if needed
dir_create(here::here("output", "clockstops"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "data"), showWarnings = FALSE, recurse = TRUE)


## Load data ##
ortho_final <- read_csv(here::here("output", "data", "cohort_ortho_clockstops.csv.gz"),
  col_types = cols(rtt_start_date = col_date(format="%Y-%m-%d"),
                    rtt_end_date = col_date(format="%Y-%m-%d"),
                    reg_end_date = col_date(format="%Y-%m-%d"),
                    dod = col_date(format="%Y-%m-%d"),
                    end_date = col_date(format="%Y-%m-%d"),
                    rtt_start_month =  col_date(format="%Y-%m-%d"),
                    rtt_end_month =  col_date(format="%Y-%m-%d"))) %>%
  mutate(admit_gp = ifelse(admitted == TRUE, "Admitted", "Not admitted")) 
  


############# Plot start/end dates by month #################

rtt_month <- ortho_final %>%
  mutate(month = pmax(rtt_start_month, as.Date("2020-01-01")),
         type = "RTT start date") %>%
  group_by(month, type) %>%
  summarise(count = n()) %>%
  bind_rows(
    ortho_final %>%
      rename(month = rtt_end_month) %>%
      mutate(type = "RTT end date") %>%
      group_by(month, type) %>%
      summarise(count = n())
  ) %>%
  mutate(count = rounding(count), source = "clockstops", cohort = "ortho") 

write.csv(rtt_month, here::here("output", "clockstops", "rtt_dates_ortho.csv"),
          row.names = FALSE)


############### Waiting time distribution #################


quantile <- scales::percent(c(.1,.25,.5,.75,.9,.95,.99))

# Percentiles
wait_pcent_overall <- ortho_final %>% 
  summarise(p10 = quantile(wait_time, .1, na.rm=TRUE),
            p25 = quantile(wait_time, .25, na.rm=TRUE),
            p50 = quantile(wait_time, .5, na.rm=TRUE),
            p75 = quantile(wait_time, .75, na.rm=TRUE),
            p90 = quantile(wait_time, .9, na.rm=TRUE)) %>%
  mutate(source = "clockstops", cohort = "ortho", routine = "All", admit_gp = "All") 

wait_pcent_stratified <- ortho_final %>% 
  group_by(routine, admit_gp) %>%
  summarise(p10 = quantile(wait_time, .1, na.rm=TRUE),
            p25 = quantile(wait_time, .25, na.rm=TRUE),
            p50 = quantile(wait_time, .5, na.rm=TRUE),
            p75 = quantile(wait_time, .75, na.rm=TRUE),
            p90 = quantile(wait_time, .9, na.rm=TRUE)) %>%
  mutate(source = "clockstops", cohort = "ortho") %>%
  subset(routine != "Missing" & !is.na(routine))

wait_pcent <- rbind(wait_pcent_overall, wait_pcent_stratified)

wait_pcent <- wait_pcent[,c("source", "cohort", "routine", "admit_gp", 
                            "p10", "p25", "p50", "p75", "p90")]

write.csv(wait_pcent, here::here("output", "clockstops", "wait_time_pcent_ortho.csv"),
          row.names = FALSE)

# By week
wait_time <- ortho_final %>%
  group_by(week52) %>%
  mutate(total = n()) %>%
  group_by(week52, admitted, routine, total) %>%
  summarise(count = n()) %>%
  mutate(count = rounding(count),
         total = rounding(total),
         source = "clockstops", cohort = "ortho") %>%
  subset(routine != "Missing" & !is.na(routine)) %>%
  arrange(source, cohort, routine, admitted, week52, total)

wait_time <- wait_time[,c("source", "cohort", "routine", "admitted", 
                          "week52", "count",  "total")]

write.csv(wait_time, file = here::here("output", "clockstops", "wait_time_ortho.csv"),
          row.names = FALSE)

# Stratified by demographics
wait_gp <- function(gp, name){
  
  ortho_final %>%
    group_by({{gp}}, admitted, routine) %>%
    mutate(total = n(),
           p25 = quantile(wait_time, .25, na.rm=TRUE),
           p50 = quantile(wait_time, .5, na.rm=TRUE),
           p75 = quantile(wait_time, .75, na.rm=TRUE)) %>%
    group_by({{gp}}, routine, admitted, week_gp, total, p25, p50, p75) %>%
    summarise(count = n()) %>%
    mutate(count = rounding(count),
           total = rounding(total),
           var = name,
           cohort = "ortho", 
           source = "clockstops") %>%
    rename(category = {{gp}}) %>%
    ungroup() %>%
    subset(routine != "Missing" & !is.na(routine))
  
}

wait_by_group <- rbind(
  wait_gp(age_group, "Age group"),
  wait_gp(sex, "Sex"),
  wait_gp(imd10, "IMD decile"),
  wait_gp(region, "Region")
  ) %>% 
  arrange(var, category, routine, week_gp) %>%
  subset(!(is.na(category)  | (var == "IMD decile" & category == "Unknown")))

wait_by_group <- wait_by_group[,c("source", "cohort", "var", "category",
                                  "admitted",
                                  "routine", "week_gp", "count", "total",
                                  "p25", "p50", "p75")]

write.csv(wait_by_group, here::here("output", "clockstops", "wait_by_group_ortho.csv"),
  row.names = FALSE) 



