###############################################################
# This script creates final cohorts for analysis 
# for: 1. all people with an open RTT pathway (May22)
# and 2. all people with an open RTT pathway for trauma/orthopaedic surgery
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
dir_create(here::here("output", "openpathways"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "data"), showWarnings = FALSE, recurse = TRUE)


## Load data ##
full <- read_csv(here::here("output", "data", "dataset_openpathways.csv.gz"),
  col_types = cols(rtt_start_date = col_date(format="%Y-%m-%d"),
                    reg_end_date = col_date(format="%Y-%m-%d"),
                    dod = col_date(format="%Y-%m-%d"),
                    end_date = col_date(format="%Y-%m-%d"))) %>%
                
                # Exclude if dod before RTT end date
                #subset(is.na(dod) | (!is.na(dod) & rtt_end_date > dod)) %>%
  
                # Create new variables                
                mutate(# Month of WL start/end
                    rtt_start_month = floor_date(rtt_start_date, "month"),
                    rtt_end_month = as.Date("2022-05-01"),
                       
                    # Were on multiple WL during study period
                    rtt_multiple = ifelse(count_rtt_start_date > 1, 1, 0),
                    
                    # Admitted
                    admitted = (waiting_list_type %in% c("IRTT","PTLI","RTTI")),

                    # Orthopaedic surgery
                    ortho_surgery = (treatment_function %in% c("110", "111")),
                    
                    # Died while on WL
                    died_during_wl = ifelse(!is.na(dod) & dod <= as.Date("2022-05-01"), 1, 0),
                    
                    week = ceiling(wait_time / 7),
                    week52 =  ifelse(week > 52, 52, week),
                    week_gp = ifelse(week <= 18, "<=18 weeks", 
                                     ifelse(week > 18 & week <= 52, "19-52 weeks", 
                                            "52+ weeks")))
                         

## Save as final
write.csv(full, file = here::here("output", "data", "cohort_full_openpathways.csv.gz"),
          row.names = FALSE)


####################################################


# Restrict to people with trauma/orthopaedic surgery
ortho <- full %>%
  subset(ortho_surgery == TRUE)

## Save as final
write.csv(ortho, file = here::here("output", "data", "cohort_ortho_openpathways.csv.gz"),
          row.names = FALSE)
