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
library('rlang')

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
                                         rtt_end_month =  col_date(format="%Y-%m-%d"))) 


################## Medicine Rx count variables - overall and stratified ####################

# Count total number of Rx, total person-days
#   for each period and each medicine group
summ_combined <- function(gp, var){
  
  summ_gp <- function(gp, med) {
    
    pre_count <- rlang::sym(paste0(med, "_pre_count"))
    wait_count <- rlang::sym(paste0(med, "_wait_count"))
    post_count <- rlang::sym(paste0(med, "_post_count"))
    
    tmp <- ortho_final %>%
      mutate(full = "Full cohort") %>%
      subset(routin != "Missing")%>%
      group_by(routine, admitted, {{gp}}) %>%
      summarise(pre_person_days = rounding(sum(pre_time)),
                wait_person_days = rounding(sum(wait_time_adj)),
                post_person_days = rounding(sum(post_time_adj)),
                pre_total_rx = rounding(sum(!!pre_count)),
                wait_total_rx = rounding(sum(!!wait_count)),
                post_total_rx = rounding(sum(!!post_count))
                
      ) %>%
      mutate(med_group = med) 
    
    return(tmp)
    
  }
  
  rbind(
    summ_gp({{gp}}, "opioid"),
    summ_gp({{gp}}, "strong_opioid"),
    summ_gp({{gp}}, "lo_opioid"),
    summ_gp({{gp}}, "med_opioid"),
    summ_gp({{gp}}, "hi_opioid"),
    summ_gp({{gp}}, "weak_opioid"),
    summ_gp({{gp}}, "long_opioid"),
    summ_gp({{gp}}, "short_opioid"),
    summ_gp({{gp}}, "gabapentinoid"),
    summ_gp({{gp}}, "nsaid"),
    summ_gp({{gp}}, "antidepressant")
  ) %>%
    mutate(source = "clockstops", cohort = "ortho", variable = var,
           category = as.character({{gp}}))
  
}

prescribing_group <- rbind(
  summ_combined(full, "Full cohort"),
  summ_combined(age_group, "Age"),
  summ_combined(imd10, "IMD decile"),
  summ_combined(sex, "Sex"),
  summ_combined(region, "Region")) %>%
  arrange(source, cohort, routine, variable, category, med_group) %>%
  subset(!is.na(region) & !(imd10 == "Unknown"))

prescribing_group <- prescribing_group[,c("source", "cohort", "variable", 
                                          "category", "routine", "admitted","med_group", 
                                          "pre_total_rx", "pre_person_days", 
                                          "wait_total_rx", "wait_person_days", 
                                          "post_total_rx", "post_person_days")]

write.csv(prescribing_group, here::here("output", "clockstops", "med_by_period_ortho.csv"),
          row.names = FALSE)

