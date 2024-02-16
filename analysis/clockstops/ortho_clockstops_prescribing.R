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
ortho_final <- read_csv(here::here("output", "data", "cohort_full_clockstops.csv.gz"),
                        col_types = cols(rtt_start_date = col_date(format="%Y-%m-%d"),
                                         rtt_end_date = col_date(format="%Y-%m-%d"),
                                         reg_end_date = col_date(format="%Y-%m-%d"),
                                         dod = col_date(format="%Y-%m-%d"),
                                         end_date = col_date(format="%Y-%m-%d"),
                                         rtt_start_month =  col_date(format="%Y-%m-%d"),
                                         rtt_end_month =  col_date(format="%Y-%m-%d"))) %>%
  dplyr::select(c(patient_id, ends_with(c("_count")), post_time_adj, wait_time_adj, pre_time,
                                        routine, admitted, age_group, sex, imd10, ethnicity6, region)) %>%
  reshape2::melt(id = c("patient_id","age_group","sex","imd10","ethnicity6","region",
                        "routine", "admitted")) %>%
  mutate(period = ifelse(grepl("pre_", variable), "Pre-WL",
                         ifelse(grepl("wait_", variable), "During WL", 
                                ifelse(grepl("post_", variable), "Post WL",
                                       ifelse(grepl("1yr_", variable), "Pre-WL (1 year)",
                                          "Missing")))),
         measure = ifelse(grepl("time", variable), "Person time",
                          ifelse(grepl("short_opioid", variable), "Short-acting opioid",
                            ifelse(grepl("long_opioid", variable), "Long-acting opioid",
                              ifelse(grepl("weak_opioid", variable), "Weak opioid",
                                ifelse(grepl("strong_opioid", variable), "Strong opioid",
                                    ifelse(grepl("gabapentinoid", variable), "Gabapentinoid",
                                       ifelse(grepl("antidepressant", variable), "Antidepressant",
                                          ifelse(grepl("nsaid", variable), "NSAID",
                                             ifelse(grepl("tca", variable), "TCA",
                                                "Any opioid"))))))))))

person_time <- ortho_final %>%
    subset(measure == "Person time") %>%
    dplyr::select(c(patient_id, period, value)) %>%
    rename(person_time = value)

ortho_final <- merge(subset(ortho_final, measure != "Person time"), person_time, 
                     by = c("patient_id", "period")) 


################## Medicine Rx count variables - overall and stratified ####################

# Count total number of Rx, total person-days
#   for each period and each medicine group
summ <- function(gp, var){

    tmp <- ortho_final %>%
      mutate(full = "Full cohort") %>%
      subset(routine != "Missing" & value >= 1) %>%
      group_by(routine, admitted, period, measure, {{gp}}) %>%
      summarise(person_days = rounding(sum(person_time)),
                n_any = rounding(n())
      ) %>%
      rename(category = {{gp}}) %>%
      mutate(variable = var)
    
    tmp2 <- ortho_final %>%
      mutate(full = "Full cohort") %>%
      subset(routine != "Missing" & value >= 3) %>%
      group_by(routine, admitted, period, measure, {{gp}}) %>%
      summarise(n_3plus = rounding(n())) %>%
      rename(category = {{gp}}) %>%
    mutate(variable = var)
    
    tmp3 <- merge(tmp, tmp2, by = c("routine", "admitted", "variable", "category", "period", "measure")) %>%
        mutate(source = "clockstops", cohort = "ortho") 
    
    return(tmp3)
  
}

prescribing_group <- rbind(
    summ(full, "Full cohort"),
    summ(age_group, "Age"),
    summ(imd10, "IMD decile"),
    summ(ethnicity6, "Ethnicity"),
    summ(region, "Region"),
    summ(sex, "Sex")) %>%
  arrange(source, cohort, routine, variable, category, period, measure) 

prescribing_group <- prescribing_group[,c("source", "cohort", "routine", "admitted", "variable", 
                                          "category","person_days", "measure",  
                                          "period", "n_any", "n_3plus")]

write.csv(prescribing_group, here::here("output", "clockstops", "med_by_period_ortho.csv"),
          row.names = FALSE)

