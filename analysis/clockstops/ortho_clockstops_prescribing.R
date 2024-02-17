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
                                                "Any opioid"))))))))),
         med_any = ifelse(value >= 1, 1, 0),
         med_3plus = ifelse(value >=3, 1, 0))

person_time <- ortho_final %>%
    subset(measure == "Person time") %>%
    dplyr::select(c(patient_id, period, value)) %>%
    rename(person_time = value)

ortho_final <- merge(subset(ortho_final, measure != "Person time"), person_time, 
                     by = c("patient_id", "period"), all.x = TRUE) 


##################### Medicine prescribing counts ######################


# Frequency distribution of categorical variables
cat_dist_meds <- function() {
  cat_dist <- function(variable, name) {
    
    dat %>%
      subset(routine == "Routine") %>%
      group_by({{variable}}, measure, period, routine, admitted) %>%
      summarise(count_any = sum(med_any),
                count_3plus = sum(med_3plus),
                total = n()) %>%
      mutate(
        var = name, total = rounding(total),
        count_any = rounding(count_any),
        count_3plus = rounding(count_3plus)
      ) %>%
      ungroup() %>%
      mutate(
        total = rounding(total),
        source = "clockstops", 
        cohort = "ortho"
      ) %>%
      rename(category = {{variable}}) %>%
      mutate(category = as.character(category))
    
  }
  
  rbind(
    cat_dist(age_group, "Age"),
    cat_dist(imd10, "IMD"),
    cat_dist(ethnicity6, "Ethnicity"),
    cat_dist(region, "Region"),
    cat_dist(sex, "Sex")
  ) 
  
}

# Overall
dat <- ortho_final

meds <- cat_dist_meds() 

meds <- meds[,c("source", "cohort", "routine", "admitted", 
                        "var", "category",  "period",
                        "measure", "count_any", "count_3plus", "total") ]

write.csv(meds, here::here("output", "clockstops",  "meds_dist_ortho.csv"),
          row.names = FALSE) 


################## Medicine Rx count variables - overall and stratified ####################

# Count total number of Rx, total person-days
#   for each period and each medicine group
summ <- function(gp, var){

    ortho_final %>%
      mutate(full = "Full cohort") %>%
      subset(routine == "Routine" & period != "Pre-WL (1 year)") %>%
      group_by(routine, admitted, period, measure, {{gp}}) %>%
      summarise(person_days = rounding(sum(person_time)),
                count_rx = rounding(sum(value))) %>%
      rename(category = {{gp}}) %>%
    mutate(variable = var, source = "clockstops", cohort = "ortho") 
    
  
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
                                          "category","period","person_days", "measure", "count_rx"
                                          )]

write.csv(prescribing_group, here::here("output", "clockstops", "med_by_period_ortho.csv"),
          row.names = FALSE)

