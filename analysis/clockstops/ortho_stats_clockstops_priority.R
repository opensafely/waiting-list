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
ortho <- read_csv(here::here("output", "data", "cohort_ortho_clockstops.csv.gz"),
  col_types = cols(rtt_start_date = col_date(format="%Y-%m-%d"),
                    rtt_end_date = col_date(format="%Y-%m-%d"),
                    reg_end_date = col_date(format="%Y-%m-%d"),
                    dod = col_date(format="%Y-%m-%d"),
                    end_date = col_date(format="%Y-%m-%d"),
                    rtt_start_month =  col_date(format="%Y-%m-%d"),
                    rtt_end_month =  col_date(format="%Y-%m-%d")))


###### Number of people excluded due to cancer 

exclude <- ortho %>%
  mutate(total = n()) %>%
  group_by(cancer, total) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(var = "Cancer",
    count = rounding(count),
    total = rounding(total),
    source = "clockstops", 
    cohort = "ortho"
  ) %>%
  rename(category = cancer)

exclude <- exclude[,c("source", "cohort", "var","category","count","total")]
 
write.csv(exclude, here::here("output", "clockstops", "exclude_ortho.csv"),
          row.names = FALSE)


##############

ortho_final <- ortho %>%
  subset(cancer == FALSE)


############## Plot start/end dates by month #################

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
wait_time_pcent <- ortho_final %>% 
  group_by(routine) %>%
  summarise(p10 = quantile(wait_time, .1, na.rm=TRUE),
            p25 = quantile(wait_time, .25, na.rm=TRUE),
            p50 = quantile(wait_time, .5, na.rm=TRUE),
            p75 = quantile(wait_time, .75, na.rm=TRUE),
            p90 = quantile(wait_time, .9, na.rm=TRUE),
            p95 = quantile(wait_time, .95, na.rm=TRUE),
            p99 = quantile(wait_time, .99, na.rm=TRUE)) %>%
  mutate(source = "clockstops", cohort = "ortho") %>%
  subset(routine != "Missing" & !is.na(routine))

write.csv(wait_time_pcent, here::here("output", "clockstops", "wait_time_pcent_ortho.csv"),
          row.names = FALSE)

# By week
wait_time <- ortho_final %>%
  mutate(total = n()) %>%
  group_by(week52, total, routine) %>%
  summarise(count = n()) %>%
  mutate(count = rounding(count),
         total = rounding(total),
         source = "clockstops", cohort = "ortho") %>%
  subset(routine != "Missing" & !is.na(routine))

write.csv(wait_time, file = here::here("output", "clockstops", "wait_time_ortho.csv"),
          row.names = FALSE)

# Stratified by demographics
wait_gp <- function(gp, name){
  
  ortho_final %>%
    group_by({{gp}}, routine) %>%
    mutate(total = n(),
           p25 = quantile(wait_time, .25, na.rm=TRUE),
           p50 = quantile(wait_time, .5, na.rm=TRUE),
           p75 = quantile(wait_time, .75, na.rm=TRUE)) %>%
    group_by({{gp}}, routine, week_gp, total, p25, p50, p75) %>%
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
  wait_gp(ethnicity6, "Ethnicity"),
  wait_gp(imd10, "IMD decile"),
  wait_gp(region, "Region")
) 

write.csv(wait_by_group, here::here("output", "clockstops", "wait_by_group_ortho.csv"),
  row.names = FALSE) 


############ Categorical variable relative frequency distributions #############

# Frequency distribution of categorical variables
cat_dist_combined <- function() {
  cat_dist <- function(variable, name) {
    
    dat %>%
      mutate(total = n()) %>%
      group_by({{variable}}, total) %>%
      summarise(count = n()) %>%
      mutate(
        var = name,
        count = rounding(count)
      ) %>%
      ungroup() %>%
      mutate(
        total = rounding(total),
        source = "clockstops", 
        cohort = "ortho"
      ) %>%
      rename(category = {{variable}}) 
  }
  
  rbind(
    cat_dist(treatment_function, "Treatment function"),
    cat_dist(priority_type, "Priority type"),
    cat_dist(censor_before_rtt_end, "Censor before WL end"),
    cat_dist(censor_before_study_end, "Censor before study end"),
    cat_dist(admitted, "Admitted"),
    cat_dist(rtt_multiple, "Multiple RTT pathways"),
    
    cat_dist(age_group, "Age group"),
    cat_dist(sex, "Sex"),
    cat_dist(imd10, "IMD decile"),
    cat_dist(ethnicity6, "Ethnicity (6 groups)"),
    cat_dist(ethnicity16, "Ethnicity (16 groups)"),
    cat_dist(region, "Region"),
    
    cat_dist(cancer, "Cancer"),
    cat_dist(diabetes, "Diabetes"),
    cat_dist(cardiac, "Cardiac"),
    cat_dist(copd, "COPD"),
    cat_dist(liver, "Liver"),
    cat_dist(ckd, "CKD"),
    cat_dist(oa, "Osteoarthritis"),
    cat_dist(depression, "Depression"),   
    cat_dist(anxiety, "Anxiety"),
    cat_dist(smi, "Severe mental illness"),
    cat_dist(oud, "Opioid use disorder"),
    cat_dist(ra, "Rheumatoid arthritis"),

    cat_dist(died_during_wl, "Died while on WL"),
    cat_dist(died_during_post, "Died during post-WL follow-up"),
    
    cat_dist(any_opioid_pre, "Any opioid (pre-WL)"),
    cat_dist(any_opioid_wait, "Any opioid (during WL)"),
    cat_dist(any_opioid_post, "Any opioid (post-WL)"),
    
    cat_dist(hi_opioid_pre, "High dose opioid (pre-WL)"),
    cat_dist(hi_opioid_wait, "High dose opioid (during WL)"),
    cat_dist(hi_opioid_post, "High dose opioid (post-WL)"),
    
    cat_dist(long_opioid_pre, "Long acting opioid (pre-WL)"),
    cat_dist(long_opioid_wait, "Long acting opioid (during WL)"),
    cat_dist(long_opioid_post, "Long acting opioid (post-WL)"),
    
    cat_dist(short_opioid_pre, "Short acting opioid (pre-WL)"),
    cat_dist(short_opioid_wait, "Short acting opioid (during WL)"),
    cat_dist(short_opioid_post, "Short acting opioid (post-WL)"),
    
    cat_dist(weak_opioid_pre, "Weak opioid (pre-WL)"),
    cat_dist(weak_opioid_wait, "Weak dose opioid (during WL)"),
    cat_dist(weak_opioid_post, "Weak dose opioid (post-WL)"),
    
    cat_dist(strong_opioid_pre, "Strong dose opioid (pre-WL)"),
    cat_dist(strong_opioid_wait, "Strong dose opioid (during WL)"),
    cat_dist(strong_opioid_post, "Strong dose opioid (post-WL)"),
    
    cat_dist(codeine_pre, "Codeine (pre-WL)"),
    cat_dist(codeine_wait, "Codeine (during WL)"),
    cat_dist(codeine_post, "Codeine (post-WL)"),
    
    cat_dist(tramadol_pre, "Tramadol (pre-WL)"),
    cat_dist(tramadol_wait, "Tramadol (during WL)"),
    cat_dist(tramadol_post, "Tramadol (post-WL)"),
    
    cat_dist(oxycodone_pre, "Oxycodone (pre-WL)"),
    cat_dist(oxycodone_wait, "Oxycodone (during WL)"),
    cat_dist(oxycodone_post, "Oxycodone (post-WL)"),
    
    cat_dist(gabapentinoid_pre, "Gabapentinoid (pre-WL)"),
    cat_dist(gabapentinoid_wait, "Gabapentinoid (during WL)"),
    cat_dist(gabapentinoid_post, "Gabapentinoid (post-WL)"),
    
    cat_dist(nsaid_pre, "NSAID (pre-WL)"),
    cat_dist(nsaid_wait, "NSAID (during WL)"),
    cat_dist(nsaid_post, "NSAID (post-WL)"),
    
    cat_dist(antidepressant_pre, "Antidepressant (pre-WL)"),
    cat_dist(antidepressant_wait, "Antidepressant (during WL)"),
    cat_dist(antidepressant_post, "Antidepressant (post-WL)")
  ) 
  
}

# Overall
dat <- ortho_final

overall <- cat_dist_combined() 

# Urgent only
dat <- ortho_final %>%
  subset(routine == "Urgent")

urgent <- cat_dist_combined() %>%
  rename(count_urgent = count, total_urgent = total)

# Routine only
dat <- ortho_final %>%
  subset(priority_type %in% c("routine"))

routine <- cat_dist_combined() %>%
  rename(count_routine = count, total_routine = total)


# Merge 
cat_dist <- list(overall, urgent, routine) %>% 
  reduce(full_join, by=c("category","var","cohort","source")) %>%
  arrange(var, category) 

cat_dist <- cat_dist[,c("source", "cohort", "var", "category", "count", "total",
       "count_routine", "total_routine", "count_urgent", "total_urgent")]

write.csv(cat_dist, here::here("output", "clockstops",  "cat_var_dist_ortho.csv"),
          row.names = FALSE) 


################## Medicine Rx count variables - overall and stratified ####################

# Count total number of Rx, total person-days
#   for each period and each medicine group
summ_combined <- function(gp, var){
  
  summ_gp <- function(gp, med) {
    ortho_final %>%
      mutate(tmp = "Full cohort") %>%
      group_by(routine, {{gp}}) %>%
      summarise(pre_person_days = rounding(sum(pre_time)),
                wait_person_days = rounding(sum(wait_time_adj)),
                post_person_days = rounding(sum(post_time_adj)),
                
                pre_total_rx = rounding(sum(across(intersect(starts_with(med), ends_with("_pre_count"))))),
                wait_total_rx = rounding(sum(across(intersect(starts_with(med), ends_with("_wait_count"))))),
                post_total_rx = rounding(sum(across(intersect(starts_with(med), ends_with("_post_count")))))) %>%
      ungroup() %>%
      mutate(med_group = med) 
  }
  
  rbind(
    summ_gp({{gp}}, "opioid"),
    summ_gp({{gp}}, "strong_opioid"),
    summ_gp({{gp}}, "high_opioid"),
    summ_gp({{gp}}, "weak_opioid"),
    summ_gp({{gp}}, "long_opioid"),
    summ_gp({{gp}}, "short_opioid"),
    summ_gp({{gp}}, "gabapentinoid"),
    summ_gp({{gp}}, "nsaid"),
    summ_gp({{gp}}, "antidepressant")
  ) %>%
    mutate(source = "clockstops", cohort = "ortho", variable = var) %>%
    rename(category = {{gp}})
  
}

prescribing_group <- rbind(
    summ_combined(tmp, "Full cohort"),
    summ_combined(age_group, "Age"),
    summ_combined(imd10, "IMD decile"),
    summ_combined(sex, "Sex"),
    summ_combined(ethnicity6, "Ethnicity"),
    summ_combined(admitted, "Admitted")) %>%
  arrange(source, cohort, routine, variable, category, med_group)

prescribing_group <- prescribing_group[,c("source", "cohort", "routine","variable", 
                            "category", "med_group", "pre_total_rx", "pre_person_days", 
                            "wait_total_rx", "wait_person_days", 
                            "post_total_rx", "post_person_days")]

write.csv(prescribing_group, here::here("output", "clockstops", "med_by_period_ortho.csv"),
          row.names = FALSE)

