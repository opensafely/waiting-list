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
                                         rtt_end_month =  col_date(format="%Y-%m-%d"))) 


############ Mean/median age ###############

quantile <- scales::percent(c(.25,.5,.75))

age_stats <- full_final %>%
      summarise_at(vars(age), list(p25 = ~quantile(., .25, na.rm=TRUE),
                             p50 = ~quantile(., .5, na.rm=TRUE),
                             p75 = ~quantile(., .75, na.rm=TRUE),
                             mean = ~mean(.))) %>%
      mutate(variable = "Age summary statistics")
    

write.csv(age_stats, here::here("output", "clockstops", "age_stats.csv"),
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
      rename(category = {{variable}}) %>%
      mutate(category = as.character(category))
  }
  
  rbind(
    cat_dist(treatment_function, "Treatment function"),
    cat_dist(priority_type, "Priority type"),
    cat_dist(censor_before_rtt_end, "Censor before WL end"),
    cat_dist(censor_before_study_end, "Censor before study end"),
    cat_dist(rtt_multiple, "Multiple RTT pathways"),
    cat_dist(covid_timing, "Start date timing"),
    
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
    
    cat_dist(opioid_pre_any, "Any opioid (pre-WL)"),
    cat_dist(opioid_wait_any, "Any opioid (during WL)"),
    cat_dist(opioid_post_any, "Any opioid (post-WL)"),
    
    cat_dist(long_opioid_pre_any, "Long acting opioid (pre-WL)"),
    cat_dist(long_opioid_wait_any, "Long acting opioid (during WL)"),
    cat_dist(long_opioid_post_any, "Long acting opioid (post-WL)"),
    
    cat_dist(short_opioid_pre_any, "Short acting opioid (pre-WL)"),
    cat_dist(short_opioid_wait_any, "Short acting opioid (during WL)"),
    cat_dist(short_opioid_post_any, "Short acting opioid (post-WL)"),
    
    cat_dist(weak_opioid_pre_any, "Weak opioid (pre-WL)"),
    cat_dist(weak_opioid_wait_any, "Weak dose opioid (during WL)"),
    cat_dist(weak_opioid_post_any, "Weak dose opioid (post-WL)"),
    
    cat_dist(strong_opioid_pre_any, "Strong dose opioid (pre-WL)"),
    cat_dist(strong_opioid_wait_any, "Strong dose opioid (during WL)"),
    cat_dist(strong_opioid_post_any, "Strong dose opioid (post-WL)"),
    
    cat_dist(gabapentinoid_pre_any, "Gabapentinoid (pre-WL)"),
    cat_dist(gabapentinoid_wait_any, "Gabapentinoid (during WL)"),
    cat_dist(gabapentinoid_post_any, "Gabapentinoid (post-WL)"),
    
    cat_dist(nsaid_pre_any, "NSAID (pre-WL)"),
    cat_dist(nsaid_wait_any, "NSAID (during WL)"),
    cat_dist(nsaid_post_any, "NSAID (post-WL)"),
    
    cat_dist(antidepressant_pre_any, "Antidepressant (pre-WL)"),
    cat_dist(antidepressant_wait_any, "Antidepressant (during WL)"),
    cat_dist(antidepressant_post_any, "Antidepressant (post-WL)")
  ) 
  
}

# Overall
dat <- ortho_final

overall <- cat_dist_combined() 

# Urgent / admitted only
dat <- ortho_final %>%
  subset(routine == "Urgent" & admitted == TRUE)

urgent_admit <- cat_dist_combined() %>%
  rename(count_urgent_admitted = count, total_urgent_admitted = total)

# Routine / admitted only
dat <- ortho_final %>%
  subset(routine == "Routine" & admitted == TRUE)
routine_admit <- cat_dist_combined() %>%
  rename(count_routine_admitted = count, total_routine_admitted = total)


# Urgent / not admitted only
dat <- ortho_final %>%
  subset(routine == "Urgent" & admitted == FALSE)

urgent_notadmit <- cat_dist_combined() %>%
  rename(count_urgent_notadmitted = count, total_urgent_notadmitted = total)

# Routine / not admitted only
dat <- ortho_final %>%
  subset(routine == "Routine" & admitted == FALSE)

routine_notadmit <- cat_dist_combined() %>%
  rename(count_routine_notadmitted = count, total_routine_notadmitted = total)


# Merge 
cat_dist <- list(overall, urgent_admit, routine_admit, urgent_notadmit, routine_notadmit) %>% 
  reduce(full_join, by=c("category","var","cohort","source")) %>%
  filter(category != FALSE) %>%
  mutate(count_routine_admitted =
           ifelse(var == "Priority type" & (category %in% c("Missing", "two week wait", "urgent")),
                                            0, count_routine_admitted),
         
         count_routine_notadmitted = 
           ifelse(var == "Priority type" & (category %in% c("Missing", "two week wait", "urgent")),
                                            0, count_routine_notadmitted),
         
         total_routine_admitted = 
           ifelse(var == "Priority type" & (category %in% c("Missing", "two week wait", "urgent")),
                  0, total_routine_admitted),
         
         total_routine_notadmitted = 
           ifelse(var == "Priority type" & (category %in% c("Missing", "two week wait", "urgent")),
                  0, total_routine_notadmitted),
         
         count_urgent_admitted =
           ifelse(var == "Priority type" & (category %in% c("Missing", "routine")), 0, count_urgent_admitted),
         
         count_urgent_notadmitted = 
           ifelse(var == "Priority type" & (category %in% c("Missing", "routine")), 0, count_urgent_notadmitted),
         
         total_urgent_admitted = 
           ifelse(var == "Priority type" & (category %in% c("Missing", "routine")), 0, total_urgent_admitted),
         
         total_urgent_notadmitted = 
           ifelse(var == "Priority type" & (category %in% c("Missing", "routine")), 0, total_urgent_notadmitted)
         ) %>%
  arrange(var, category) 

cat_dist <- cat_dist[,c("source", "cohort", "var", "category", "count", "total",
       "count_routine_admitted", "total_routine_admitted",
       "count_routine_notadmitted", "total_routine_notadmitted",
       "count_urgent_admitted", "total_urgent_admitted",
       "count_urgent_notadmitted", "total_urgent_notadmitted")]

write.csv(cat_dist, here::here("output", "clockstops",  "cat_var_dist_ortho.csv"),
          row.names = FALSE) 
