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

age_stats <- ortho_final %>%
  subset(routine == "Routine" & admitted == TRUE) %>%
  summarise(p25 = quantile(age, .25, na.rm=TRUE),
                p50 = quantile(age, .5, na.rm=TRUE),
                p75 = quantile(age, .75, na.rm=TRUE),
                mean = mean(age)) %>%
      mutate(variable = "Age summary statistics", 
             prior_opioid_rx = NA,
             source = "clockstops",
             cohort = "Orthopaedic - Routine/Admitted")

age_stats_prior <- ortho_final %>%
  subset(routine == "Routine" & admitted == TRUE) %>%
  group_by(prior_opioid_rx) %>%
  summarise(p25 = quantile(age, .25, na.rm=TRUE),
               p50 = quantile(age, .5, na.rm=TRUE),
               p75 = quantile(age, .75, na.rm=TRUE),
               mean = mean(age)) %>%
  mutate(variable = "Age summary statistics",
         source = "clockstops",
         cohort = "Orthopaedic - Routine/Admitted")

age_stats_combined <- rbind(age_stats, age_stats_prior)

write.csv(age_stats_combined, here::here("output", "clockstops", "age_stats.csv"),
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
        cohort = "Orthopaedic"
      ) %>%
      rename(category = {{variable}}) %>%
      mutate(category = as.character(category))
    
  }
  
  rbind(
    cat_dist(priority_type, "Priority type"),
    cat_dist(censor_before_rtt_end, "Censor before WL end"),
    cat_dist(censor_before_study_end, "Censor before study end"),
    cat_dist(rtt_multiple, "Multiple RTT pathways"),
    cat_dist(covid_timing, "Start date timing"),
    cat_dist(died_during_post, "Died during post-WL follow-up"),

    cat_dist(age_group, "Age group"),
    cat_dist(sex, "Sex"),
    cat_dist(imd10, "IMD decile"),
    cat_dist(ethnicity6, "Ethnicity (6 groups)"),
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
    cat_dist(ra, "Rheumatoid arthritis")
  ) 
  
}


####### Stratified by urgent/routine and admitted/not-admitted (for supp) #########

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
cat_dist <- list(urgent_admit, routine_admit, urgent_notadmit, routine_notadmit) %>% 
  reduce(full_join, by=c("category","var","cohort","source")) %>%
  filter(category != FALSE) %>%
  mutate(
          # Un-redact true zeroes
          count_routine_admitted =
           ifelse(var == "Priority type" & 
                    (category %in% c("Missing", "two week wait", "urgent")),
                                            0, count_routine_admitted),
         
         count_routine_notadmitted = 
           ifelse(var == "Priority type" &
                    (category %in% c("Missing", "two week wait", "urgent")),
                                            0, count_routine_notadmitted),
         
         total_routine_admitted = 
           ifelse(var == "Priority type" & 
                    (category %in% c("Missing", "two week wait", "urgent")),
                                            0, total_routine_admitted),
         
         total_routine_notadmitted = 
           ifelse(var == "Priority type" & 
                    (category %in% c("Missing", "two week wait", "urgent")),
                                            0, total_routine_notadmitted),
         
         count_urgent_admitted =
           ifelse(var == "Priority type" & 
                    (category %in% c("Missing", "routine")), 
                                            0, count_urgent_admitted),
         
         count_urgent_notadmitted = 
           ifelse(var == "Priority type" & 
                    (category %in% c("Missing", "routine")), 0, count_urgent_notadmitted),
         
         total_urgent_admitted = 
           ifelse(var == "Priority type" & 
                    (category %in% c("Missing", "routine")), 0, total_urgent_admitted),
         
         total_urgent_notadmitted = 
           ifelse(var == "Priority type" & 
                    (category %in% c("Missing", "routine")), 0, total_urgent_notadmitted)
         ) %>%
  arrange(var, category) 

cat_dist <- cat_dist[,c("source", "cohort", "var", "category", 
       "count_routine_admitted", "total_routine_admitted",
       "count_routine_notadmitted", "total_routine_notadmitted",
       "count_urgent_admitted", "total_urgent_admitted",
       "count_urgent_notadmitted", "total_urgent_notadmitted")]

write.csv(cat_dist, here::here("output", "clockstops",  "cat_var_dist_ortho.csv"),
          row.names = FALSE) 


#####################################################
############ By prior opioid rx #####################
#####################################################


############ Categorical variable relative frequency distributions #############

# Prior opioid rx only
dat <- ortho_final %>%
  subset(routine == "Routine" & admitted == TRUE & prior_opioid_rx == TRUE)

prior_yes <- cat_dist_combined() %>%
  rename(count_prior_opioid = count, total_prior_opioid = total)

# No prior opioid rx only
dat <- ortho_final %>%
  subset(routine == "Routine" & admitted == TRUE & prior_opioid_rx == FALSE)

prior_no <- cat_dist_combined() %>%
  rename(count_opioid_naive = count, total_opioid_naive = total)


# Merge 
cat_dist <- list(prior_yes, prior_no) %>% 
  reduce(full_join, by=c("category","var","cohort","source")) %>%
  filter(category != FALSE) %>%
  arrange(var, category) 

cat_dist <- cat_dist[,c("source", "cohort", "var", "category", 
                        "count_prior_opioid", "total_prior_opioid", 
                        "count_opioid_naive", "total_opioid_naive")]

write.csv(cat_dist, here::here("output", "clockstops",  "cat_var_dist_ortho_prior.csv"),
          row.names = FALSE) 

