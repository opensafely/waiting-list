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


## Load data - orthopaedic routine only ##
ortho_routine_final <- read_csv(here::here("output", "data", "cohort_ortho_routine_clockstops.csv.gz"),
                        col_types = cols(rtt_start_date = col_date(format="%Y-%m-%d"),
                                         rtt_end_date = col_date(format="%Y-%m-%d"),
                                         reg_end_date = col_date(format="%Y-%m-%d"),
                                         dod = col_date(format="%Y-%m-%d"),
                                         end_date = col_date(format="%Y-%m-%d"),
                                         rtt_start_month =  col_date(format="%Y-%m-%d"),
                                         rtt_end_month =  col_date(format="%Y-%m-%d"))) %>%
  mutate(gaba_any = (gabapentinoid_pre_count >= 1),
         gaba_3plus = (gabapentinoid_pre_count >= 3),
         nsaid_any = (nsaid_pre_count >= 1),
         nsaid_3plus = (nsaid_pre_count >= 3),
         tca_any = (tca_pre_count >= 1),
         tca_3plus = (tca_pre_count >= 3),
         ad_any = (antidepressant_pre_count >= 1),
         ad_3plus = (antidepressant_pre_count >= 3))
         


############ Mean/median age ###############

quantile <- scales::percent(c(.25,.5,.75))

age_stats <- ortho_routine_final %>%
  summarise(p25 = quantile(age, .25, na.rm=TRUE),
                p50 = quantile(age, .5, na.rm=TRUE),
                p75 = quantile(age, .75, na.rm=TRUE)) %>%
      mutate(variable = "Age summary statistics", 
             prior_opioid_rx = "Full cohort", 
             cohort = "Orthopaedic - Routine/Admitted")

age_stats_prior <- ortho_routine_final %>%
  group_by(prior_opioid_rx) %>%
  summarise(p25 = quantile(age, .25, na.rm=TRUE),
               p50 = quantile(age, .5, na.rm=TRUE),
               p75 = quantile(age, .75, na.rm=TRUE)) %>%
  mutate(variable = "Age summary statistics",
         cohort = "Orthopaedic - Routine/Admitted")

age_stats_combined <- rbind(age_stats, age_stats_prior) 

write.csv(age_stats_combined, here::here("output", "clockstops", "age_stats.csv"),
          row.names = FALSE) 



########### Categorical variable relative frequency distributions #############
############ By prior opioid rx ###############################################


# Prior opioid rx only
dat <- ortho_routine_final %>%
  subset(prior_opioid_rx == TRUE)

prior_yes <- cat_dist_combined() %>%
  rbind(cat_dist(hip_hrg, "Hip procedure"),
        cat_dist(knee_hrg, "Knee procedure"),
        cat_dist(shoulder_hrg, "Shoulder procedure"),
        cat_dist(elbow_hrg, "Elbow procedure"),
        cat_dist(hand_hrg, "Hand procedure"),
        cat_dist(foot_hrg, "Foot procedure"),
        cat_dist(complex_hrg, "Complex procedure"),
        cat_dist(any_nontrauma_hrg, "Any procedure (non-trauma)"),
        cat_dist(any_admission, "Any admission"),
        cat_dist(pain_hrg, "Pain management"),
        cat_dist(trauma_hrg, "Trauma procedure"),
        
        cat_dist(gaba_any, "Gabapentinoids (any)"),
        cat_dist(gaba_3plus, "Gabapentinoids (>=3)"),
        cat_dist(nsaid_any, "NSAID (any)"),
        cat_dist(nsaid_3plus, "NSAID (>=3)"),
        cat_dist(tca_any, "TCA (any)"),
        cat_dist(tca_3plus, "TCA (>=3)"),
        cat_dist(ad_any, "Antidepressant (any)"),
        cat_dist(ad_3plus, "Antidepressant (>=3)")) %>%
  rename(count_prior_opioid = count, total_prior_opioid = total)

# No prior opioid rx only
dat <- ortho_routine_final %>%
  subset(prior_opioid_rx == FALSE)

prior_no <- cat_dist_combined() %>%
  rbind(cat_dist(hip_hrg, "Hip procedure"),
        cat_dist(knee_hrg, "Knee procedure"),
        cat_dist(shoulder_hrg, "Shoulder procedure"),
        cat_dist(elbow_hrg, "Elbow procedure"),
        cat_dist(hand_hrg, "Hand procedure"),
        cat_dist(foot_hrg, "Foot procedure"),
        cat_dist(complex_hrg, "Complex procedure"),
        cat_dist(any_nontrauma_hrg, "Any procedure (non-trauma)"),
        cat_dist(any_admission, "Any admission"),
        cat_dist(pain_hrg, "Pain management"),
        cat_dist(trauma_hrg, "Trauma procedure"),
        
        cat_dist(gaba_any, "Gabapentinoids (any)"),
        cat_dist(gaba_3plus, "Gabapentinoids (>=3)"),
        cat_dist(nsaid_any, "NSAID (any)"),
        cat_dist(nsaid_3plus, "NSAID (>=3)"),
        cat_dist(tca_any, "TCA (any)"),
        cat_dist(tca_3plus, "TCA (>=3)"),
        cat_dist(ad_any, "Antidepressant (any)"),
        cat_dist(ad_3plus, "Antidepressant (>=3)")) %>%
  rename(count_opioid_naive = count, total_opioid_naive = total)


# Merge 
cat_dist <- list(prior_yes, prior_no) %>% 
  reduce(full_join, by=c("category","var","cohort")) %>%
  filter(category != FALSE) %>%
  arrange(var, category) 

cat_dist <- cat_dist[,c("cohort", "var", "category", 
                        "count_prior_opioid", "total_prior_opioid", 
                        "count_opioid_naive", "total_opioid_naive")]

write.csv(cat_dist, here::here("output", "clockstops",  "cat_var_dist_prior.csv"),
          row.names = FALSE) 


#################

