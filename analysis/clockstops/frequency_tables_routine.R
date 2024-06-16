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
library('readr')
library('purrr')

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
  mutate(gaba_any = (gabapentinoid_pre_count2 >= 1),
         gaba_3plus = (gabapentinoid_pre_count2 >= 3),
         nsaid_any = (nsaid_pre_count2 >= 1),
         nsaid_3plus = (nsaid_pre_count2 >= 3),
         tca_any = (tca_pre_count2 >= 1),
         tca_3plus = (tca_pre_count2 >= 3),
         ad_any = (antidepressant_pre_count2 >= 1),
         ad_3plus = (antidepressant_pre_count2 >= 3))
         


############ Mean/median age ###############

quantile <- scales::percent(c(.25,.5,.75))

age_stats <- ortho_routine_final %>%
  summarise(p25 = quantile(age, .25, na.rm=TRUE),
                p50 = quantile(age, .5, na.rm=TRUE),
                p75 = quantile(age, .75, na.rm=TRUE)) %>%
      mutate(variable = "Age summary statistics", 
             any_opioid = "Full cohort", 
             long_term_opioid = "Full cohort",
             cohort = "Orthopaedic - Routine/Admitted")

age_stats_prior_1 <- ortho_routine_final %>%
  group_by(any_opioid) %>%
  summarise(p25 = quantile(age, .25, na.rm=TRUE),
            p50 = quantile(age, .5, na.rm=TRUE),
            p75 = quantile(age, .75, na.rm=TRUE)) %>%
  mutate(variable = "Age summary statistics",
         long_term_opioid = " ",
         cohort = "Orthopaedic - Routine/Admitted")

age_stats_prior_2 <- ortho_routine_final %>%
  group_by(long_term_opioid) %>%
  summarise(p25 = quantile(age, .25, na.rm=TRUE),
            p50 = quantile(age, .5, na.rm=TRUE),
            p75 = quantile(age, .75, na.rm=TRUE)) %>%
  mutate(variable = "Age summary statistics",
         any_opioid = " ",
         cohort = "Orthopaedic - Routine/Admitted")


age_stats_combined <- rbind(age_stats, age_stats_prior_1, age_stats_prior_2) 

write.csv(age_stats_combined, here::here("output", "clockstops", "age_stats.csv"),
          row.names = FALSE) 



########### Table with procedure type ################################

dat <- ortho_routine_final 

procedures <- 
  rbind(cat_dist(hip_hrg, "Hip procedure"),
        cat_dist(knee_hrg, "Knee procedure"),
        cat_dist(shoulder_hrg, "Shoulder procedure"),
        cat_dist(elbow_hrg, "Elbow procedure"),
        cat_dist(hand_hrg, "Hand procedure"),
        cat_dist(foot_hrg, "Foot procedure"),
        cat_dist(complex_hrg, "Complex procedure"),
        cat_dist(any_nontrauma_hrg, "Any procedure (non-trauma)"),
        cat_dist(pain_hrg, "Pain management"),
        cat_dist(trauma_hrg, "Trauma procedure")) %>%
  subset(category == TRUE)

write.csv(procedures, here::here("output", "clockstops",  "procedures.csv"),
          row.names = FALSE) 


########### Categorical variable relative frequency distributions #############
############ By prior opioid rx ###############################################

# Prior opioid rx only
dat <- ortho_routine_final %>%
  subset(any_opioid == TRUE)

prior_yes <- cat_dist_combined() %>%
  rbind(cat_dist(hip_hrg, "Hip procedure"),
        cat_dist(knee_hrg, "Knee procedure"),
        cat_dist(gaba_any, "Gabapentinoids (any)"),
        cat_dist(gaba_3plus, "Gabapentinoids (>=3)"),
        cat_dist(nsaid_any, "NSAID (any)"),
        cat_dist(nsaid_3plus, "NSAID (>=3)"),
        cat_dist(tca_any, "TCA (any)"),
        cat_dist(tca_3plus, "TCA (>=3)"),
        cat_dist(ad_any, "Antidepressant (any)"),
        cat_dist(ad_3plus, "Antidepressant (>=3)")) %>%
  rename(count_any_opioid = count, total_any_opioid = total)

# No prior opioid rx only
dat <- ortho_routine_final %>%
  subset(any_opioid == FALSE)

prior_no <- cat_dist_combined() %>%
  rbind(cat_dist(hip_hrg, "Hip procedure"),
        cat_dist(knee_hrg, "Knee procedure"),
        cat_dist(gaba_any, "Gabapentinoids (any)"),
        cat_dist(gaba_3plus, "Gabapentinoids (>=3)"),
        cat_dist(nsaid_any, "NSAID (any)"),
        cat_dist(nsaid_3plus, "NSAID (>=3)"),
        cat_dist(tca_any, "TCA (any)"),
        cat_dist(tca_3plus, "TCA (>=3)"),
        cat_dist(ad_any, "Antidepressant (any)"),
        cat_dist(ad_3plus, "Antidepressant (>=3)")) %>%
  rename(count_opioid_naive = count, total_opioid_naive = total)


# long term opioid only
dat <- ortho_routine_final %>%
  subset(long_term_opioid == TRUE)

long_yes <- cat_dist_combined() %>%
  rbind(cat_dist(hip_hrg, "Hip procedure"),
        cat_dist(knee_hrg, "Knee procedure"),
        cat_dist(gaba_any, "Gabapentinoids (any)"),
        cat_dist(gaba_3plus, "Gabapentinoids (>=3)"),
        cat_dist(nsaid_any, "NSAID (any)"),
        cat_dist(nsaid_3plus, "NSAID (>=3)"),
        cat_dist(tca_any, "TCA (any)"),
        cat_dist(tca_3plus, "TCA (>=3)"),
        cat_dist(ad_any, "Antidepressant (any)"),
        cat_dist(ad_3plus, "Antidepressant (>=3)")) %>%
  rename(count_opioid_long = count, total_opioid_long = total)


# People with OA only
dat <- ortho_routine_final %>%
  subset(oa == TRUE)

oa_yes <- cat_dist_combined() %>%
  rbind(cat_dist(hip_hrg, "Hip procedure"),
        cat_dist(knee_hrg, "Knee procedure"),
        cat_dist(gaba_any, "Gabapentinoids (any)"),
        cat_dist(gaba_3plus, "Gabapentinoids (>=3)"),
        cat_dist(nsaid_any, "NSAID (any)"),
        cat_dist(nsaid_3plus, "NSAID (>=3)"),
        cat_dist(tca_any, "TCA (any)"),
        cat_dist(tca_3plus, "TCA (>=3)"),
        cat_dist(ad_any, "Antidepressant (any)"),
        cat_dist(ad_3plus, "Antidepressant (>=3)")) %>%
  rename(count_oa = count, total_oa = total)



# People with hip procedure only
dat <- ortho_routine_final %>%
  subset(hip_hrg == TRUE)

hip_yes <- cat_dist_combined() %>%
  rbind(cat_dist(hip_hrg, "Hip procedure"),
        cat_dist(knee_hrg, "Knee procedure"),
        cat_dist(gaba_any, "Gabapentinoids (any)"),
        cat_dist(gaba_3plus, "Gabapentinoids (>=3)"),
        cat_dist(nsaid_any, "NSAID (any)"),
        cat_dist(nsaid_3plus, "NSAID (>=3)"),
        cat_dist(tca_any, "TCA (any)"),
        cat_dist(tca_3plus, "TCA (>=3)"),
        cat_dist(ad_any, "Antidepressant (any)"),
        cat_dist(ad_3plus, "Antidepressant (>=3)")) %>%
  rename(count_hip = count, total_hip = total)


# People with knee procedure only
dat <- ortho_routine_final %>%
  subset(knee_hrg == TRUE)

knee_yes <- cat_dist_combined() %>%
  rbind(cat_dist(hip_hrg, "Hip procedure"),
        cat_dist(knee_hrg, "Knee procedure"),
        cat_dist(gaba_any, "Gabapentinoids (any)"),
        cat_dist(gaba_3plus, "Gabapentinoids (>=3)"),
        cat_dist(nsaid_any, "NSAID (any)"),
        cat_dist(nsaid_3plus, "NSAID (>=3)"),
        cat_dist(tca_any, "TCA (any)"),
        cat_dist(tca_3plus, "TCA (>=3)"),
        cat_dist(ad_any, "Antidepressant (any)"),
        cat_dist(ad_3plus, "Antidepressant (>=3)")) %>%
  rename(count_knee = count, total_knee = total)


# Merge 
cat_dist <- list(prior_yes, prior_no, long_yes, oa_yes, hip_yes, knee_yes) %>% 
  reduce(full_join, by=c("category","var","cohort")) %>%
  filter(category != FALSE) %>%
  arrange(var, category) %>%
  replace(is.na(.), 0)

cat_dist <- cat_dist[,c("cohort", "var", "category", 
                        "count_any_opioid", "total_any_opioid", 
                        "count_opioid_naive", "total_opioid_naive",
                        "count_opioid_long", "total_opioid_long",
                        "count_oa", "total_oa",
                          "count_hip", "total_hip","count_knee","total_knee")]

write.csv(cat_dist, here::here("output", "clockstops",  "cat_var_dist_opioid.csv"),
          row.names = FALSE) 


#################

