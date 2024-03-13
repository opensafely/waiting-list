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


## Load data - orthopaedic only ##
ortho_final <- read_csv(here::here("output", "data", "cohort_ortho_clockstops.csv.gz"),
                        col_types = cols(rtt_start_date = col_date(format="%Y-%m-%d"),
                                         rtt_end_date = col_date(format="%Y-%m-%d"),
                                         reg_end_date = col_date(format="%Y-%m-%d"),
                                         dod = col_date(format="%Y-%m-%d"),
                                         end_date = col_date(format="%Y-%m-%d"),
                                         rtt_start_month =  col_date(format="%Y-%m-%d"),
                                         rtt_end_month =  col_date(format="%Y-%m-%d"))) 


############ Categorical variable relative frequency distributions #############
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
  reduce(full_join, by=c("category","var","cohort")) %>%
  filter(category != FALSE) %>%
  mutate(
          # Un-redact structural zeroes
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

cat_dist <- cat_dist[,c("cohort", "var", "category", 
       "count_routine_admitted", "total_routine_admitted",
       "count_routine_notadmitted", "total_routine_notadmitted",
       "count_urgent_admitted", "total_urgent_admitted",
       "count_urgent_notadmitted", "total_urgent_notadmitted")]

write.csv(cat_dist, here::here("output", "clockstops",  "cat_var_dist_ortho.csv"),
          row.names = FALSE) 

