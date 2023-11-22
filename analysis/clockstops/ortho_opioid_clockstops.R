###############################################################
# Cumulative opioid use
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
dat <- read_csv(here::here("output", "data", "cohort_ortho_clockstops.csv.gz"),
                col_types = cols(rtt_start_date = col_date(format="%Y-%m-%d"),
                                 rtt_end_date = col_date(format="%Y-%m-%d"),
                                 reg_end_date = col_date(format="%Y-%m-%d"),
                                 dod = col_date(format="%Y-%m-%d"),
                                 end_date = col_date(format="%Y-%m-%d"),
                                 rtt_start_month =  col_date(format="%Y-%m-%d"),
                                 rtt_end_month =  col_date(format="%Y-%m-%d"))) %>%
  mutate(opioid_pre_gp = ifelse(opioid_pre_count >3, 4, opioid_pre_count),
         opioid_wait_gp = ifelse(opioid_wait_count >3, 4, opioid_wait_count),
         opioid_post_gp = ifelse(opioid_post_count >3, 4, opioid_post_count),
         new_opioid_wait = (opioid_pre_count == 0 & opioid_wait_count > 0),
         new_hi_opioid_wait = (hi_opioid_pre_count == 0 & hi_opioid_wait_count >0)) 


cumulative_opi <- dat %>%
  subset(end_date > rtt_end_date) %>%
  group_by(week52) %>%
  summarise(p50 = quantile(opioid_wait_count, .5, na.rm=TRUE),
            p75 = quantile(opioid_wait_count, .75, na.rm=TRUE),
            total_opioid_rx = sum(opioid_wait_count),   
            n_round = rounding(n()),
            mean = total_opioid_rx / n_round) %>%
  ungroup()

write.csv(cumulative_opi, here::here("output", "clockstops", "cumulative_opi.csv"),
          row.names = FALSE)

ggplot(cumulative_opi) +
  geom_point(aes(x = week52, y = mean), col = "dodgerblue3") +
  ylab("Mean") + xlab("Weeks on waiting list") +
  theme_bw() +
  theme()

ggsave(here::here("output", "clockstops", "cumulative_opi.png"),
       dpi = 300, width = 4.5, height = 3.5, unit = "in")

