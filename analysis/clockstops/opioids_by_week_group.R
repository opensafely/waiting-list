###############################################################
# This script plots opioid prescribing per 100 people by week
# in 6 months prior to WL start, during WL, and 6 months after WL end
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
dir_create(here::here("output", "measures"), showWarnings = FALSE, recurse = TRUE)


# Load data
opioid_rx <- rbind(
  read_csv(here::here("output", "measures", "measures_opioid_weeks_during.csv"),
                         col_types = cols(interval_start = col_date(format="%Y-%m-%d"))),
  read_csv(here::here("output", "measures", "measures_opioid_weeks_post.csv"),
         col_types = cols(interval_start = col_date(format="%Y-%m-%d")))
  ) %>%
  mutate(
    # Convert interval start date to number of weeks 
    tmp_week = as.numeric(((interval_start - as.Date("2000-01-01"))/7)) + 1,
    week = ifelse(measure == "count_post", num_weeks + tmp_week, tmp_week),
    period = ifelse(measure == "count_post", "Post-WL", "During WL"),
    opioid_rx = numerator,
    rate = opioid_rx / denominator * 100
  ) %>% 
  dplyr::select(!c(interval_start, numerator, ratio, measure, tmp_week)) %>%
  arrange(num_weeks, week)

write.csv(opioid_rx, file = here::here("output", "clockstops", "opioid_by_week_group.csv"),
          row.names = FALSE)


ggplot(subset(opioid_rx,
              num_weeks %in% c(4,8,12,16,20,24,28,32,36,40,44,48,52))) +
  geom_line(aes(x = week, y = rate, group = num_weeks, col = num_weeks), size =1) +
  geom_vline(aes(xintercept = num_weeks+ 1), linetype = "longdash", col = "gray70") +
  ylab("Rate per 100 people per week") + xlab("Week") +
  facet_wrap(~ num_weeks, scales = "free_y") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray95"))


ggsave(here::here("output", "clockstops", "opioid_by_week_group_v2.png"), dpi = 300,
       height = 10, width = 12)