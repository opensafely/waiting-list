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
opioid_rx <- read_csv(here::here("output", "measures", "measures_opioid_weeks.csv"),
                         col_types = cols(interval_start = col_date(format="%Y-%m-%d"))) %>%
              dplyr::select(c(numerator, denominator, interval_start, num_weeks)) %>%
  mutate(
    # Convert interval start date to number of weeks 
    week = as.numeric(((interval_start - as.Date("2000-01-01"))/7)) + 1,
    period = "During WL",
    opioid_rx = numerator,
    rate = opioid_rx / denominator * 100
  ) %>% 
  dplyr::select(!c(interval_start, numerator)) %>%
  arrange(num_weeks, week)

write.csv(opioid_rx, file = here::here("output", "clockstops", "opioid_by_week_group.csv"),
          row.names = FALSE)


ggplot(opioid_rx) +
  geom_line(aes(x = week, y = rate, group = num_weeks, col = num_weeks), linewidth =1) +
  #geom_ribbon( aes(ymin = rate_lci, ymax = rate_uci, x = week2, 
  #                fill = period), alpha = 0.15) +
  # scale_color_manual(values = c("dodgerblue3", "maroon", "goldenrod1")) +
  # scale_fill_manual(values = c("dodgerblue3", "maroon", "goldenrod1")) +
  scale_x_continuous(limits = c(1, 52)) +
  ylab("Rate per 100 people per week") + xlab("Week") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray95"))

ggsave(here::here("output", "clockstops", "opioid_by_week_group_v1.png"), dpi = 300,
       height = 3, width = 4.5)


ggplot(subset(opioid_rx,
              num_weeks %in% c(4,8,12,24,36,48))) +
  geom_line(aes(x = weeks, y = rate, group = num_weeks, col = num_weeks), size =1) +
  #geom_ribbon( aes(ymin = rate_lci, ymax = rate_uci, x = week2, 
  #                fill = period), alpha = 0.15) +
  # scale_color_manual(values = c("dodgerblue3", "maroon", "goldenrod1")) +
  # scale_fill_manual(values = c("dodgerblue3", "maroon", "goldenrod1")) +
  scale_x_continuous(limits = c(1, 52)) +
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
       height = 8, width = 8)