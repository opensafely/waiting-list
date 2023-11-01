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
any_opioid_rx <- read_csv(here::here("output", "measures", "measures_any_opioid.csv"),
                  col_types = cols(interval_start = col_date(format="%Y-%m-%d"))) %>%
              mutate(opioid_type = "Any opioid")

hi_opioid_rx <- read_csv(here::here("output", "measures", "measures_hi_opioid.csv"),
                         col_types = cols(interval_start = col_date(format="%Y-%m-%d"))) %>%
              mutate(opioid_type = "High dose opioid")

opioid_rx <- rbind(any_opioid_rx, hi_opioid_rx) %>%
 
       mutate(# Convert interval start date to number of weeks 
              week = as.numeric(((interval_start - as.Date("2000-01-01"))/7)) + 1,
       
              # If pre-WL, count backward
              week = ifelse(grepl("_pre", measure, fixed=TRUE), week*(-1), week),
                  
              # Create variables for prior opioid Rx, opioid type, and time period
              prior_opioid_gp = ifelse(is.na(prior_opioid_rx), "Full cohort",
                            ifelse(prior_opioid_rx == TRUE, "Prior opioid Rx",
                                   "No prior opioid Rx")),
                  
              period = ifelse(grepl("pre", measure), "Pre-WL", 
                          ifelse(grepl("post", measure), "Post-WL", "During WL")),
   
              # Rounding
              opioid_rx = rounding(numerator),
              denominator = rounding(denominator),
                  
              # Rx per 100 people
              rate = opioid_rx / denominator * 100,
              rate_lci = rate - (1.96 * sqrt( ( rate * (100 - rate) ) / denominator)),
              rate_uci = rate + (1.96 * sqrt( ( rate * (100 - rate) ) / denominator)),
              ) %>%
  
       dplyr::select(c(week, prior_opioid_gp, opioid_rx, denominator, rate,
                     rate_lci, rate_uci, opioid_type, period)) %>%
                     
       arrange(prior_opioid_gp, opioid_type, period, week)

write.csv(opioid_rx, file = here::here("output", "clockstops", "opioid_by_week.csv"),
          row.names = FALSE)


# Plot - pre and during WL - FULL COHORT
ggplot(subset(opioid_rx, period %in% c("Pre-WL", "During WL") 
              & prior_opioid_gp == "Full cohort")) +
  geom_line(aes(x = week, y = rate, col = opioid_type)) +
  geom_ribbon(aes(ymin = rate_lci, ymax = rate_uci, x = week), alpha = 0.15) +
  geom_vline(aes(xintercept = 0)) +
  scale_color_manual(values = c("dodgerblue3", "maroon")) +
  scale_x_continuous(limits = c(-26, 52)) +
  facet_wrap(~ opioid_type, nrow = 2) +
  ylab("Rate per 100 people") + xlab("Week") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray95"))


ggsave(here::here("output", "clockstops", "plot_opioid_pre_during.png"), dpi = 300,
         height = 5, width = 6)


# Plot - post WL - FULL COHORT
ggplot(subset(opioid_rx, period %in% c("Post-WL")
       & prior_opioid_gp == "Full cohort")) +
  geom_line(aes(x = week, y = rate, col = opioid_type)) +
  geom_ribbon(aes(ymin = rate_lci, ymax = rate_uci, x = week), alpha = 0.15) +
  scale_color_manual(values = c("dodgerblue3", "maroon")) +
  scale_x_continuous(limits = c(1, 26)) +
  facet_wrap(~ opioid_type, nrow = 2) +
  ylab("Rate per 100 people") + xlab("Week") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray95"))


ggsave(here::here("output", "clockstops", "plot_opioid_post.png"), dpi = 300,
       height = 6, width = 4)


# Plot - during WL - BY PRIOR OPIOID
ggplot(subset(opioid_rx, period %in% c("During WL") 
              & prior_opioid_gp != "Full cohort")) +
  geom_line(aes(x = week, y = rate, col = prior_opioid_gp, group = prior_opioid_gp)) +
  geom_ribbon(aes(ymin = rate_lci, ymax = rate_uci, x = week, group = prior_opioid_gp), alpha = 0.15) +
  geom_vline(aes(xintercept = 0)) +
  scale_color_manual(values = c("#0f85a0", "#dd4124")) +
  scale_x_continuous(limits = c(-26, 52)) +
  facet_wrap(~ opioid_type, nrow = 2) +
  ylab("Rate per 100 people") + xlab("Week") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray95"))


ggsave(here::here("output", "clockstops", "plot_opioid_pre_during_prior.png"), dpi = 300,
       height = 5, width = 6)

=