################################################################################
# Code for generating Supplementary Figure 2. 
# 
# Distribution of waiting list start date (referral date) and end date 
# (admission date) 
################################################################################

library('tidyverse')
library('here')
library('dplyr')
library('ggplot2')
library('fs')


setwd("C:/Users/aschaffer/OneDrive - Nexus365/Documents/GitHub/waiting-list")
dir_create(here::here("output", "released_outputs"), showWarnings = FALSE, recurse = TRUE)
source(here("analysis", "plot_functions.R"))

options(scipen = 9999)

### Start /end dates by month

dat <- read_csv(here::here("output", "released_outputs", "final", "rtt_dates.csv"), col_types = cols(month = col_date(format="%Y-%m-%d")))
dat$type <- factor(dat$type, levels =c("RTT start date", "RTT end date"),
                   labels = c("Waiting list referral date", "Waiting list end date"))
  
ggplot(dat, aes(x = month, y = count, fill = type)) +
    geom_bar(stat = "identity", position = "dodge", col = "white") +
    scale_fill_manual(values = c("dodgerblue3", "maroon")) +
    scale_x_date(breaks = seq(as.Date("2020-01-01"), as.Date("2022-01-01"), by = "6 months"),
                 date_labels = "%b %Y") +
    facet_wrap(~ type, nrow = 2) +
    scale_x_continuous(breaks = c(as.Date("2020-01-01"),
                                  as.Date("2020-07-01"),
                                  as.Date("2021-01-01"),
                                  as.Date("2021-07-01"),
                                  as.Date("2022-01-01")),
                       labels = c("On or before\nJan 2020","Jul 2020","Jan 2021","Jul 2021","Jan 2022")) +
    labs(x = "", y = "No. people") +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.y = element_text(size = 10),
          strip.background = element_blank(),
          strip.text = element_text(hjust = 0)
    )


ggsave(here::here("output", "released_outputs", "final", "figures", "supp_figure_1.tiff"),
       dpi = 300, height = 4.5, width = 4.5, units = "in")



