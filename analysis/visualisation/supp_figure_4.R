################################################################################
# Code for generating Supplementary Figure 4. 
# 
# Number of opioid prescriptions per week per 100 people in the 26 weeks 
# prior to waiting list referral date, during the waiting list, and up to 52 
# weeks after waiting list end date (date of treatment) among people who had a 
# knee procedure (A) and by opioid type (B). 
################################################################################

library('tidyverse')
library('here')
library('dplyr')
library('ggplot2')
library('fs')
library('patchwork')



setwd("C:/Users/aschaffer/OneDrive - Nexus365/Documents/GitHub/waiting-list")
dir_create(here::here("output", "released_outputs"), showWarnings = FALSE, recurse = TRUE)
source(here("analysis", "plot_functions.R"))


################################################################################

# Read in data
opi_week <- 
  read.csv(here::here("output","released_outputs","final","opioid_by_week_oa.csv")) %>%
  
  subset(oa_diagnosis == "TRUE") %>%
  
  mutate(# Create continuous week variable for entire study period
    week2 = ifelse(period == "During WL", week + 26,
                   ifelse(period == "Post-WL", week + 52 + 26, week)),
    
    # Prescribing rate
    rate = opioid_rx / denominator * 100,
    rate_lci = rate - 1.96*sqrt((rate*(100- rate))/denominator),
    rate_uci = rate + 1.96*sqrt((rate*(100- rate))/denominator),
    
    # OPioid grouping
    kind = ifelse(opioid_type %in%
                    c("Strong opioid 1","Strong opioid 2", 
                      "Moderate opioid", "Weak opioid"), 
                  "By opioid strength",
                  ifelse(opioid_type %in% 
                           c("Any opioid"), "All opioids", "By duration of analgesia")))


# Start and end of waiting list
lines <- tibble(line = c(26.5, 78.5))
lines2 <- tibble(line = c(26.5, 52.5))


################################################################################

# Modelling 

mod_any <- pred.mod("Any opioid") 


# Plot 

oa_opi <- pred.plot(mod_any, "maroon", 0, 25)
oa_denom <- denom.plot(mod_any, "maroon")

oa <- oa_opi / oa_denom + plot_layout(heights = c(2,1))
oa

ggsave(here::here("output", "released_outputs", "final", "figures", "supp_figure_4.tiff"), 
       dpi = 300, height = 4, width =4.5, units = "in")


