################################################################################
# Code for generating Figure 2. 
# 
# Number of opioid prescriptions per week per 100 people in the 26 weeks 
# prior to waiting list referral date, during the waiting list, and up to 52 
# weeks after waiting list end date (date of treatment) overall (A) and by 
# opioid type (B). 
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
  read.csv(here::here("output","released_outputs","final","opioid_by_week_full.csv")) %>%
  
  mutate(# Create continuous week variable for entire study period
          week2 = ifelse(period == "During WL", week + 26,
                        ifelse(period == "Post-WL", week + 52 + 26, week)),
         
          # Prescribing rate
          rate = opioid_rx / denominator * 100,
          rate_lci = rate - 1.96*sqrt((rate*(100- rate))/denominator),
          rate_uci = rate + 1.96*sqrt((rate*(100- rate))/denominator),
          
          # Opioid grouping
          kind = ifelse(opioid_type %in%
                          c("Strong opioid 1","Strong opioid 2", 
                            "Moderate opioid", "Weak opioid"), 
                       "By opioid strength",
                       ifelse(opioid_type %in% 
                                c("Any opioid"), "All opioids", "By duration of analgesia")))

opi_week$opioid_type <- factor(opi_week$opioid_type,
                               levels = c("Any opioid", "Weak opioid", 
                                          "Strong opioid 1", "Strong opioid 2", 
                                          "Moderate opioid", "Short-acting opioid",
                                          "Long-acting opioid"),
                               labels = c("Any opioid", "Weak opioid", 
           "Moderate/strong opioid", "Strong opioid", "Moderate opioid",
           "Immediate-release opioid",
           "Modified-release opioid"))

# Start and end of waiting list
lines <- tibble(line = c(26.5, 78.5))
lines2 <- tibble(line = c(26.5, 52.5))


################################################################################

###### Modelling 

mod_any <- pred.mod("Any opioid") 
mod_long <- pred.mod("Modified-release opioid") 
mod_short <- pred.mod("Immediate-release opioid")
mod_weak <- pred.mod("Weak opioid") 
mod_mod <- pred.mod("Moderate opioid")
mod_strong <- pred.mod("Strong opioid")

mod_modstrong <- pred.mod("Moderate/strong opioid")


################################################################################

######## Plots 

# Full cohort - all opioids
full_opi <- pred.plot(mod_any, "maroon", 0, 18)
full_denom <- denom.plot(mod_any, "maroon")

full <- full_opi / full_denom + plot_layout(heights = c(2,1))
full

ggsave(here::here("output", "released_outputs", "final", "figures", "figure_2A.tiff"), 
       dpi = 300, height = 4, width =4.5, units = "in")


# Full cohort - by opioid type
full_type <- rbind(mod_modstrong, mod_weak, mod_short, mod_long) 

full_type$opioid_type <- factor(full_type$opioid_type, levels = 
                                 c("Immediate-release opioid","Modified-release opioid",
                                   "Weak opioid","Moderate/strong opioid"))

full_comb <- plot.pred.type(full_type, 0, 18)
full_comb

ggsave(here::here("output", "released_outputs", "final", "figures", "figure_2B.tiff"), 
       dpi = 300, height = 4.5, width =5, units = "in")




