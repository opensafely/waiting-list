################################################################################
# Code for generating Figure 3. 
# 
# Number of opioid prescriptions per week per 100 people in the 26 weeks prior 
# to waiting list referral date, during the waiting list, and 52 weeks after 
# waiting list end date (date of treatment) stratified by time on waiting list 
# and by opioid type. 
################################################################################


library('tidyverse')
library('here')
library('dplyr')
library('ggplot2')
library('fs')
library('patchwork')


setwd("C:/Users/aschaffer/OneDrive - Nexus365/Documents/GitHub/waiting-list")
dir_create(here::here("output", "released_outputs"), showWarnings = FALSE, recurse = TRUE)
source(here("analysis", "visualisation", "plot_functions.R"))


################################################################################

# Read in data
opi_week <- 
  read.csv(here::here("output","released_outputs","final","opioid_by_week_wait.csv")) %>%
  mutate(# Create continuous week variable for entire study period
          week2 = ifelse(period == "During WL", week + 26,
                        ifelse(period == "Post-WL", week + 52 + 26, week)),
         
          # Prescribing rate
          rate = opioid_rx / denominator * 100,
          rate_lci = rate - 1.96*sqrt((rate*(100- rate))/denominator),
          rate_uci = rate + 1.96*sqrt((rate*(100- rate))/denominator),
          
          # Opioid grouping
          kind = ifelse(opioid_type %in% c("Strong opioid 1","Strong opioid 2","Weak opioid"), 
                       "Opioid strength","Duration of analgesia"))

opi_week$opioid_type <- factor(opi_week$opioid_type,
                               levels = c("Any opioid", "Weak opioid", 
                                          "Strong opioid 1", "Strong opioid 2", 
                                          "Moderate opioid", "Short-acting opioid",
                                          "Long-acting opioid"),
                               labels = c("Any opioid", "Weak opioid", 
                                          "Strong/moderate opioid", "Strong opioid", 
                                          "Moderate opioid", "Immediate-release opioid",
                                          "Modified-release opioid"))

opi_week$wait_gp <- factor(opi_week$wait_gp,
                           levels = c("<=18 weeks", "19-52 weeks", ">52 weeks"),
                           labels = c("\u226418 weeks","19-52 weeks", ">52 weeks"))

# Start and end of waiting list
lines <- tibble(line = c(26.5, 78.5))
lines2 <- tibble(line = c(26.5, 52.5))


################################################################################

## Modelling 

mod_any <- pred.mod.wait("Any opioid") 
mod_long <- pred.mod.wait( "Modified-release opioid") 
mod_short <- pred.mod.wait( "Immediate-release opioid")
mod_weak <- pred.mod.wait("Weak opioid") 
mod_strong <- pred.mod.wait("Strong opioid")
mod_mod <- pred.mod.wait("Moderate opioid")
mod_modstrong <- pred.mod.wait("Strong/moderate opioid")

df <- rbind(mod_short, mod_long, mod_modstrong,  mod_weak)

df$opioid_type <- factor(df$opioid_type,
                               levels = c( "Weak opioid", "Strong/moderate opioid", 
                                           "Immediate-release opioid",
                                          "Modified-release opioid"))

################################################################################

# Plot

rect_data <- data.frame(
  xmax = 90,
  xmin = 79,
  opioid_type = c(
    "Weak opioid", "Strong/moderate opioid", "Immediate-release opioid",
    "Modified-release opioid",
    "Weak opioid", "Strong/moderate opioid",  "Immediate-release opioid",
    "Modified-release opioid",
    "Weak opioid", "Strong/moderate opioid",  "Immediate-release opioid",
    "Modified-release opioid"
  ),
  wait_gp = c("\u226418 weeks","\u226418 weeks","\u226418 weeks",
              "\u226418 weeks",
              "19-52 weeks", "19-52 weeks", "19-52 weeks", "19-52 weeks", 
              ">52 weeks",">52 weeks",">52 weeks",">52 weeks")
)


type <- plot.pred.type.wait(df, 0, 18) 
type

ggsave(here::here("output", "released_outputs",  "final", "figures","figure_3.tiff"), 
       dpi = 300, height =4.5, width =6, units = "in")