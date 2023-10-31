###############################################################
# This script creates summary statistics for all key variables
# for all people with a closed RTT pathway (May21-May22)
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
full <- read_csv(here::here("output", "data", "dataset_clockstops.csv.gz"),
  col_types = cols(rtt_start_date = col_date(format="%Y-%m-%d"),
                    rtt_end_date = col_date(format="%Y-%m-%d"),
                    reg_end_date = col_date(format="%Y-%m-%d"),
                    dod = col_date(format="%Y-%m-%d"),
                    end_date = col_date(format="%Y-%m-%d"))) %>%
                
                # Exclude if dod before RTT end date
                subset(is.na(dod) | (!is.na(dod) & rtt_end_date > dod)) %>%
  
                # Create new variables                
                mutate(# Month of WL start/end
                    rtt_start_month = floor_date(rtt_start_date, "month"),
                    rtt_end_month = floor_date(rtt_end_date, "month"),
                       
                    # Were on multiple WL during study period
                    rtt_multiple = ifelse(count_rtt_pathways > 1, 1, 0),
                    
                    # Orthopaedic surgery
                    ortho_surgery = (treatment_function == "110"),
                    
                    # Died while on WL
                    died_during_wl = ifelse(!is.na(dod) & dod == rtt_end_date, 1, 0),
                       
                    # Time on WL, censored at death/deregistration
                    wait_time_adjusted = as.numeric(
                        pmin(rtt_end_date, end_date, na.rm = FALSE) - rtt_start_date + 1),
                       
                    # Time post-WL, censored at death/deregistration (max 182 days)
                    #    If study end date before RTT end date, set to zero
                    post_time_adjusted = ifelse(
                        end_date >= rtt_end_date, 
                        as.numeric(pmin((rtt_end_date + 182), end_date, na.rm = FALSE) - rtt_end_date + 1),
                        0),
                       
                    # Time pre-WL (182 days for everyone)
                    pre_time = 182,
                    
                    any_opioid_pre = ifelse(opioid_pre_count > 0, 1, 0),
                    any_opioid_wait = ifelse(opioid_wait_count > 0, 1, 0),
                    any_opioid_post = ifelse(opioid_post_count > 0, 1, 0),
                    
                    hi_opioid_pre = ifelse(hi_opioid_pre_count > 0, 1, 0),
                    hi_opioid_wait = ifelse(hi_opioid_wait_count > 0, 1, 0),
                    hi_opioid_post = ifelse(hi_opioid_post_count > 0, 1, 0),
                    
                    gaba_pre = ifelse(gaba_pre_count > 0, 1, 0),
                    gaba_wait = ifelse(gaba_wait_count > 0, 1, 0),
                    gaba_post = ifelse(gaba_post_count > 0, 1, 0),
                    
                    nsaid_pre = ifelse(nsaid_pre_count > 0, 1, 0),
                    nsaid_wait = ifelse(nsaid_wait_count > 0, 1, 0),
                    nsaid_post = ifelse(nsaid_post_count > 0, 1, 0),
                    
                    ad_pre = ifelse(ad_pre_count > 0, 1, 0),
                    ad_wait = ifelse(ad_wait_count > 0, 1, 0),
                    ad_post = ifelse(ad_post_count > 0, 1, 0))
                         

## Save as final
write.csv(full, file = here::here("output", "data", "cohort_full_clockstops.csv.gz"),
          row.names = FALSE)


############## Plot end dates by month #################

rtt_start_month <- full %>%
  # If start date before Jan 2020, set to Jan 2020 (for plotting)
  mutate(month = ifelse(rtt_start_month < as.Date("2020-01-01"), 
                        as.Date("2020-01-01"), rtt_start_month),
         month = as.Date(month, format="%Y-%m-%d"),
         type = "RTT start date") %>%
  group_by(month, type) %>%
  summarise(count = n()) 

rtt_end_month <- full %>%
  rename(month = rtt_end_month) %>%
  mutate(type = "RTT end date") %>%
  group_by(month, type) %>%
  summarise(count = n()) 

rtt_month <- rbind(rtt_start_month, rtt_end_month) %>%
  mutate(count_round = rounding(count)) %>%
  dplyr::select(!count)

# Save plot data 
write.csv(rtt_month, file = here::here("output", "clockstops", "rtt_dates.csv"),
          row.names = FALSE)

# Plot 
plot1 <- ggplot(rtt_month) +
  geom_bar(aes(x = month, y = count_round, group = type, fill = type), 
           position = "dodge", stat = "identity", col = "white") + 
  scale_fill_manual(values = c("dodgerblue3", "maroon")) + 
  scale_x_continuous(breaks = c(as.Date("2020-01-01"), as.Date("2020-07-01"),
                                as.Date("2021-01-01"), as.Date("2021-07-01"),
                                as.Date("2022-01-01")),
                     labels = c("Jan 2020\nor earlier", "Jul 2020", "Jan 2021", "Jul 2021", "Jan 2022")) +
  facet_wrap(~ type, nrow =2) +
  xlab("") + ylab("No. people") +
  theme_bw() + theme(panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 10),
                     axis.text.x = element_text(angle = 45, hjust = 1),
                     axis.title.y = element_text(size = 10),
                     strip.background = element_blank(),
                     strip.text = element_text(hjust = 0))

ggsave(here::here("output", "clockstops", "plot_rtt_dates.png"), plot1, dpi = 300,
       height = 4.5, width = 6)


############ Categorical variable relative frequency distributions #############

cat <- function(variable, name){
  full %>%
    mutate(total = n()) %>%
    group_by({{variable}}, total) %>%
    summarise(count = n()) %>%
    mutate(var = name) %>%
    rename(category = {{variable}}) %>%
    mutate(category = as.character(category),
           count_round = rounding(count),
           total_round = rounding(total),
           pcent = count_round / total_round * 100) %>%
    dplyr::select(!c(count, total))
}

categorical_dist <- rbind(
  cat(waiting_list_type, "Waiting list type"),
  cat(treatment_function, "Treatment function"),
  cat(priority_type, "Priority type"),
  cat(censor_before_rtt_end, "Censor before WL end"),
  cat(censor_before_study_end, "Censor before study end"),
  cat(rtt_multiple, "Multiple RTT pathways"),
  cat(age_group, "Age group"),
  cat(sex, "Sex"),
  cat(imd10, "IMD decile"),
  cat(ethnicity6, "Ethnicity (6 groups)"),
  cat(ethnicity16, "Ethnicity (16 groups)"),
  cat(region, "Region"),
  
  cat(cancer, "Cancer"),
  cat(diabetes, "Diabetes"),
  cat(cardiac, "Cardiac"),
  cat(copd, "COPD"),
  cat(liver, "Liver"),
  cat(ckd, "CKD"),
  cat(osteoarthritis, "Osteoarthritis"),
  cat(depress_or_gad, "Depression/GAD"),
  cat(died_during_wl, "Died while on WL"),
  
  cat(any_opioid_pre, "Any opioid (pre-WL)"),
  cat(any_opioid_wait, "Any opioid (during WL)"),
  cat(any_opioid_post, "Any opioid (post-WL)"),
  
  cat(hi_opioid_pre, "High dose opioid (pre-WL)"),
  cat(hi_opioid_wait, "High dose opioid (during WL)"),
  cat(hi_opioid_post, "High dose opioid (post-WL)"),
  
  cat(gaba_pre, "Gabapentinoid (pre-WL)"),
  cat(gaba_wait, "Gabapentinoid (during WL)"),
  cat(gaba_post, "Gabapentinoid (post-WL)"),
  
  cat(nsaid_pre, "NSAID (pre-WL)"),
  cat(nsaid_wait, "NSAID (during WL)"),
  cat(nsaid_post, "NSAID (post-WL)")
)

# Create count variable - for treatment function only output top 20 codes
categorical_dist <- categorical_dist %>%
  arrange(var, -pcent) %>%
  group_by(var) %>%
  mutate(count = row_number()) %>%
  subset(var != "Treatment function" | (var == "Treatment function" & count <= 20)) %>%
  select(!count)
  
write.csv(categorical_dist, here::here("output", "clockstops", "cat_var_dist.csv"),
          row.names = FALSE)


############### Waiting time #################

quantile <- scales::percent(c(.1,.25,.5,.75,.9,.95,.99))

# Percentiles
wait_time_pcent <- full %>% 
    summarise(p10 = quantile(wait_time, .1, na.rm=TRUE),
           p25 = quantile(wait_time, .25, na.rm=TRUE),
           p50 = quantile(wait_time, .5, na.rm=TRUE),
           p75 = quantile(wait_time, .75, na.rm=TRUE),
           p90 = quantile(wait_time, .9, na.rm=TRUE),
           p95 = quantile(wait_time, .95, na.rm=TRUE),
           p99 = quantile(wait_time, .99, na.rm=TRUE)) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column("percentile") %>%
  rename(value = V1)

write.csv(wait_time_pcent, here::here("output", "clockstops", "wait_time_pcent.csv"),
          row.names = FALSE)

# By week
wait_time <- full %>%
  mutate(week = ceiling(wait_time / 7),
         week = ifelse(week > 52, 52, week)) %>%
  mutate(total = n()) %>%
  group_by(week, total) %>%
  summarise(count = n()) %>%
  mutate(count_round = rounding(count),
         total_round = rounding(total)) %>%
  dplyr::select(!c(count_round, total_round))

# Save data for plot
write.csv(wait_time, file = here::here("output", "clockstops", "plot_wait_time.csv"),
          row.names = FALSE)

# Plot 
plot2 <- ggplot(wait_time) +
  geom_bar(aes(x = week, y = count), 
           position = "dodge", stat = "identity", col = "white", fill = "dodgerblue3") + 
  scale_x_continuous(breaks = c(4, 8, 12, 18, 26, 34, 42, 52)) +
  xlab("No. weeks") + ylab("No. people") +
  theme_bw() + theme(panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 10),
                     axis.text.x = element_text(angle = 45, hjust = 1),
                     axis.title.y = element_text(size = 10),
                     strip.background = element_blank(),
                     strip.text = element_text(hjust = 0))

ggsave(here::here("output", "clockstops", "plot_wait_time.png"), plot2, dpi = 300,
       height = 3, width = 5)


################## Medicine Rx count variables ####################

# Count total number of Rx, total person-days, and p25/median/75
#   for each period (per, during, post WL) and each medicine group
summ <- function(var1, var2, var3, med){
  
  wl <- full %>% 
    mutate(person_days = sum(wait_time_adjusted),
           total_rx = sum({{var1}})) %>%
    group_by(person_days, total_rx) %>%
    summarise_at(vars({{var1}}), list(p25 = ~quantile(., .25, na.rm=TRUE),
                                       p50 = ~quantile(., .5, na.rm=TRUE),
                                       p75 = ~quantile(., .75, na.rm=TRUE))) %>%
    mutate(med_name = med, time = "Waiting list")
  
  pre_wl <- full %>% 
    mutate(person_days = sum(pre_time),
           total_rx = sum({{var2}})) %>%
    group_by(person_days, total_rx) %>%
    summarise_at(vars({{var2}}), list(p25 = ~quantile(., .25, na.rm=TRUE),
                                          p50 = ~quantile(., .5, na.rm=TRUE),
                                          p75 = ~quantile(., .75, na.rm=TRUE))) %>%
    mutate(med_name = med, time = "Pre-waiting list")
  
  post_wl <- full %>% 
    mutate(person_days = sum(post_time_adjusted),
           total_rx = sum({{var3}})) %>%
    group_by(person_days, total_rx) %>%
    summarise_at(vars({{var3}}), list(p25 = ~quantile(., .25, na.rm=TRUE),
                                          p50 = ~quantile(., .5, na.rm=TRUE),
                                          p75 = ~quantile(., .75, na.rm=TRUE))) %>%
    mutate(med_name = med, time = "Post-waiting list")
  
  combined <- rbind(wl, pre_wl, post_wl)
  
  return(combined)
}

med_count <- rbind(
  summ(opioid_wait_count, opioid_pre_count, opioid_post_count, "Any opioid"),
  summ(hi_opioid_wait_count, hi_opioid_pre_count, hi_opioid_post_count, "High dose opioid"),
  summ(nsaid_wait_count, nsaid_pre_count, nsaid_post_count, "NSAID"),
  summ(gaba_wait_count, gaba_pre_count, gaba_post_count, "Gabapentinoid"),
  summ(ad_wait_count, ad_wait_count, ad_wait_count, "Antidepressant")
)

med_count_2 <- med_count %>%
  mutate(total_rx_round = rounding(total_rx),
         pdays_round = rounding(person_days), 
         rate_pmonth = total_rx_round / person_days * 100 * 30) %>%
  dplyr::select(!c(total_rx, person_days))

write.csv(med_count_2, here::here("output", "clockstops", "med_counts_by_period.csv"),
          row.names = FALSE)

