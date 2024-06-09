#########################################
# This script contains custom functions  
#########################################


##### Rounding and redaction #####
rounding <- function(vars) {
  case_when(vars == 0 ~ 0,
            vars > 7 ~ round(vars / 5) * 5)
}


# Frequency distribution of categorical variables
cat_dist <- function(variable, name) {
  
  dat %>%
    mutate(total = n()) %>%
    group_by({{variable}}, total) %>%
    summarise(count = n()) %>%
    mutate(
      var = name,
      count = rounding(count)
    ) %>%
    ungroup() %>%
    mutate(
      total = rounding(total),
      cohort = "Orthopaedic"
    ) %>%
    rename(category = {{variable}}) %>%
    mutate(category = as.character(category))
  
}


# Frequency distribution of categorical variables
cat_dist_combined <- function() {
  
  rbind(
    cat_dist(censor_before_rtt_end, "Censor before WL end"),
    cat_dist(censor_before_study_end, "Censor before study end"),
    cat_dist(rtt_multiple, "Multiple RTT pathways"),
    cat_dist(covid_timing, "Start date timing"),
    cat_dist(died_during_post, "Died during post-WL follow-up"),
    
    cat_dist(age_group, "Age group"),
    cat_dist(sex, "Sex"),
    cat_dist(imd10, "IMD decile"),
    cat_dist(ethnicity6, "Ethnicity (6 groups)"),
    cat_dist(region, "Region"),
    
    cat_dist(cancer, "Cancer"),
    cat_dist(diabetes, "Diabetes"),
    cat_dist(cardiac, "Cardiac"),
    cat_dist(copd, "COPD"),
    cat_dist(liver, "Liver"),
    cat_dist(ckd, "CKD"),
    cat_dist(oa, "Osteoarthritis"),
    cat_dist(depression, "Depression"),
    cat_dist(anxiety, "Anxiety"),
    cat_dist(smi, "Severe mental illness"),
    cat_dist(oud, "Opioid use disorder"),
    cat_dist(ra, "Rheumatoid arthritis"),
    cat_dist(no_opioid, "No opioid"),
    cat_dist(long_term_opioid, "Long-term opioid"),
    cat_dist(prior_opioid_rx, ">=3 opioid Rx")
  ) 
  
}

# Number of people with any prescription and 3+ prescriptions
# during each time period
meds_dist <- function(var, name) {
  
  dat %>% 
    mutate(full = "Full cohort") %>%
    subset(period %in% c("Pre-WL","Post WL")) %>%
    group_by({{var}}, period, measure) %>%
    mutate(total = rounding(n()), 
           total_post = rounding(sum(censor_before_study_end == FALSE))) %>%
    ungroup() %>%
    group_by({{var}}, measure, period, total, total_post) %>%
    summarise(count_any_6mos = rounding(sum(med_any_6mos)),
              count_any_3mos = rounding(sum(med_any_3mos)),
              count_3more_6mos = rounding(sum(med_3more_6mos)),
              count_3more_3mos = rounding(sum(med_3more_3mos)),
              count_none_3mos = rounding(sum(med_none_3mos)),
              count_none_6mos = rounding(sum(med_none_6mos))) %>%
    ungroup() %>%
    rename(category = {{var}}) %>%
    mutate(cohort = "Orthopaedic - Routine/Admitted",
           category = as.character(category),
           variable = name,
           total = ifelse(period == "Pre-WL", total, total_post)) %>%
    dplyr::select(!total_post)
  
}

# Count total number of Rx, total person-days
#   for each period and each medicine group
ptime <- function(gp, var){
  
  ortho_routine_final_2 %>%
    mutate(full = "Full cohort",
           prior_opioid_rx = ifelse(prior_opioid_rx == TRUE, "Yes", "No")) %>%
    group_by(period, measure, {{gp}}) %>%
    summarise(person_days = rounding(sum(person_time)),
              count_rx = rounding(sum(value))) %>%
    rename(category = {{gp}}) %>%
    mutate(variable = var,  cohort = "Orthopaedic - Routine/Admitted")
  
}

# Stratified by demographics
wait_gp <- function(gp, name){
  
  dat %>%
    group_by({{gp}}) %>%
    mutate(total = n(),
           p25 = quantile(wait_time, .25, na.rm=TRUE),
           p50 = quantile(wait_time, .5, na.rm=TRUE),
           p75 = quantile(wait_time, .75, na.rm=TRUE)) %>%
    group_by({{gp}}, wait_gp, total, p25, p50, p75) %>%
    summarise(count = n()) %>%
    mutate(count = rounding(count),
           total = rounding(total),
           var = name,
           cohort = "Orthopaedic - Routine/Admitted",
           p25 = ifelse(total <= 32, NA, p25),
           p50 = ifelse(total <= 32, NA, p50),
           p75 = ifelse(total <= 32, NA, p75),
           name = ifelse(wait_gp == "<=18 weeks", 1,
                         ifelse(wait_gp == "19-52 weeks", 2, 3))) %>%
    rename(category = {{gp}}) %>%
    ungroup() %>%
    arrange(var, category, name) %>%
    pivot_wider(id_cols = c("cohort","var","category","total","p25","p50","p75"),
                values_from = "count", 
                names_prefix = "wait_gp")
  
}