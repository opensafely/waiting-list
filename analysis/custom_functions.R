#########################################
# This script contains custom functions  
#########################################


##### Rounding and redaction #####
rounding <- function(vars) {
  case_when(vars > 7 ~ round(vars / 5) * 5)
}


##### Summary statistic functions #####

# RTT start/end dates by month
rtt_dates <- function(source, cohort){
  
  rtt_month <- dat %>%
    mutate(month = pmax(rtt_start_month, as.Date("2020-01-01")),
           type = "RTT start date") %>%
    group_by(month, type) %>%
    summarise(count = n()) %>%
    bind_rows(
      dat %>%
        rename(month = rtt_end_month) %>%
        mutate(type = "RTT end date") %>%
        group_by(month, type) %>%
        summarise(count = n())
    ) %>%
    mutate(count_round = rounding(count), source = source, cohort = cohort) %>%
    select(-count)

  write.csv(rtt_month, here::here("output", source, paste0("rtt_dates_", cohort, ".csv")),
            row.names = FALSE)
  
  ggplot(rtt_month, aes(x = month, y = count_round, fill = type)) +
    geom_bar(stat = "identity", position = "dodge", col = "white") +
    scale_fill_manual(values = c("dodgerblue3", "maroon")) +
    scale_x_date(breaks = seq(as.Date("2020-01-01"), as.Date("2022-01-01"), by = "6 months"),
                 date_labels = "%b %Y") +
    facet_wrap(~ type, nrow = 2) +
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
  
  ggsave(here::here("output", source, paste0("rtt_dates_", cohort, ".png")), dpi = 300,
         height = 4.5, width = 6)

}

# Categorical variable distribution
cat_dist <- function(variable, name) {
  
  dat %>%
    group_by({{ variable }}) %>%
    summarise(count = n()) %>%
    mutate(
      var = name,
      category = as.character({{variable}}),
      count_round = rounding(count)
    ) %>%
    ungroup() %>%
    mutate(
      total_round = rounding(sum(count)),
      pcent = count_round / total_round * 100
    ) %>%
    select(-count, -total_round)
}


# Waiting time distribution
wait <- function(source, cohort){
    
  quantile <- scales::percent(c(.1,.25,.5,.75,.9,.95,.99))
  
  # Percentiles
  wait_time_pcent <- dat %>% 
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
    rename(value = V1) %>%
    mutate(source = source, cohort = cohort)
  
  write.csv(wait_time_pcent, here::here("output", source, paste0("wait_time_pcent_", cohort,".csv")),
            row.names = FALSE)
  
  # By week
  wait_time <- dat %>%
    mutate(total = n()) %>%
    group_by(week52, total) %>%
    summarise(count = n()) %>%
    mutate(count_round = rounding(count),
           total_round = rounding(total),
           source = source, cohort = cohort) %>%
    dplyr::select(!c(count, total))
  
  # Save data for plot
  write.csv(wait_time, file = here::here("output", source, paste0("wait_time_", cohort, ".csv")),
            row.names = FALSE)
  
  # Plot 
  ggplot(wait_time) +
    geom_bar(aes(x = week52, y = count_round), 
             position = "dodge", stat = "identity", col = "white", fill = "dodgerblue3") + 
    scale_x_continuous(breaks = c(4, 8, 12, 18, 26, 34, 42, 52)) +
    xlab("No. weeks") + ylab("No. people") +
    theme_bw() + theme(panel.grid.major.x = element_blank(),
                       panel.grid.minor.x = element_blank(),
                       legend.position = "none",
                       axis.text.x = element_text(angle = 45, hjust = 1),
                       axis.title.y = element_text(size = 10),
                       strip.background = element_blank(),
                       strip.text = element_text(hjust = 0))
  
  ggsave(here::here("output", source, paste0("wait_time_", cohort, ".png")), dpi = 300,
         height = 3, width = 4.5)

}


# Waiting time distribution
wait_gp <- function(gp, name){
  
  dat %>%
    group_by({{gp}}) %>%
    mutate(total = n(),
              p25 = quantile(wait_time, .25, na.rm=TRUE),
              p50 = quantile(wait_time, .5, na.rm=TRUE),
              p75 = quantile(wait_time, .75, na.rm=TRUE)) %>%
    group_by({{gp}}, week_gp, total, p25, p50, p75) %>%
    summarise(count = n()) %>%
    mutate(count_round = rounding(count),
           total_round = rounding(total),
           var = name,
           pcent = count_round / total_round * 100) %>%
    rename(category = {{gp}}) %>%
    ungroup() %>%
    dplyr::select(!c(count, total))

}


# Frequency distribution of categorical variables
cat_dist <- function(variable, name) {
  
  dat %>%
    group_by({{ variable }}) %>%
    summarise(count = n()) %>%
    mutate(
      var = name,
      count_round = rounding(count)
    ) %>%
    ungroup() %>%
    mutate(
      total_round = rounding(sum(count)),
      pcent = count_round / total_round * 100
    ) %>%
    rename(category = {{variable}}) %>%
    # Only output top 20 categories for each variable
    arrange(var, -pcent) %>%
    group_by(var) %>%
    mutate(row_number = row_number(),
           category = as.character(category)) %>%
    subset(row_number <= 20) %>%
    select(-count, -total_round, -row_number)
  
}



# Count total number of Rx, total person-days, and p25/median/75
#   for each period and each medicine group
summ <- function(wait_time, var, med, time) {
  dat %>%
    mutate(person_days = sum({{wait_time}}),
           total_rx = sum({{var}})) %>%
    group_by(person_days, total_rx) %>%
    summarise(
      p25 = quantile({{var}}, 0.25, na.rm = TRUE),
      p50 = quantile({{var}}, 0.50, na.rm = TRUE),
      p75 = quantile({{var}}, 0.75, na.rm = TRUE)
    ) %>%
    mutate(med_gp = med, time = time,
           total_rx_round = rounding(total_rx),
           pdays_round = rounding(person_days), 
           rate_pmonth = total_rx_round / person_days * 100 * 30) %>%
    ungroup() %>%
    dplyr::select(!c(total_rx, person_days))
}
