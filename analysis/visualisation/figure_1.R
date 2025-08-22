################################################################################
# Code for generating Figure 1.
#
# Number of completed waits for elective trauma and orthopaedic 
# admitted procedures based on month of waiting list end date (A) and time on 
# waiting list for these procedures (B), Jan 2019 to Apr 2022. 
################################################################################

library('tidyverse')
library('here')
library('dplyr')
library('ggplot2')
library('zoo')
library('reshape2')
library('fs')
library('patchwork')


setwd("C:/Users/aschaffer/OneDrive - Nexus365/Documents/GitHub/waiting-list/")
dir_create(here::here("output", "released_outputs", "figures"), showWarnings = FALSE, recurse = TRUE)

################################################################################

# Read in aggregate data, calculate percent in each waiting time category

# rtt_read <- function(path){
#   read.csv(paste0("G:/Shared drives/OpenSAFELY/Research/Opioids/Opioids in people on waiting list/Supporting documents/Monthly raw data/", path, ".csv")) %>%
#     subset(Treatment.Function.Code %in% c("C_110") 
#            & RTT.Part.Description %in% c("Completed Pathways For Admitted Patients",
#                                          "Completed Pathways For Non-Admitted Patients",
#                                          "Incomplete Pathways")) %>%
#     mutate(wait_gp_1 = across(starts_with(c("Gt.0", "Gt.10.T", "Gt.11", "Gt.12", "Gt.13",
#                                             "Gt.14", "Gt.15", "Gt.16", "Gt.17"))) %>% rowSums(., na.rm = TRUE),
#             wait_gp_2 = across(starts_with(c("Gt.18", "Gt.19", "Gt.2", "Gt.3", "Gt.4", "Gt.50", "Gt.51"))) %>% rowSums(., na.rm = TRUE),
#             wait_gp_3 = across(starts_with(c("Gt.52", "Gt.53", "Gt.54", "Gt.55", "Gt.56", "Gt.57",
#                                              "Gt.58", "Gt.59", "Gt.6", "Gt.7", "Gt.8", "Gt.9", "Gt.101",
#                                              "Gt.102", "Gt.103", "Gt.104" ))) %>% rowSums(., na.rm = TRUE)) %>%
#     dplyr::select(c(Period, RTT.Part.Description, Treatment.Function.Code, Treatment.Function.Name, 
#                     wait_gp_1, wait_gp_2, wait_gp_3, Total.All)) %>%
#     group_by(Period, RTT.Part.Description, Treatment.Function.Code, Treatment.Function.Name) %>%
#     summarise(Total.All = sum(Total.All),
#               wait_gp_1 = sum(wait_gp_1), 
#               wait_gp_2 = sum(wait_gp_2), wait_gp_3 = sum(wait_gp_3)) %>%
#     mutate(wait_1_pcent = wait_gp_1 / Total.All * 100,
#            wait_2_pcent = wait_gp_2 / Total.All * 100,
#            wait_3_pcent = wait_gp_3 / Total.All * 100)
# }
# 
# rtt_all <- rbind(
#     rtt_read("20190330-RTT-MARCH-2019-full-extract"),
#     rtt_read("20190430-RTT-APRIL-2019-full-extract-revised"),
#     rtt_read("20190531-RTT-MAY-2019-full-extract-revised"),
#     rtt_read("20190630-RTT-JUNE-2019-full-extract-revision"),
#     rtt_read("20190731-RTT-JULY-2019-full-extract-revised"),
#     rtt_read("20190831-RTT-AUGUST-2019-full-extract-revised"),
#     rtt_read("20190930-RTT-SEPTEMBER-2019-full-extract"),
#     rtt_read("20191031-RTT-OCTOBER-2019-full-extract"),
#     rtt_read("20191130-RTT-NOVEMBER-2019-full-extract-revised"),
#     rtt_read("20191231-RTT-DECEMBER-2019-full-extract-revised"),
# 
#     rtt_read("20200131-RTT-JANUARY-2020-full-extract-revised"),
#     rtt_read("20200229-RTT-FEBRUARY-2020-full-extract-revised"),
#     rtt_read("20200331-RTT-MARCH-2020-full-extract"),
#     rtt_read("20200430-RTT-APRIL-2020-full-extract-revised"),
#     rtt_read("20200531-RTT-MAY-2020-full-extract-revised"),
#     rtt_read("20200630-RTT-JUNE-2020-full-extract-revised"),
#     rtt_read("20200731-RTT-JULY-2020-full-extract"),
#     rtt_read("20200831-RTT-AUGUST-2020-full-extract"),
#     rtt_read("20200930-RTT-SEPTEMBER-2020-full-extract"),
#     rtt_read("20201031-RTT-OCTOBER-2020-full-extract-revised"),
#     rtt_read("20201130-RTT-NOVEMBER-2020-full-extract"),
#     rtt_read("20201231-RTT-DECEMBER-2020-full-extract-revised"),
# 
#     rtt_read("20210131-RTT-JANUARY-2021-full-extract"),
#     rtt_read("20210228-RTT-FEBRUARY-2021-full-extract"),
#     rtt_read("20210331-RTT-MARCH-2021-full-extract"),
#     rtt_read("20210430-RTT-APRIL-2021-full-extract-revised"),
#     rtt_read("20210531-RTT-MAY-2021-full-extract-revised"),
#     rtt_read("20210630-RTT-JUNE-2021-full-extract-revised"),
#     rtt_read("20210731-RTT-JULY-2021-full-extract-revised"),
#     rtt_read("20210831-RTT-AUGUST-2021-full-extract-revised"),
#     rtt_read("20210930-RTT-SEPTEMBER-2021-full-extract-revised"),
#     rtt_read("20211031-RTT-OCTOBER-2021-full-extract-revised"),
#     rtt_read("20211130-RTT-NOVEMBER-2021-full-extract-revised"),
#     rtt_read("20211231-RTT-DECEMBER-2021-full-extract-revised"),
# 
#     rtt_read("20220131-RTT-JANUARY-2022-full-extract-revised"),
#     rtt_read("20220228-RTT-FEBRUARY-2022-full-extract-revised"),
#     rtt_read("20220331-RTT-MARCH-2022-full-extract-revised"),
#     rtt_read("20220430-RTT-APRIL-2022-full-extract-revised")
#   ) %>% arrange(RTT.Part.Description) 
# 
# write.csv(rtt_all, "C:/Users/aschaffer/OneDrive - Nexus365/Documents/GitHub/waiting-list/output/descriptive/rtt_all.csv",
#           row.names = FALSE)


# Read in combined file
rtt_all <- read.csv("C:/Users/aschaffer/OneDrive - Nexus365/Documents/GitHub/waiting-list/output/descriptive/rtt_all.csv")

rtt_pcent <- rtt_all %>%
  cbind(month =  rep(seq(as.Date("2019-03-01"), by="months", length.out=38), 3)) %>%
  ungroup() %>%
  dplyr::select(!c(Period, Treatment.Function.Code, Treatment.Function.Name, Total.All,
                   wait_gp_1, wait_gp_2, wait_gp_3)) %>%
  melt(id = c("month","RTT.Part.Description"))

rtt_pcent$variable <- factor(rtt_pcent$variable,
                            levels = c("wait_1_pcent", "wait_2_pcent", "wait_3_pcent"),
                            labels = c("\u226418 weeks", "19-52 weeks", ">52 weeks"))

##################################################################################

# Create plots

a <- ggplot(subset(rtt_pcent, RTT.Part.Description %in% c("Completed Pathways For Admitted Patients"))) +
  geom_bar(aes(x = month, y = value, group = variable, fill = variable), col = "white", 
           stat= "identity") +
  scale_fill_manual(values = c("dodgerblue3", "goldenrod2", "maroon")) +
  xlab("") + ylab("Wait duration (%)") +
  theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(), axis.title.y=element_text(size = 10),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

rtt_total <- rtt_all %>%
  cbind(month =  rep(seq(as.Date("2019-03-01"), by="months", length.out=38), 3)) %>%
  ungroup()

b <- ggplot(subset(rtt_total, RTT.Part.Description %in% c("Completed Pathways For Admitted Patients")),
       aes(x = month, y = Total.All)) +
  geom_rect(aes(ymin = 0, ymax = Inf, xmin = as.Date("2020-03-01"), xmax = as.Date("2021-04-01")),
            alpha=.1, fill = "gray92") +
  
  geom_vline(aes(xintercept = as.Date("2021-04-01")), col = "gray30", linetype = "longdash",
           linewidth = .5) +
  geom_vline(aes(xintercept = as.Date("2020-03-01")), col = "gray30", linetype = "longdash",
             linewidth = .5) +
  geom_text(aes(x=as.Date("2020-09-15"), label="COVID-19\nlockdown period", y=66000), size=2.5) +
  geom_line(color = "dodgerblue4", linewidth = 1) +
  scale_y_continuous(limits = c(0,70000)) +
  xlab(NULL) + ylab("Completed admitted waits (n)") +
  theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        axis.text.x = element_blank(), axis.title.y=element_text(size = 10),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

b / a + plot_annotation(tag_levels = 'A')


# Save
ggsave("C:/Users/aschaffer/OneDrive - Nexus365/Documents/GitHub/waiting-list/output/released_outputs/figures/figure_1.tiff",
       width = 5.5, height = 5.25, dpi = 300, units = "in")