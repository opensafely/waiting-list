
###############################################
# This script contains functions for creating 
# loess curves and figures
###############################################


##### Functions 

# Poisson model with splines 
pred.mod <- function(type){
  
  model <- function(wl,  type){
    
    dat <- subset(opi_week, 
                  period == wl
                  & opioid_type == type) %>%
      mutate(rate = opioid_rx / denominator * 100)
    
    mod <- loess(opioid_rx / denominator * 100 ~ week, data = dat, span = .3)
    
    pred <- predict(mod) 
    
    dat <- cbind(select(dat, c(week, rate, denominator, period, 
                               opioid_type, kind)), pred) %>% 
      rename(pred_rate = pred)
    
    
    return(dat)
  }
  
  combined <- rbind(model("Pre-WL", type),
                    model("During WL", type),
                    model("Post-WL", type)) %>%
    mutate(week2 = ifelse(period == "During WL", week + 26, 
                          ifelse(period == "Post-WL", week + 78, week)))
  
  return(combined)
  
}

## Poisson model with splines (by waiting time)
pred.mod.wait <- function(type){
  
  model <- function(wl, wait, type){
    
    dat <- subset(opi_week, 
                  period == wl
                  & opioid_type == type
                  & wait_gp == wait) %>%
      mutate(rate = opioid_rx / denominator * 100)
    
    mod <- loess(opioid_rx / denominator * 100 ~ week, data = dat, span = .4)
    
    pred <- predict(mod) 
    
    dat <- cbind(select(dat, c(week, rate, denominator, period, 
                               opioid_type, wait_gp, kind)), pred) %>% 
      rename(pred_rate = pred)
    
    
    return(dat)
  }
  
  combined <- rbind(model("Pre-WL", "\u226418 weeks", type),
                    model("During WL", "\u226418 weeks", type),
                    model("Post-WL", "\u226418 weeks",  type),
                    
                    model("Pre-WL", "19-52 weeks",  type),
                    model("During WL", "19-52 weeks", type),
                    model("Post-WL", "19-52 weeks",  type),
                    
                    model("Pre-WL",  ">52 weeks", type),
                    model("During WL", ">52 weeks",type),
                    model("Post-WL", ">52 weeks", type)) %>%
    mutate(week2 = ifelse(period == "During WL", week + 26, 
                          ifelse(period == "Post-WL", week + 78, week)))
  
  return(combined)
  
}

## Plot predicted + observed values
pred.plot <- function(dat, col, lower, upper){
  ggplot() +
    geom_rect(aes(xmin=79,xmax=90,ymin=0,ymax=Inf),fill="gray",alpha=.3) +
    geom_point(data = dat, aes(x = week2, y = rate), 
               col = col, size = 1, alpha = .5) +
    geom_line(data = subset(dat, week2 <= 78), aes(x = week2, y = pred_rate), 
              col = col, linewidth = 1) +
    geom_line(data=subset(dat, week2 > 78), aes(x = week2, y = pred_rate), 
              col = col, linewidth = 1) +
    geom_vline(data = lines, aes(xintercept = line), 
               col = "gray60", size = 1, linetype = "longdash") +
    scale_y_continuous(limits = c(lower, upper)) +    
    scale_x_continuous(limits = c(1, 130),
                       breaks = c(1,  27, 78, 90, 130),
                       labels = c("-26 weeks",
                                  "Waiting list referral date", 
                                  "Waiting list end date","12 weeks","52 weeks")) +
    ylab("Prescriptions per\n100 people per week") + xlab(NULL) +
    facet_wrap(~kind) +
    theme_bw() +
    theme(strip.background = element_blank(),
          strip.text = element_text(hjust = 0),
          legend.title = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_text(size = 9),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = "gray95"))
}


## Plot denominator
denom.plot <- function(dat, fill){
  
  ggplot(dat) +
    geom_bar(aes(x = week2, y = denominator), 
             col="white", fill = fill, stat = "identity",
             position=position_dodge())+ 
    geom_vline(data = lines, aes(xintercept = line), 
               col = "gray60", size = 1, linetype = "longdash") +
    scale_x_continuous(limits = c(1, 130),
                       breaks = c(1,  27, 78, 90, 130),
                       labels = c("-26 weeks",
                                  "Waiting list referral date", 
                                  "Waiting list end date","12 weeks","52 weeks")) +
    scale_y_continuous(expand=expansion(mult=.1))+
    ylab("Denominator") + xlab(NULL) +
    theme_bw() +
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          legend.title = element_blank(), 
          legend.position = "none", 
          axis.title.y = element_text(size = 9),
          axis.text.x = element_text(angle = 45, hjust=1),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = "gray95"))
}


rect_data <- data.frame(
  xmax = 90,
  xmin = 79,
  opioid_type = c(
    "Weak opioid", "Strong/moderate opioid", "Immediate-release opioid",
    "Modified-release opioid"
  )
)

## Plot all opioid types in one figure
plot.pred.type <- function(dat, lower, upper){
  
  ggplot() +
    geom_rect(data=rect_data, aes(xmin=xmin,xmax=xmax),ymin=0,ymax=Inf,fill="gray",alpha=.1) +
    
    geom_point(data = dat, aes(x = week2, y = rate, col = opioid_type, group = opioid_type), 
               size = 1, alpha = .5) +
    geom_line(data = subset(dat, week2 <= 78), 
              aes(x = week2, y = pred_rate, col = opioid_type, group = opioid_type), 
              linewidth = 1) +
    geom_line(data=subset(dat, week2 > 78), 
              aes(x = week2, y = pred_rate, col = opioid_type, group = opioid_type), 
              linewidth = 1) +
    geom_vline(data = lines, aes(xintercept = line), 
               col = "gray60", size = 1, linetype = "longdash") +
    scale_color_manual(values = c("maroon","dodgerblue3","goldenrod2","seagreen","purple4"))+
    scale_fill_manual(values = c("maroon","dodgerblue3","goldenrod2","seagreen","purple4"))+
    scale_y_continuous(limits = c(lower, upper)) +    
    scale_x_continuous(limits = c(1, 130),
                       breaks = c(1,  27, 78, 90, 130),
                       labels = c("-26 weeks",
                                  "Waiting list referral date", 
                                  "Waiting list end date","12 weeks","52 weeks")) +
    facet_wrap(~ kind, nrow = 2) +
    ylab("Prescriptions per 100 people per week") + xlab(NULL) +
    theme_bw() +
    theme(strip.background = element_blank(),
          strip.text = element_text(hjust = 0),
          legend.title = element_blank(),
          legend.text = element_text(size = 8),
          legend.position = "right",
          axis.title.y = element_text(size = 9),
          axis.text.x = element_text(angle = 45, hjust=1),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = "gray95"))
}


## Plot all opioid types in one figure
plot.pred.type.wait <- function(dat, lower, upper){
  
  dat <- subset(dat, wait_gp != "\u226418 weeks" | (wait_gp == "\u226418 weeks" & week2!=44))
  
  ggplot() +
    geom_point(data = dat, aes(x = week2, y = rate, col = wait_gp, group = wait_gp), 
               size = 1, alpha = .3) +
    geom_line(data = subset(dat, week2 <= 78), 
              aes(x = week2, y = pred_rate, col = wait_gp, group = wait_gp), 
              linewidth = 1) +
    geom_line(data=subset(dat, week2 > 78), 
              aes(x = week2, y = pred_rate, col = wait_gp, group =wait_gp), 
              linewidth = 1) +    
    geom_rect(data=rect_data,aes(xmin=xmin,xmax=xmax,group=wait_gp),ymin=0,
              ymax=Inf,fill="gray",alpha=.1) +
    geom_vline(data = lines, aes(xintercept = line), 
               col = "gray60", size = 1, linetype = "longdash") +
    scale_color_manual(values = c("maroon","dodgerblue3","goldenrod2","seagreen"))+
    scale_fill_manual(values = c("maroon","dodgerblue3","goldenrod2","seagreen"))+
    #  scale_y_continuous(limits = c(lower, upper)) +    
    scale_x_continuous(limits = c(1, 130),
                       breaks = c(1,  27, 78, 90, 130),
                       labels = c("-26 weeks",
                                  "Waiting list referral", 
                                  "Waiting list end","12 weeks","52 weeks")) +
    facet_wrap(~ opioid_type, nrow = 3) +
    ylab("Prescriptions per\n100 people per week") + xlab(NULL) +
    theme_bw() +
    theme(strip.background = element_blank(),
          strip.text = element_text(hjust = 0),
          legend.position="bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 8),
          axis.title.y = element_text(size = 9),
          axis.text.x = element_text(angle = 45, hjust=1),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = "gray95"))
}

