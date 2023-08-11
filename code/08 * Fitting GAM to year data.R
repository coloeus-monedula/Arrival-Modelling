source("code/00 Helper Functions.R")

library("mgcv")
library("ggplot2")




predict_GAM_graph <- function(gam_bird, x_count, title, zero_threshold = 0.00001, small_peak_threshold = 0.1) {
  
  # trying to predict values
  x_day <- seq(from=0, to=365, by=0.25)
  
  y_pred <- predict(gam_bird, data.frame(day=x_day, count=x_count), type="response")
  #get rid of minute fluctuations
  y_pred <- ifelse(y_pred < zero_threshold, 0, y_pred)
  
  predicted <- data.frame(day = x_day, rate = y_pred)
  
  diffs <- diff(predicted$rate)
  #can't have diff for first value
  predicted$diff <- c(NA, diffs)
  
  max_rate <- max(predicted$rate)
  
  #when doing calculations ignore the initial downwards slope if any
  #also cuts out flat gradient ie. = 0
  first_positive <- which(predicted$diff>0)[1]
  sliced <- slice(predicted, first_positive:n())
  
  # change from increasing rate to just starting to decrease
  # also ignores small peaks
  change <- which(sliced$diff < 0 & sliced$rate > small_peak_threshold*max_rate)[1]
  first_peak <- sliced[change,]
  
  #account for increased baseline in case of eg. overwintering species
  # ten percent taken using the difference between first peak and minimum rate
  ten_percent <- (first_peak$rate-min(predicted$rate))  * 0.1 + min(predicted$rate)
  #account for the downwards initial slope edge case
  arrival_start <- sliced[which(sliced$rate >= ten_percent)[1],]
  
  arrival_date <- as.Date(arrival_start$day-1, origin = ymd(year, truncated=2))
  # put into graph
  graph <- ggplot(data=predicted, aes(x=day, y=rate)) +
    geom_col(data = bird_weekly, aes(x=day, y=rate), fill="grey75")+
    geom_line() +
    geom_vline(xintercept = first_peak$day, col="blue") +
    geom_vline(xintercept = arrival_start$day, col="green") +
    labs(title=title, subtitle = paste("Arrival date = ", format(as.Date(arrival_date), "%b %d")), x="Day", y="Reporting rate")
}


year <-  2022
species <- "CC"
tenkm_area <- "TL31"

# 2022 CC TL31 to test increased baseline
# 2022 CC TL88 for steep downslope

bird <- get_presenceabsence_data("data_in/RENEW_extract_TL.csv", tenkm_area = tenkm_area, species = species, year = year) 
# convert to numerical day of the year
bird$day <- yday(bird$date)
bird$week <- week(bird$date)


# sense check - weekly reporting rate
bird_weekly <- bird %>% 
  group_by(week) %>% 
  summarise(rate = mean(presence))

# midpoint for plotting
bird_weekly$day <- ((bird_weekly$week -1) * 7) + 3.5


gam_bird <- gam(presence~s(day, k=20) + s(count, k=10), method="REML",family = "binomial", data = bird) 

#dummy variable set to median of all lists
x_count <- median(bird$count)
title <- paste(species, "for", tenkm_area, "in", year)
graph <- predict_GAM_graph(gam_bird, x_count, title)
print(graph)
print(sum(bird$presence))
gc()

#dsm::rqgam_check(gam_bird)


# EARLIER LIST LENGTH COMPARISON
# # comparing effect of list length of arrival date
# bird_seen <- bird[bird$presence==1,] %>% 
#   arrange(count)
# 
# x_count_median <- median(bird_seen$count)
# x_count_low <- head(bird_seen, 1)$count
# x_count_high <- tail(bird_seen, 1)$count
# 
# median <- predict_GAM_graph(gam_bird, x_count = x_count_median)
# 
# low <- predict_GAM_graph(gam_bird, x_count = x_count_low)
# high <- predict_GAM_graph(gam_bird, x_count = x_count_high)
# 
# grid.arrange(high, median, low)


# # fit residuals
# refit_resid <- bird
# refit_resid$resid <- gam_bird$residuals
# 
# gam_resid <- gam(resid ~ s(day) + s(n), data = refit_resid, method = "REML")
# par(mfrow = c(2, 2))
# gam.check(gam_resid)

# par(mfrow = c(2, 2))
# summary(gam_bird)
# gam.check(gam_bird)

