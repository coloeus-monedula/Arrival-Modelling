source("code/00 Helper Functions.R")

library("mgcv")
library("ggplot2")
library("gridExtra")


# https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/

year <-  2022

#TL88 swallow 2022
bird <- get_presenceabsence_data("data_in/RENEW_extract_TL.csv", tenkm_area = "TL88", species = "SI", year = year) 
# convert to numerical day of the year
bird$day <- yday(bird$date)
bird$week <- week(bird$date)


# sense check - weekly reporting rate
bird_weekly <- bird %>% 
  group_by(week) %>% 
  summarise(rate = mean(presence))

# midpoint for plotting
bird_weekly$day <- ((bird_weekly$week -1) * 7) + 3.5


gam_bird <- gam(presence~s(day, k=10) + s(count, k=10), method="REML",family = "binomial", data = bird) 



predict_GAM_graph <- function(gam_bird, x_count, zero_threshold = 0.00001) {
  
  # trying to predict values
  x_day <- seq(from=0, to=365, by=0.25)
  
  y_pred <- predict(gam_bird, data.frame(day=x_day, count=x_count), type="response")
  #get rid of minute fluctuations
  y_pred <- ifelse(y_pred < zero_threshold, 0, y_pred)
  
  predicted <- data.frame(day = x_day, rate = y_pred)
  
  first_diff <- diff(predicted$rate)
  
  # edge case to cover graphs that start with downwards slope - only count those that come after positive slope
  first_positive <- which(first_diff>0)[1]
  sliced <- first_diff[first_positive: length(first_diff)]
  
  #when doing calculations ignore the initial downwards slope
  predicted_sliced <- slice(predicted, first_positive:n())
  
  # change from increasing rate to just starting to decrease
  change <- which(sliced<0)[1] 
  first_peak <- predicted_sliced[change,]
  
  ten_percent <- first_peak$rate * 0.1
  #account for edge case
  arrival_start <- predicted_sliced[which(predicted_sliced$rate >= ten_percent)[1],]
  
  arrival_date <- as.Date(arrival_start$day-1, origin = ymd(year, truncated=2))
  print(arrival_date)
  
  # browser()
  
  # put into graph
  graph <- ggplot(data=predicted, aes(x=day, y=rate)) +
    geom_col(data = bird_weekly, aes(x=day, y=rate), fill="grey75")+
    geom_line() +
    geom_vline(xintercept = first_peak$day, col="blue") +
    geom_vline(xintercept = arrival_start$day, col="green") 
}



# for single graph, might be best if just do median number of list length regardless of presence/absence
x_count <- median(bird$count)
graph <- predict_GAM_graph(gam_bird, x_count)
graph


# comparing effect of list length of arrival date
bird_seen <- bird[bird$presence==1,] %>% 
  arrange(count)

x_count_median <- median(bird_seen$count)
x_count_low <- head(bird_seen, 1)$count
x_count_high <- tail(bird_seen, 1)$count

median <- predict_GAM_graph(gam_bird, x_count = x_count_median)

low <- predict_GAM_graph(gam_bird, x_count = x_count_low)
high <- predict_GAM_graph(gam_bird, x_count = x_count_high)

grid.arrange(high, median, low)


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

