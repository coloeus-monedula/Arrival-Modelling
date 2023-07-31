source("code/00 Helper Functions.R")

library("mgcv")
library("ggplot2")


# https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/

# note: use "bam" for the big dataset? if doing multiple years
# note: need to take into account species count for reliabilitty?

# # extracting dummy dataset
# swallow_2021 <-  get_bird_data(path = "data_in/RENEW_extract.csv", species = "Swallow")
# users_2021 <- get_user_data(path = "data_in/RENEW_extract.csv") %>% 
#   filter(year(date) == 2021)
# gc()
# 
# swallow_2021 <- swallow_2021 %>% 
#   filter(year(date) == 2021)
# 
# #to avoid loading dataset again
# write_csv(swallow_2021,file = "data_temp/swallow_2021.csv")
# write_csv(users_2021, file="data_temp/users_2021.csv")


year = 2022
#TL88 swallow 2022
bird <- get_presenceabsence_data("data_in/RENEW_extract_TL.csv", tenkm_area = "TL88", species = "Swallow", year = year) 
# convert to numerical day of the year
bird$day <- yday(bird$date)
bird$week <- week(bird$date)


# sense check - weekly reporting rate
bird_weekly <- bird %>% 
  group_by(week) %>% 
  summarise(rate = mean(presence))

# midpoint for plotting
bird_weekly$day <- ((bird_weekly$week -1) * 7) + 3.5


# TODO: add smoothed relationship w number of species recorded 

# TODO: change funcs so one gives 0/1 data for all species (like simon's code), and another summarises into count and presence/absence data for a focal species
gam_bird <- gam(presence~s(day, k=10), method="REML",family = "binomial", data = bird) 

# trying to predict values
x_new <- seq(from=0, to=365, by=0.25)
y_pred <- predict(gam_bird, data.frame(day=x_new), type="response")


predicted <- data.frame(day = x_new, rate = y_pred)

first_diff <- diff(predicted$rate)
# change from increasing rate to just starting to decrease
change <- which(first_diff<0)[1]
first_peak <- predicted[change,]

ten_percent <- first_peak$rate * 0.1
arrival_start <- predicted[which(predicted$rate >= ten_percent)[1],]

# -1 day since peak is calculated using when it starts decreasing - "offset" that
arrival_date <- as.Date(arrival_start$day-1, origin = ymd(year, truncated=2))

# # fit residuals
# refit_resid <- reporting_rate
# refit_resid$resid <- gam_swallow$residuals
# 
# gam_resid <- gam(resid ~ s(day) + s(n), data = refit_resid, method = "REML")
# par(mfrow = c(2, 2))
# gam.check(gam_resid)

# todo: add line where x-intercept is crossed
ggplot(data=predicted, aes(x=day, y=rate)) +
  geom_col(data = bird_weekly, aes(x=day, y=rate), fill="grey75")+
  geom_line() +
  geom_vline(xintercept = first_peak$day, col="blue") +
  geom_vline(xintercept = arrival_start$day, col="green")

# par(mfrow = c(2, 2))
# summary(gam_bird)
# gam.check(gam_bird)

