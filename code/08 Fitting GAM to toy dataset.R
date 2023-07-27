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

swallow_2021 <- read_csv("data_temp/swallow_2021.csv")
users_2021 <- read_csv("data_temp/users_2021.csv")

# get reporting rate
# x = date. y = reporting rate
swallow_daily <- get_interval_lists(data_list = swallow_2021, interval = "day", is_bird = "TRUE")
users_daily <- get_interval_lists(data_list = users_2021, interval = "day")

reporting_rate <- get_reportingrate_time(bird_list = swallow_daily, total_lists = users_daily, invar = "interval_of")

# convert to numerical day of the year
reporting_rate$day <- yday(reporting_rate$interval_of)

# TODO: add smoothed relationship w number of species recorded 
gam_swallow <- gam(n~s(day, k = 20,), method="REML", data = reporting_rate) 

# trying to predict values
x_new <- seq(from=1, to=365)
y_pred <- predict(gam_swallow, data.frame(day=x_new))
predicted <- data.frame(day = x_new, n = y_pred)


# fit residuals
refit_resid <- reporting_rate
refit_resid$resid <- gam_swallow$residuals

gam_resid <- gam(resid ~ s(day) + s(n), data = refit_resid, method = "REML")
par(mfrow = c(2, 2))
gam.check(gam_resid)

# todo: add line where x-intercept is crossed
ggplot(data=reporting_rate, aes(x=day, y=n)) +
  geom_point() +
  geom_point(data = predicted, colour="red") 

par(mfrow = c(2, 2))
summary(gam_swallow)
gam.check(gam_swallow)

