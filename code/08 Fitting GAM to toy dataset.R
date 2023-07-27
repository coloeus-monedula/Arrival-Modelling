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

#TL88 swallow 2021
swallows <- get_presenceabsence_data("data_in/RENEW_extract_TL.csv", tenkm_area = "TL88", species = "Swallow", year = 2021) 
# convert to numerical day of the year
swallows$day <- yday(swallows$date)




# TODO: add smoothed relationship w number of species recorded 
gam_swallow <- gam(presence~s(day), method="REML",family = "binomial", data = swallows) 

# trying to predict values
x_new <- seq(from=1, to=365, by=0.25)
y_pred <- predict(gam_swallow, data.frame(day=x_new))

# first differential?

predicted <- data.frame(day = x_new, y = y_pred)

first_diff <- diff(y_pred)
# shifts day so inbetween, then removes last row
day_shift <- x_new +0.125
day_shift <- head(day_shift, -1)

# TODO: https://stackoverflow.com/questions/25091581/how-to-detect-sign-change-eg-positive-to-negative-in-time-series-data 
# computational way of detecting sign change
predict_firstdiff <- data.frame(day=day_shift, y=first_diff)
predict_firstdiff$sign <- diff(sign(first_diff))


# # fit residuals
# refit_resid <- reporting_rate
# refit_resid$resid <- gam_swallow$residuals
# 
# gam_resid <- gam(resid ~ s(day) + s(n), data = refit_resid, method = "REML")
# par(mfrow = c(2, 2))
# gam.check(gam_resid)

# todo: add line where x-intercept is crossed
ggplot(data=predicted, aes(x=day, y=y)) +
  geom_point()

par(mfrow = c(2, 2))
summary(gam_swallow)
gam.check(gam_swallow)

