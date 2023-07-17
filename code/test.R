
library(lubridate)
library(ggplot2)
library(readr)

#read the data
raw <- read_csv(file = "data_in/RENEW_extract_TL.csv")


#convert date string to date type and filter to one year for this simple example
raw$obs_dt2 <- as.Date(raw$obs_dt, format="%d/%m/%Y")
raw2019 <- raw

#get number of lists done per day
visits <- unique(raw2019[,c('sub_code', 'obs_dt2')])
visits_per_day <- setNames(aggregate(data = visits, sub_code ~ obs_dt2, NROW), c('obs_dt2', 'nvisits'))

#get number of lists per day with species of interest
focal_spp <- 'Swallow'
#focal_spp <- 'Woodpigeon'
#focal_spp <- 'Redwing'
bird <- unique(raw2019[raw2019$english_name == focal_spp,c('sub_code', 'obs_dt2')])
bird_per_day <- setNames(aggregate(data = bird, sub_code ~ obs_dt2, NROW), c('obs_dt2', 'nvisits_with_bird'))

#merge effort and bird data, and infill zeroes for lists done when no birds
visit_and_bird <- merge(visits_per_day, bird_per_day, by = 'obs_dt2', all.x = TRUE)
visit_and_bird[is.na(visit_and_bird)] <- 0

#unsmoothed reporting rate - simply proprtion of lists occupied on each day
visit_and_bird$rep_rate <- 100 * visit_and_bird$nvisits_with_bird / visit_and_bird$nvisits

windowsize <- 10

#loop over days
smoothed_rep_rate <- list()
for(d in 1:nrow(visit_and_bird)) {
  #what is the focal day
  day <-  visit_and_bird$obs_dt2[d]
  #get all info +/- windowsize days of focal day
  window <- subset(visit_and_bird, obs_dt2 >= day-windowsize & obs_dt2 <= day+windowsize)
  #calculate the reporting rate for all lists from this window
  smoothed_rep_rate[d] <- 100 * sum(window$nvisits_with_bird) / sum(window$nvisits)
}

#add smoothed values to new column on the dataframe
visit_and_bird$rep_rate_sm <- unlist(smoothed_rep_rate)

#and plot
ggplot() +
  geom_line(data = visit_and_bird, aes(x = obs_dt2, y = rep_rate), col= 'grey50') +
  geom_line(data = visit_and_bird, aes(x = obs_dt2, y = rep_rate_sm), col = 'red') +
  scale_y_continuous(limits = c(0,NA)) +
  labs(x = 'Date', y = 'Reporting rate', title = paste("Unsmoothed (grey) and smoothed (red) reporting rates for",focal_spp)) +
  theme_classic()
