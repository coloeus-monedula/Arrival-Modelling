source("code/00 Processing Raw Data.R")
source("code/00 Helper Functions.R")

# a set of functions that calculate unsmoothed and smoothed reporting rates together


# expects single bird list and number of lists done per day already done
# note: window number is for each side
get_comparison_reportingrates <- function(bird_data, list_count, window = 5) {
  # rename the n cols
  bird_data <- bird_data %>% 
    rename(bird_n=n)
  
  list_count <- list_count %>% 
    rename(lists_n = n)
  
  # merge by date
  merged <- merge(list_count, bird_data, by="interval_of", all.x = TRUE)
  # fill with 0s for lists done with no birds
  merged[is.na(merged)] <- 0
  
  # then divide two cols
  merged$report_rate <- 100*merged$bird_n / merged$lists_n
  
  #the initial focal point's index 
  init_focal_index <- window
  end_focal_index <- nrow(merged) - init_focal_index
  
  
  # create new col
  merged <- merged %>% 
    add_column(smoothed_report_rate = NA)
  
  
  for (d in init_focal_index:end_focal_index) {
    focal_day <- merged$interval_of[d]
    #get all info +/- window days of focal day
    window_data <- subset(merged, interval_of >= focal_day-window & interval_of <=focal_day+window)
    # calculate smoothed reporting rate for this window
    merged$smoothed_report_rate[d] <- 100 * sum(window_data$bird_n) / sum(window_data$lists_n)
    
  }
  
  return(merged)
}

name <- "Swallow"
bird_data <- get_bird_data("data_in/RENEW_extract_TL.csv")
user_data <- get_user_data("data_in/RENEW_extract_TL.csv")
swallow_count <- get_interval_lists(bird_data, "Swallow","day", TRUE)
user_count <- get_interval_lists(user_data, "ALL", "day")
reporting_rates <- get_comparison_reportingrates(swallow_count, user_count, window=10)


ggplot() +
  geom_line(data = reporting_rates, aes(x = interval_of, y = report_rate), col= 'grey50') +
  geom_line(data = reporting_rates, aes(x = interval_of, y = smoothed_report_rate), col = 'red') +
  scale_y_continuous(limits = c(0,NA)) +
  labs(x = 'Date', y = 'Reporting rate', title = paste("Unsmoothed (grey) and smoothed (red) reporting rates for",name)) +
  theme_classic()
