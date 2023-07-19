source("code/00 Helper Functions.R")
library(ggplot2)


# function that calculates unsmoothed and smoothed reporting rates together
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
  merged$smoothed_report_rate <- NA
  
  
  for (d in init_focal_index:end_focal_index) {
    focal_day <- merged$interval_of[d]
    #get all info +/- window days of focal day
    window_data <- subset(merged, interval_of >= focal_day-window & interval_of <=focal_day+window)
    # calculate smoothed reporting rate for this window
    merged$smoothed_report_rate[d] <- 100 * sum(window_data$bird_n) / sum(window_data$lists_n)
    
  }
  
  return(merged)
}


plot_reportingrate_comparison <- function(reporting_rates, invar, name, y_col="report_rate", y_col_smooth = "smoothed_report_rate") {
  graph <- ggplot() +
    geom_line(data = reporting_rates, aes(x = .data[[invar]], y = .data[[y_col]]), col= 'grey50') +
    geom_line(data = reporting_rates, aes(x = .data[[invar]], y = .data[[y_col_smooth]]), col = 'red') +
    scale_y_continuous(limits = c(0,NA)) +
    labs(x = 'Date', y = 'Reporting rate (%)', title = paste("Unsmoothed (grey) and smoothed (red) reporting rates for",name)) +
    theme_classic() + 
    scale_x_date(date_labels = "%b '%y", 
                 date_breaks ="1 month", 
                 expand = c(0, 0)) +
    theme(axis.text.x = element_text(angle=270))
}


name <- "Cuckoo"
bird_data <- get_bird_data("data_in/RENEW_extract_TL.csv")
user_data <- get_user_data("data_in/RENEW_extract_TL.csv")
swallow_count <- get_interval_lists(bird_data, "day","Swallow", TRUE)
user_count <- get_interval_lists(user_data, "day")
reporting_rates <- get_comparison_reportingrates(swallow_count, user_count, window=10)

graph <- plot_reportingrate_comparison(reporting_rates, "interval_of", "Cuckoo" )

graph
