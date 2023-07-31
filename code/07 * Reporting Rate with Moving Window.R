source("code/00 Helper Functions.R")
library(ggplot2)




plot_reportingrate_comparison <- function(reporting_rates, invar, name, smooth_period, y_col="report_rate", y_col_smooth = "smoothed_report_rate") {
  graph <- ggplot() +
    geom_line(data = reporting_rates, aes(x = .data[[invar]], y = .data[[y_col]]), col= 'grey50') +
    geom_line(data = reporting_rates, aes(x = .data[[invar]], y = .data[[y_col_smooth]]), col = 'red') +
    scale_y_continuous(limits = c(0,NA)) +
    labs(x = 'Date', y = 'Reporting rate (%)', title = paste("Unsmoothed (grey) and smoothed (red) reporting rates for",name,"with a moving window of",smooth_period)) +
    theme_classic() + 
    scale_x_date(date_labels = "%b '%y", 
                 date_breaks ="1 month", 
                 expand = c(0, 0)) +
    theme(axis.text.x = element_text(angle=270))
}


name <- "Barn Owl"
bird_data <- get_bird_data("data_in/RENEW_extract_TL.csv")
user_data <- get_user_data("data_in/RENEW_extract_TL.csv")
gc()

bird_count <- get_interval_lists(bird_data, "day",name, TRUE)
user_count <- get_interval_lists(user_data, "day")
reporting_rates <- get_comparison_reportingrates(bird_count, user_count, window=10)

graph <- plot_reportingrate_comparison(reporting_rates, "interval_of", name, "10 days" )

graph
