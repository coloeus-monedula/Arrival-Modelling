# uses the total amount of lists by month to get reporting rate for a bird species

source("code/00 Graphing Functions.R")
source("code/00 Helper Functions.R")




#' Create reporting rate barchart
#' 
#' Takes reporting rate dataframe info and plots a month-rate barchart.
#' 
#' @param reporting_rate Dataframe made using create_reporting_rate() or with columns interval of rate and reporting rate (by default labelled "n"). 
#' @param invar Name of column with intervals
#' @param bird_name Name of the bird species that reporting rate is for.
#' @param location Location for a reporting rate.
#' @return Barchart with month on the x-axis and rate on the y-axis.
plot_reportingrate_barchart <- function(reporting_rate, invar, bird_name, location="TL", y_col = "n") {
  earliest <- tail(reporting_rate, 1)[[invar]]
  latest <- head(reporting_rate, 1)[[invar]]
  title <- paste("Reporting rate for",bird_name,"in",location)
  chart <- plot_barchart(reporting_rate,invar,earliest,latest, 
                         title,"Reporting Rate", y_col)
}

# complete_lists <- read_csv("results/lists_by_month.csv")
# bird <- read_csv("data_temp/cettiwarbler.csv")
# reporting_rate <- get_reportingrate_time(bird,complete_lists, "month_of")
# chart <- plot_reportingrate_barchart(reporting_rate, "interval_of","Cetti's Warbler")
# chart

# ggsave("results/cettiswarbler_rate.png", chart, width= 10, height=7)

# we trying w weekly and 10day
raw_user <- get_user_data("data_in/RENEW_extract_TL.csv")
birds <-  get_bird_data("data_in/RENEW_extract_TL.csv")
gc()

name <- "Swallow"

# weekly_birds <- get_interval_lists(birds,name,"week",TRUE)
tenday_birds <- get_interval_lists(birds, interval = "10 day", id_code = name, is_bird = TRUE)

# weekly <- get_interval_lists(raw_user,"ALL","week")
tenday <- get_interval_lists(raw_user,interval = "10 day")

# reporting_rate_weekly <- get_reportingrate_time(weekly_birds, weekly, "interval_of")
reporting_rate_tenday <- get_reportingrate_time(tenday_birds, tenday, "interval_of")


chart2 <- plot_reportingrate_barchart(reporting_rate_tenday, "interval_of", "Cetti's Warbler")

chart2

# test <- plot_yearly_linechart(reporting_rate_tenday, "interval_of", "Reporting rate for Cetti's Warbler", "Reporting rate" )
# test
# 
# 
# cumulative_users_tenday <- get_movingwindow_daylists(raw_user, "ALL", "10 days")
# cumulative_swallow <- get_movingwindow_daylists(birds, name, "10 days", TRUE)
# 
# reporting_rate_cumulative <- get_reportingrate_time(cumulative_swallow, cumulative_users_tenday, "date")
# test2 <- plot_reportingrate_barchart(reporting_rate_cumulative, "interval_of", "Swallow")
# test2


# ggsave("results/cettiwarbler_rate_yearly.png", test, width= 10, height=7)

