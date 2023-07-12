# uses the total amount of lists by month to get reporting rate for a bird species

source("code/00 Graphing Functions.R")
source("code/00 Helper Functions.R")


#' Create reporting rate dataframe
#' 
#' Given observations for a bird species and total complete lists made, creates reporting rate for the bird. 
#' 
#' Reporting rate is calculated by dividing absolute species numbers for an interval by total lists for that interval, giving a number between 0 and 1. Both lists are cleaned and sorted by descending order within the function.  
#' 
#' @param bird_list Dataframe with columns: interval of observation and bird count for that month ("n").
#' @param total_lists Dataframe with columns: intervals of complete list made and total complete lists made ("n").
#' @param invar Name of column with intervals
#' @return A Dataframe with columns: intervals used in reporting rate ("interval_of") and reporting rate for a bird species ("reporting_rate").
get_reportingrate_time <- function(bird_list, total_lists,invar)  {
  # removing NAs and arranging by date
  bird_list <- bird_list %>% 
    filter(!is.na(bird_list[[invar]])) %>% 
    arrange(desc(.data[[invar]]))

  # only contains months where bird was sighted
  # arranges by date in the same way the bird absolute numbers are
  patterns <- bird_list[[invar]]
  
  bird_sighted_months <- total_lists %>% 
    filter(grepl(paste(patterns,collapse = "|"), .data[[invar]])) %>% 
    arrange(desc(.data[[invar]]))
  
  # divides absolute numbers by total lists for that month
  # assumes complete lists can only note a bird once
  reporting_rate_num <- bird_list[,"n"] / bird_sighted_months[,"n"]
  reporting_rate <- data.frame (
    interval_of = bird_list[[invar]],
    reporting_rate = reporting_rate_num
  )
}

#' Create reporting rate barchart
#' 
#' Takes reporting rate dataframe info and plots a month-rate barchart.
#' 
#' @param reporting_rate Dataframe made using create_reporting_rate() or with columns interval of rate and reporting rate (labelled "n"). 
#' @param invar Name of column with intervals
#' @param bird_name Name of the bird species that reporting rate is for.
#' @param location Location for a reporting rate.
#' @return Barchart with month on the x-axis and rate on the y-axis.
plot_reportingrate_barchart <- function(reporting_rate, invar, bird_name, location="TL") {
  earliest <- tail(reporting_rate, 1)[[invar]]
  latest <- head(reporting_rate, 1)[[invar]]
  title <- paste("Reporting rate for",bird_name,"in",location)
  chart <- plot_barchart(reporting_rate,invar,earliest,latest, 
                         title,"Reporting Rate")
}

complete_lists <- read_csv("results/lists_by_month.csv")
bird <- read_csv("data_temp/cettiwarbler.csv")
reporting_rate <- get_reportingrate_time(bird,complete_lists, "month_of")
chart <- plot_reportingrate_barchart(reporting_rate, "interval_of","Cetti's Warbler")
chart

# ggsave("results/cettiswarbler_rate.png", chart, width= 10, height=7)

# we trying w weekly and 10day
raw_user <- read_csv("data_temp/user_data.csv")
birds <-  read_csv("data_temp/bird_lists.csv")
name <- "Cetti's Warbler"

weekly_birds <- get_interval_lists(birds,name,"week",TRUE)
tenday_birds <- get_interval_lists(birds, name, "10 day", TRUE)

weekly <- get_interval_lists(raw_user,"ALL","week")
tenday <- get_interval_lists(raw_user,"ALL","10 day")

reporting_rate_weekly <- get_reportingrate_time(weekly_birds, weekly, "interval_of")
reporting_rate_tenday <- get_reportingrate_time(tenday_birds, tenday, "interval_of")


chart2 <- plot_reportingrate_barchart(reporting_rate_tenday, "interval_of", "Cetti's Warbler")

chart2

test <- plot_yearly_linechart(reporting_rate_tenday, "interval_of", "Reporting rate for Cetti's Warbler", "Reporting rate" )
test
# ggsave("results/cettiwarbler_rate_yearly.png", test, width= 10, height=7)
# TODO: test on different bird datas
