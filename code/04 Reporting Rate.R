# uses the total amount of lists by month to get reporting rate for a bird species

source("code/00 Graphing Functions.R")
source("code/00 Helper Functions.R")


#' Create reporting rate dataframe
#' 
#' Given observations for a bird species and total complete lists made, creates reporting rate for the bird. 
#' 
#' Reporting rate is calculated by dividing absolute species numbers for a month by total lists for that month, giving a number between 0 and 1. Both lists are cleaned and sorted by descending order within the function.  
#' 
#' @param bird_list Dataframe with columns: month of observation ("month_of") and bird count for that month ("n").
#' @param total_lists Dataframe with columns: month of complete list made ("month_of") and total complete lists made ("n").
#' @return A Dataframe with columns: month of reporting rate ("month_of") and reporting rate for a bird species ("reporting_rate").
get_reportingrate_time <- function(bird_list, total_lists)  {
  # removing NAs and arranging by date
  bird_list <- bird_list %>% 
    filter(!is.na(month_of)) %>% 
    arrange(desc(month_of))
  
  # only contains months where bird was sighted
  # arranges by date in the same way the bird absolute numbers are
  patterns <- bird_list$month_of
  bird_sighted_months <- total_lists %>% 
    filter(grepl(paste(patterns,collapse = "|"), month_of)) %>% 
    arrange(desc(month_of))
  
  # divides absolute numbers by total lists for that month
  # assumes complete lists can only note a bird once
  reporting_rate_num <- bird_list[,"n"] / bird_sighted_months[,"n"]
  reporting_rate <- data.frame (
    month_of = bird_list$month_of,
    reporting_rate = reporting_rate_num
  )
}

#' Create reporting rate barchart
#' 
#' Takes reporting rate dataframe info and plots a month-rate barchart.
#' 
#' @param reporting_rate Dataframe made using create_reporting_rate() or with columns: month of rate ("month_of") and reporting rate ("reporting_rate"). 
#' @param bird_name Name of the bird species that reporting rate is for.
#' @param location Location for a reporting rate.
#' @return Barchart with month on the x-axis and rate on the y-axis.
plot_reportingrate_barchart <- function(reporting_rate, bird_name, location="TL") {
  earliest <- tail(reporting_rate, 1)$month_of
  latest <- head(reporting_rate, 1)$month_of
  print(typeof)
  title <- paste("Reporting rate for",bird_name,"in",location)
  chart <- plot_barchart(reporting_rate,earliest,latest, 
                         title,"Reporting Rate")
}

complete_lists <- read_csv("results/lists_by_month.csv")
bird <- read_csv("data_temp/cettiwarbler.csv")
reporting_rate <- get_reportingrate_time(bird,complete_lists)
chart <- plot_reportingrate_barchart(reporting_rate, "Cetti's Warbler")
chart

ggsave("results/cettiswarbler_rate.png", chart, width= 10, height=7)



test <- plot_yearly_linechart(reporting_rate, "month_of", "Reporting rate for Cetti's Warbler", "Reporting rate" )
test
ggsave("results/cettiwarbler_rate_yearly.png", test, width= 10, height=7)
# TODO: test on different bird datas
