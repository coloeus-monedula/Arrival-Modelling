library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(roxygen2)

#' Check usercode or bird name exists 
#'
#' Checking function often used inside other functions to check for validity. Case-insensitive.
#'
#' @param data_list raw dataset.
#' @param id_code Usercode or bird species (currently English name) to search for.
#' @param is_bird Boolean, default to FALSE. Set true if searching for a bird.
#' @return TRUE if identifier found, FALSE if not.
check_id_exists <- function(data_list,id_code,is_bird=FALSE) {
  if (is_bird) {
    if (tolower(id_code) %in% tolower(data_list$english_name)) {
      return(TRUE)
    } else {
      print("Invalid English bird name entered")
      return(FALSE)
    }
  } else {
    if (tolower(id_code) %in% tolower(data_list$user_code)) {
      return(TRUE)
    } else {
      print("Invalid usercode entered")
      return(FALSE)
    }
  }
}


#' Summarises the dataset for a usercode or bird species
#' 
#' Displays total amount of lists made bird observations for a given user/bird, as well as earliest list/sighting and latest list/sighting.
#' @param data_list dataset
#' @param id_code Usercode or bird species (currently English name) to search for. Case insensitive.
#' @param is_bird Set to FALSE by default. Set TRUE to search for bird species.
#' @return Summary list of earliest_date, latest_date, and n. 
get_summary_info <- function(data_list, id_code, is_bird=FALSE) {
  if (!check_id_exists(data_list, id_code, is_bird)) {
    return(NA)
  }
  
  if (is_bird) {
    single_id_list <- data_list %>% 
      filter(tolower(english_name) == tolower(id_code))
  } else {
    single_id_list <- data_list %>% 
      filter(tolower(user_code) == tolower(id_code))
  }
  
  summary <- single_id_list %>% 
    summarise(earliest_date = min(date), latest_date = max(date), n = nrow(single_id_list))
  
  return(summary)
}


#' Get observations/list counts by month
#' 
#' For a given user/bird, returns the monthly observations or complete lists made.
#' @param data_list raw dataset
#' @param id_code Usercode or bird species (currently English name) to search for. Case insensitive.
#' @param is_bird Set to FALSE by default. Set TRUE to search for bird species.
#' @return Dataframe with columns: month_of and n (where n is count).
get_monthly_lists <- function(data_list, id_code, is_bird=FALSE) {
  if (!check_id_exists(data_list, id_code, is_bird)) {
    return(NA)
  }
  
  if (is_bird) {
    single_id_list <- data_list %>% 
      filter(tolower(english_name) == tolower(id_code))
  } else {
    single_id_list <- data_list %>% 
      filter(tolower(user_code) == tolower(id_code))
  }
  
  summary_info <- get_summary_info(data_list, id_code, is_bird)
  # finds months between first bird list and last bird list, rounded up
  total_months <- as.integer(ceiling(time_length(
    interval(summary_info$earliest_date,summary_info$latest_date), unit="month")))
  
  # init new dataframe
  # amount of rows = maximum possible months
  month_list_count <- data.frame(month_of = rep(NA, total_months), n = rep(0, total_months))
  # change to date format
  month_list_count$month_of <- dmy(month_list_count$month_of)
  
  # counter for adding to month list - tracks latest index
  counter <- 1
  for (i in 1:nrow(single_id_list)) {
    floored_date <- floor_date(single_id_list[i,]$date, unit = "month")
    
    if(floored_date %in% month_list_count$month_of) {
      month_list_count <- month_list_count %>%
        mutate(n=ifelse(month_of==floored_date,n+1,n))
    } else {
      month_list_count[counter,] <- c(month_of = floored_date, n = 1)
      counter <- counter + 1
    }
  }
  return(month_list_count)
  
}


#' Get month-count barchart
#' 
#' Plots a barchart with month on the x axis and absolute count of lists made/observations of bird on the y axis.
#' 
#' @param month_list_count Dataframe with month_of and n (n meaning count) for that month.
#' @param id_code Usercode/Bird species the graph is for
#' @param is_bird FALSE by default. Set to TRUE to plot bird species.
#' @return Barchart with month-count for a certain user/bird
plot_monthcount_barchart <- function(month_list_count, id_code, is_bird = FALSE) {
  earliest <- head(arrange(month_list_count, month_of), 1)$month_of
  # some rows will be NA since not all users do lists regularly
  latest <- head(arrange(month_list_count, desc(month_of)), 1)$month_of
  
  if (is_bird) {
    title <- paste("Number of observations for", id_code, "by month")
    y <-  paste("Number of observations")
  } else {
    title <-paste("Number of complete lists made by", id_code, "by month")
    y <- "Number of lists"
  }
  
  plot_barchart(month_list_count, earliest, latest, title, y)
  
}

#' Plot a barchart with month aggregate data
#' 
#' Called from other functions. Plots a barchart with month on the x axis and corresponding data on the y.
#' @param month_list_count Dataframe with month_of column and another column of data aggregated by month
#' @param earliest_date Earliest month_of row
#' @param latest_date Latest month_of row
#' @param title Title of barchart
#' @param y y axis label
#' @return Barchart of monthly data with specified titles and labels
plot_barchart <- function(month_list_count, earliest_date, latest_date, title, y) {
  barchart <- ggplot(data = month_list_count, aes(x = month_of, y=n)) +
    geom_col(width =15) +
    labs(title = title,
         x = "Date by month",
         y= y) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle=270)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_date(date_labels = "%b '%y", 
                 breaks = seq(earliest_date, latest_date, by = "month"), 
                 limits=ymd(c("2016/12/01", "2023/01/01")),
                 expand = c(0, 0))
}
