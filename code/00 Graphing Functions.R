library(lubridate)
library(ggplot2)
library(scales)

#' Get interval-count barchart
#' 
#' Plots a barchart with interval on the x axis (date breaks split by month) and absolute count of lists made/observations of bird on the y axis.
#' 
#' @param interval_list_count Dataframe with interval and column by default called "n" (n meaning count) for that interval.
#' @param invar Column with intervals
#' @param interval Length of the interval (eg. week, month etc.)
#' @param id_code Usercode/Bird species the graph is for
#' @param is_bird FALSE by default. Set to TRUE to plot bird species.
#' @return Barchart with month-count for a certain user/bird
plot_interval_barchart <- function(interval_list_count,invar, interval, id_code, is_bird = FALSE, y_col = "n") {
  earliest <- head(arrange(interval_list_count, invar), 1)[[invar]]
  # some rows will be NA since not all users do lists regularly
  latest <- head(arrange(interval_list_count, desc(invar)), 1)[[invar]]
  
  if (is_bird) {
    title <- paste("Number of observations for", id_code, "by", interval, "intervals")
    y_lab <-  paste("Number of observations")
  } else {
    title <-paste("Number of complete lists made by", id_code, "by", interval, "intervals")
    y_lab <- "Number of lists"
  }
  
  plot_barchart(interval_list_count,invar,earliest, latest, title, y_lab, y_col = y_col)
  
}

#' Plot a barchart with interval aggregate data
#' 
#' Often called from other functions. Plots a barchart with month on the x axis and corresponding data on the y.
#' @param time_aggregates Dataframe with interval column and another column of data aggregated by an interval (called "n" by default)
#' @param invar Column with date data
#' @param earliest_date Earliest interval row
#' @param latest_date Latest interval row
#' @param title Title of barchart
#' @param y y axis label
#' @return Barchart of data (broken by month) with specified titles and labels
plot_barchart <- function(time_aggregates, invar, earliest_date, latest_date, title, y_lab, y_col) {
  
  barchart <- ggplot(data = time_aggregates, aes(x = .data[[invar]], y=.data[[y_col]])) +
    geom_col() +
    labs(title = title,
         x = "Date by month",
         y= y_lab) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle=270)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_date(date_labels = "%b '%y", 
                 date_breaks ="month", 
                 expand = c(0, 0))
}


plot_yearly_linechart <- function(time_aggregates, invar, title, y_lab, y_col = "n") {

  #add year and monthday cols
  time_aggregates <-  time_aggregates %>% 
    mutate(year = year(time_aggregates[[invar]]), monthday = format(as.Date(time_aggregates[[invar]]), "%m-%d")) %>% 
    arrange(year)
  
  # turn into factor so graph recognises year as discrete
  time_aggregates$year <- factor(time_aggregates$year)

  linechart <- ggplot(data = time_aggregates, aes(x=mdy(monthday, truncated=1L), y=.data[[y_col]], group=year, colour=year)) +
    geom_line(linewidth=1) +
    labs(title=title, x="Month", y=y_lab, colour="Year") +
    scale_x_date(date_labels = "%b") #just show month
  
}

