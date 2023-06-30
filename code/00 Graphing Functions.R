library(lubridate)
library(ggplot2)
library(scales)

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
plot_barchart <- function(time_aggregates, earliest_date, latest_date, title, y) {
  barchart <- ggplot(data = time_aggregates, aes(x = month_of, y=n)) +
    geom_col(width =15) +
    labs(title = title,
         x = "Date by month",
         y= y) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle=270)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_date(date_labels = "%b '%y", 
                 date_breaks = "month", 
                 limits=NA,
                 expand = c(0, 0))
}


plot_yearly_linechart <- function(time_aggregates, invar, title, y) {

  #add year and month cols
  time_aggregates <-  time_aggregates %>% 
    mutate(year = year(time_aggregates[[invar]]), month = month(time_aggregates[[invar]], label=TRUE, abbr=TRUE)) %>% 
    arrange(year)

  num_years <- length(unique(time_aggregates$year))
  colours <- RColorBrewer::brewer.pal(num_years, "Spectral")
  
  years <-  unique(time_aggregates$year)
  # insert an additional year to get the latest year's range
  years[[length(years)+1]] <- years[[length(years)]] + 1
  linechart <- ggplot(data = time_aggregates, aes(x=month, y=n, group=year, colour=cut(year, breaks=years, right = FALSE))) +
    geom_line() +
    labs(title=title, x="Month", y=y, colour="Year") +
    scale_fill_brewer(name="Year", type="qual", palette = "Spectral") +
    theme_minimal()
  
}

