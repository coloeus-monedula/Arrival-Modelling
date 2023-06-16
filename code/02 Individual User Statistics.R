# check usercode exists
check_usercode_exists <- function(user_list,usercode) {
  if (usercode %in% user_list$user_code) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# given a usercode, returns date of first list made, date of most recent list made, and total amount of lists
get_summary_info <- function(user_list, usercode) {
  if (!check_usercode_exists(user_list, usercode)) {
    print("Invalid usercode entered")
    return(NA)
  }
  
  single_user <- user_list %>% 
    filter(user_code == usercode)
  
  summary <- single_user %>% 
    summarise(earliest_list = min(date), latest_list = max(date), n = nrow(single_user))
  
  return(summary)
}


# given a usercode, returns a dataframe with the number of lists per month user has made
get_monthly_lists <- function(user_list, usercode) {
  if (!check_usercode_exists(user_list, usercode)) {
    print("Invalid usercode entered")
    return(NA)
  }
  
  single_user <- user_list %>% 
    filter(user_code == usercode) %>% 
    arrange(desc(date))
  
  summary_info <- get_user_summary(user_list, usercode)
  # finds months between first bird list and last bird list, rounded up
  total_months <- as.integer(ceiling(time_length(
    interval(summary_info$earliest_list,summary_info$latest_list), unit="month")))
  
  # init new dataframe
  # amount of rows = maximum possible months
  month_list_count <- data.frame(month_of = rep(NA, total_months), n = rep(0, total_months))
  # change to date format
  month_list_count$month_of <- dmy(month_list_count$month_of)
  
  # counter for adding to month list - tracks latest index
  counter <- 1
  for (i in 1:nrow(single_user)) {
    floored_date <- floor_date(single_user[i,]$date, unit = "month")
    
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

# given a dataframe with month-count numbers, plots a bar chart 
# TODO: optional params - zoom into specific date ranges
plot_barchart <- function(month_list_count, usercode) {
  barchart <- ggplot(data = month_list_count, aes(x = month_of, y=n)) +
    geom_col() +
    labs(title = "Number of complete lists made in [insert usercode] by month",
         x = "Date by month",
         y= "Number of lists") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle=270)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_date(date_labels = "%b '%y", 
                 breaks = seq(ymd("2017/01/01"), ymd("2022/12/01"), by = "month"), 
                 limits=ymd(c("2016/12/01", "2023/01/01")),
                 expand = c(0, 0))
}

user <- read_csv("data_temp/user_data.csv")

(get_summary_info(user, "U7773"))
month_count <-  get_monthly_lists(user, "U7773")
chart <- plot_barchart(month_count, "U7773")
chart
