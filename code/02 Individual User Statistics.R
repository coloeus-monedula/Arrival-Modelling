# check usercode or bird  exists
# todo: function that converts into camelcase
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

# given a id_code, 
# returns date of first list/obs made, date of most recent list/obs made, and total amount of lists/observations
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


# given a id_code, returns a dataframe with the number of lists per month user has made
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

# given a dataframe with month-count numbers, plots a bar chart 
# TODO: optional params - zoom into specific date ranges
plot_barchart <- function(month_list_count, id_code, is_bird = FALSE) {
  earliest <- head(month_list_count, 1)$month_of
  # some rows will be NA since not all users do lists regularly
  latest <- head(arrange(month_list_count, desc(month_of)), 1)$month_of
  
  if (is_bird) {
    title <- paste("Number of observations for", id_code, "by month")
    y <-  paste("Number of observations")
  } else {
    title <-paste("Number of complete lists made by", id_code, "by month")
    y <- "Number of lists"
  }
  
  barchart <- ggplot(data = month_list_count, aes(x = month_of, y=n)) +
    geom_col(width =15) +
    labs(title = title,
         x = "Date by month",
         y= y) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle=270)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_date(date_labels = "%b '%y", 
                 breaks = seq(earliest, latest, by = "month"), 
                 limits=ymd(c("2016/12/01", "2023/01/01")),
                 expand = c(0, 0))
}

# columns pre-selected data frames - could just do raw data and sort that way
user <- read_csv("data_temp/user_data.csv")
u_code <- "u10912"

birds <- read_csv("data_temp/bird_lists.csv")
bird_name <- "Cetti's Warbler"

# 
# (get_summary_info(user, u_code))
# month_count <-  get_monthly_lists(user, u_code)
# chart <- plot_barchart(month_count, u_code)
# chart

(get_summary_info(birds, bird_name, is_bird=TRUE))
month_count <-  get_monthly_lists(birds, bird_name, TRUE)
chart <- plot_barchart(month_count, bird_name, TRUE)
chart

write_csv(month_count, file = "data_temp/cettiwarber.csv")

ggsave("results/cettiswarbler.png", chart, width= 10, height=7)
