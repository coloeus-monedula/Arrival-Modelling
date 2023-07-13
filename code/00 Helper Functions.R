library(readr)
library(dplyr)
library(lubridate)
library(roxygen2)
library(BTOTools)

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


#' Get observations/list counts by month, week, or 10 days
#' 
#' For a given user/bird, returns the interval observations or complete lists made. Note that "10 days" is more precisely splitting a month into approximate thirds: 1st - 10th, 11th - 20th, and 21st - end of the month.
#' @param data_list raw dataset with "date" column in Date format
#' @param id_code Usercode or bird species (currently English name) to search for. Case insensitive. If id_code = "ALL" and searching for users, includes all users.
#' @param interval "month", "week", or "10 days"/"10 day". How long each interval should be. 
#' @param is_bird Set to FALSE by default. Set TRUE to search for bird species.
#' @return Dataframe with columns: interval_of and n (where n is count).
get_interval_lists <- function(data_list, id_code, interval, is_bird=FALSE) {
  if (id_code!="ALL" && !check_id_exists(data_list, id_code, is_bird)) {
    return(NA)
  }
  
  if (is_bird) {
    single_id_list <- data_list %>% 
      filter(tolower(english_name) == tolower(id_code))
  } else if (id_code == "ALL"){ 
    # just uses all the data
    single_id_list <- data_list
  } else {
    single_id_list <- data_list %>% 
      filter(tolower(user_code) == tolower(id_code))
  }
  
  # splits by stated intervals of time
  single_id_list$floored_date <- floor_date(single_id_list$date, unit = interval)

  is_ten_days = interval == "10 days" || interval=="10 day" 
  # catch cases where months have 31 days (31st day data get put on separate rows) and reassign that month's bracket beginning the 21st
  single_id_list <-  single_id_list %>% 
    mutate(
      floored_date = case_when(
        (is_ten_days) & day(floored_date) == 31 ~ `day<-`(floored_date, 21),
        TRUE ~ floored_date          
        )
    ) 
  
  interval_list_count <- single_id_list %>% 
    count(floored_date) %>% 
    rename(interval_of = floored_date)
  
  
  return(interval_list_count)
  
}


#summed moving window lists that increment day by day (at the moment)
get_movingwindow_daylists <- function(data_list, id_code, period, is_bird = FALSE) {
  if (id_code!="ALL" && !check_id_exists(data_list, id_code, is_bird)) {
    return(NA)
  }
  
  if (is_bird) {
    single_id_list <- data_list %>% 
      filter(tolower(english_name) == tolower(id_code))
  } else if (id_code == "ALL"){ 
    # just uses all the data
    single_id_list <- data_list
  } else {
    single_id_list <- data_list %>% 
      filter(tolower(user_code) == tolower(id_code))
  }
  
  single_id_list <- single_id_list %>% 
    arrange(date)
  
  # define beginning and end of interval
  beginning <- head(single_id_list, 1)$date
  
  # -1 day because %within% is inclusive of enddate
  end <- beginning %m+% as.period(period) %m-% days(1)
  window <- interval(beginning, end)
  
  # get middle of moving window ie. what the summed value will be stored at
  middle <- int_end(window/2)
  middle <- as.Date(middle)
  final_middle <- tail(single_id_list, 1)$date %m-% as.period(period)
  total_rows <- final_middle - middle
  
  summed_list <- data.frame(date = rep(NA, total_rows), n=rep(0, total_rows))
  summed_list$date <- dmy(summed_list$date)
  
  for (i in 1:total_rows) {
    
    # use the %in% terminology - sum everything up, add to dataframe w date, move on
    interval_dates <- single_id_list %>% 
      filter(date %within% window)
    
    total <- nrow(interval_dates)
    summed_list[i,] <- c(date = middle, n = total)
    
    middle <- middle + days(1)
    window <- int_shift(window, days(1))
  }
  
  return (summed_list)

}

# adds a 10km square reference to a raw dataset
# invar is the column which has the grid reference
# returns in sorted by nchar order
add_10km_gridref <- function(df, invar) {
  arranged <- df %>% 
    arrange(desc(nchar(df[[invar]]))) %>% 
    mutate(
      # for slicing purposes
      row_num = row_number()
    )

  # find last index that is 1km reference
  # need to do this way because fetching row indexes dynamically in mutate and case_when doesn't work
  last_1km_row <- arranged %>% 
    filter(nchar(arranged[[invar]]) == 6) %>% 
    tail(n = 1) 
  
  # add 10km grid reference column for 1km
  onekm <- rescale_1km_to_10km(arranged[1:last_1km_row$row_num,], "grid_ref")
  
  # pads the end of the tenkm list with NAs
  arranged <- cbind(arranged, tenkm=onekm$tenkm[seq(nrow(arranged))])
  
  # adding the rest for 2km and 10km
  arranged <- arranged %>% 
    mutate(
      tenkm = case_when(
        #10km - keep as is
        nchar(arranged[[invar]]) == 4 ~ arranged[[invar]],
        #tetrads - just extract the first 4 letters to get 10km ref
        nchar(arranged[[invar]])== 5 ~ substring(arranged[[invar]], 1, 4),
        #keeping the 1km to 10km references
        TRUE ~tenkm
      )
    ) %>% 
    #removes row_num col
    select(-row_num) 
}



