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


# TODO: functions for creating data_temp files?

# adds a 10km square reference to a raw dataset
# invar is the column which has the grid reference
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
  # need to convert to data.frame if using tibbles - there is some strangeness in the 1 to 10km func
  onekm <- rescale_1km_to_10km(as.data.frame(arranged[1:last_1km_row$row_num,]), "grid_ref")
  
  # pads the end of the tenkm list with NAs
  arranged <- cbind(arranged, tenkm=onekm$tenkm[seq(nrow(arranged))])
  
  # adding the rest for 2km and 10km
  arranged <- arranged %>% 
    mutate(
      tenkm = case_when(
        #10km - keep as is
        nchar(arranged[[invar]]) == 4 ~ grid_ref,
        #tetrads - just extract the first 4 letters to get 10km ref
        nchar(arranged[[invar]])== 5 ~ substring(invar, 1, 4),
        #keeping the 1km to 10km references
        TRUE ~tenkm
      )
    ) %>% 
    #removes row_num col
    select(-row_num) 
}
