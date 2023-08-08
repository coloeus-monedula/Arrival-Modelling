library(readr)
library(dplyr)
library(lubridate)
library(roxygen2)
library(BTOTools)
library(tidyr)

source("code/00 Pipeline Functions.R")


# given 1/0 data returns condensed list of total list length(count) and 1/0 for focal species
get_presenceabsence_focal <- function(data, species, exclude = "latitude, longitude, focal, user_code, sub_code, grid_ref, date, tenkm"){
  
  data_longer <- pivot_longer(data = data, cols = !c(latitude, longitude, user_code, sub_code, grid_ref, date, tenkm), names_to = "code2ltr", values_to = "presence")

  # remove rows with value 0
  data_longer <- subset(data_longer, presence == 1)

  aggregated <- data_longer %>% 
    group_by(sub_code, date) %>% 
    summarise(count = n(), 
              #if the wanted species is observed in the list or not
              presence = ifelse(any(tolower(code2ltr) == tolower(species)),
                                1, 0)
    )
  
  return(aggregated)
    
}

#' Reading user data from source into a data frame
#' 
#' @param path Path where raw csv can be found
#' @param usercode Optional. Defaults to processing all users (ALL) but can process a single usercode.
#' Optional. Defaults to processing all locations (ALL) but can filter by the first two letters of a grid reference (eg. "TL").
#' @return A data frame with rows: user_code, sub_code, date (converted into Date), latitude, longitude, grid_ref
get_user_data <- function(path, usercode = "ALL", tenkm_area="ALL", year="ALL") {
  raw <- read_csv(path)

  # convert dates to date format
  raw$date <- dmy(raw$obs_dt)
  
  
  #optional filter by usercode
  if (usercode!="ALL") {
    if (!check_id_exists(data_list = raw, id_code = usercode)) {
      print("Usercode not found. Getting data for all users instead. ")
      user_lists <- raw
    } else {
      user_lists <- raw %>% 
        filter(tolower(user_code) == tolower(usercode))
    }
  } else {
    user_lists <- raw
  }
  
  if (year !="ALL") {
    user_lists <-  user_lists %>% 
      filter(year(date) == year)
  }
  
  #optional filter by 10km grid ref - do last as it involves adding 10km grid refs
  if (tenkm_area !="ALL") {
    user_lists <- add_10km_gridref(user_lists, "grid_ref") %>% 
      filter(tolower(tenkm) == tolower(tenkm_area))
  }
  
  
  # map users to lists
  user_lists <- user_lists %>% 
    select(user_code, sub_code, date, latitude, longitude, grid_ref) %>% 
    distinct() %>% 
    arrange(date)
  
  return(user_lists)
}


#' Reading bird data from source into a data frame
#' 
#' @param path Path where raw csv can be found
#' @param species Optional. Defaults to processing all birds (ALL) but can input the (english) name of a single species. 
#' @param tenkm_area Optional. Defaults to processing all locations (ALL) but can filter by the tenkm reference.
#' @return A data frame with rows: sub_code, english_name, scientific_name, grid_ref, longitude, latitude, date (converted into Date)
get_bird_data <- function(path, species="ALL", tenkm_area="ALL", year="ALL") {
  raw <- read_csv(path)
  
  # convert dates to date format
  raw$date <- dmy(raw$obs_dt)
  
  #optional filter by species eng name
  if (species !="ALL") {
    if (!check_id_exists(data_list = raw, id_code = species, is_bird = TRUE)) {
      print("Species name not found. Getting data for all species instead.")
      birds <- raw
    } else {
      birds <- raw %>% 
        filter(tolower(english_name) == tolower(species))
    }
  } else {
    birds <- raw
  }
  
  if (year !="ALL") {
    birds <-  birds %>% 
      filter(year(date) == year)
  }
  
  # optional filter by grid ref
  #optional filter by 10km grid ref - do last as it involves adding 10km grid refs
  if (tenkm_area !="ALL") {
    birds <- add_10km_gridref(birds, "grid_ref") %>% 
      filter(tolower(tenkm) == tolower(tenkm_area))
  }
  
  
  birds <- birds %>% 
    select(sub_code, english_name, scientific_name, grid_ref, longitude, latitude, date) %>% 
    arrange(date)
  return(birds)
}

#' Check usercode or bird name exists 
#'
#' Checking function often used inside other functions to check for validity. Case-insensitive.
#'
#' @param data_list pre-processed dataset.
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



#summed moving window lists that increment day by day (at the moment)
get_movingwindow_daylists <- function(data_list, period,id_code="ALL", is_bird = FALSE) {
  if (id_code!="ALL" && !check_id_exists(data_list, id_code, is_bird)) {
    return(NA)
  }
  
  if (id_code == "ALL"){ 
    # just uses all the data
    single_id_list <- data_list
  }
  else if (is_bird) {
    single_id_list <- data_list %>% 
      filter(tolower(english_name) == tolower(id_code))
  } 
  else {
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


# function that calculates unsmoothed and smoothed reporting rates together
# expects single bird list and number of lists done per day already done
# note: window number is for each side
get_comparison_reportingrates <- function(bird_data, list_count, window = 5) {
  # rename the n cols
  bird_data <- bird_data %>% 
    rename(bird_n=n)
  
  list_count <- list_count %>% 
    rename(lists_n = n)
  
  # merge by date
  merged <- merge(list_count, bird_data, by="interval_of", all.x = TRUE)
  # fill with 0s for lists done with no birds
  merged[is.na(merged)] <- 0
  
  # then divide two cols
  merged$report_rate <- 100*merged$bird_n / merged$lists_n
  
  #the initial focal point's index 
  init_focal_index <- window
  end_focal_index <- nrow(merged) - init_focal_index
  
  
  # create new col
  merged$smoothed_report_rate <- NA
  
  
  for (d in init_focal_index:end_focal_index) {
    focal_day <- merged$interval_of[d]
    #get all info +/- window days of focal day
    window_data <- subset(merged, interval_of >= focal_day-window & interval_of <=focal_day+window)
    # calculate smoothed reporting rate for this window
    merged$smoothed_report_rate[d] <- 100 * sum(window_data$bird_n) / sum(window_data$lists_n)
    
  }
  
  return(merged)
}




#' Create reporting rate dataframe
#' 
#' Given number of observations for a bird species and total complete lists made, creates reporting rate for the bird. 
#' 
#' Reporting rate is calculated by dividing absolute species numbers for an interval by total lists for that interval, giving a percentage between 0 and 100. Both lists are cleaned and sorted by descending order within the function.  
#' 
#' @param bird_list Dataframe with columns: interval of observation and bird count for that period ("n").
#' @param total_lists Dataframe with columns: intervals of complete list made and total complete lists made ("n").
#' @param invar Name of column with time intervals. Must be the same for both bird_list and total_lists.
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
  reporting_rate_num <- (bird_list[,"n"] / bird_sighted_months[,"n"])*100
  reporting_rate <- data.frame (
    interval_of = bird_list[[invar]],
    reporting_rate = reporting_rate_num
  )
}



