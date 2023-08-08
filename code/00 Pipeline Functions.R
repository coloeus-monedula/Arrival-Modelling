library(readr)
library(dplyr)
library(lubridate)
library(roxygen2)
library(BTOTools)
library(tidyr)
library(mgcv)


# Functions actually used in the arrival date pipeline.

# produces subcode, date, tenkm code, list length, presence/absence (1/0)
get_presenceabsence_data <- function(path, tenkm_area="ALL", species = "ALL", year="ALL") {
  raw <- read_csv(path)
  
  # convert dates to date format
  raw$date <- dmy(raw$obs_dt)
  
  #optional filter by year 
  if (year!="ALL") {
    filtered <-  raw %>% 
      filter(year(date) == year)
  }
  
  filtered <- add_10km_gridref(filtered, "grid_ref")
  #optional filter by 10km grid ref - do last as it involves adding 10km grid refs
  if (tenkm_area!="ALL") {
    filtered <- filtered %>% 
      filter(tolower(tenkm) == tolower(tenkm_area))
  } 
  
  filtered <- merge(filtered, global_species_lookup[c('english_name','code2ltr')], by="english_name")
  filtered$english_name <- NULL
  # remove rows with NA species code
  filtered <- filtered[!is.na(filtered$code2ltr),]
  
  # if species code = ALL, returns 1/0 data
  if (species=="ALL") {
    filtered$presence <- 1
    aggregated <- pivot_wider(data = filtered, id_cols = c(user_code, sub_code, grid_ref, latitude, longitude, tenkm, date), names_from = code2ltr, values_from = presence, values_fill = 0)
    
  } else {
    # filter by species here - also add column to count total species in a subcode list
    aggregated <- filtered %>% 
      group_by(sub_code, date, tenkm) %>% 
      summarise(count = n(), 
                #if the wanted species is observed in the list or not
                presence = ifelse(any(tolower(code2ltr) == tolower(species)),
                                  1, 0)
      )
  }
  
  
  return(aggregated)
}


#' Add a 10km grid reference to a dataframe
#' 
#' Given a dataframe with 1km, 2km, or 10km grid references for each row, adds to a new column the 10km grid reference that corresponds to each grid reference.
#' 
#' @param df Dataframe with grid references
#' @param invar Column name of the grid reference
#' @return Dataframe with a "tenkm" column containing the 10km grid reference for each row, with rows sorted by length of the original grid reference
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


#' Get observations/list counts by month, week, day, or 10 days
#' 
#' For a given user/bird, returns the interval observations or complete lists made. Note that "10 days" is more precisely splitting a month into approximate thirds: 1st - 10th, 11th - 20th, and 21st - end of the month. The input data can either be already filtered for a species/id code or be the whole dataset.
#' 
#' @param data_list dataset with "date" column in Date format. 
#' @param interval "month", "week", "day", or "10 days"/"10 day". How long each interval should be.
#' @param id_code Usercode or bird species (species code) to search for. Case insensitive. If id_code = "ALL", does not do any filtering.
#' @param is_bird Set to FALSE by default. Set TRUE to search for bird species.
#' @return Dataframe with columns: date and n (where n is count), sorted by date.
get_interval_lists <- function(data_list, interval,id_code = "ALL", is_bird=FALSE) {
  
  # TODO: check id exists code currently still uses english name so commented out for now
  # if (id_code!="ALL" && !check_id_exists(data_list, id_code, is_bird)) {
  #   return(NA)
  # }
  
  if (id_code == "ALL"){ 
    # just uses all the data
    single_id_list <- data_list
  } else if (is_bird) {
    single_id_list <- data_list %>% 
      filter(tolower(code2ltr) == tolower(id_code))
  } else {
    single_id_list <- data_list %>% 
      filter(tolower(user_code) == tolower(id_code))
  }
  
  # splits by stated intervals of time
  single_id_list$floored_date <- floor_date(single_id_list$date, unit = interval)
  
  is_ten_days = interval == "10 days" || interval=="10 day" 
  # catch cases where months have 31 days (31st day data get put on separate rows) and reassign to that month's bracket beginning the 21st
  single_id_list <-  single_id_list %>% 
    mutate(
      floored_date = case_when(
        (is_ten_days) & day(floored_date) == 31 ~ `day<-`(floored_date, 21),
        TRUE ~ floored_date          
      )
    ) 
  
  interval_list_count <- single_id_list %>% 
    group_by(floored_date) %>% 
    summarise(n = n()) %>% 
    rename(date = floored_date)
  
  
  return(interval_list_count)
  
}


#function to check if for a given dataset, the chosen tenkm area and year has enough lists and detections
check_valid_thresholds <- function(dataset, tenkm_area, min_threshold, min_month_threshold, min_detections, verbose = FALSE) {
  tenkm_dataset <- dataset %>% 
    filter(tolower(tenkm) == tolower(tenkm_area))
  
  #check total lists
  total_lists <- nrow(tenkm_dataset)
  if (total_lists < min_threshold) {
    if (verbose) {
      print(paste(tenkm_area, "did not pass minimum list threshold of",min_threshold,"(had",total_lists,"instead)"))
    }
    return(FALSE)
  }
  else if (verbose) {
    print(paste("Number of total lists for",tenkm_area,":",total_lists))
  }
  
  #check list coverage
  min_months <- get_interval_lists(tenkm_dataset, "month") %>% 
    arrange(n) %>% 
    head(1)
  min_n <- min_months$n
  
  if (min_n < min_month_threshold) {
    if (verbose) {
      print(paste(tenkm_area, "did not pass the minimum per month list threshold of",min_month_threshold,"(had",min_n,"instead)"))
    }
    return(FALSE)
  } else if (verbose) {
    print(paste("Minimum lists were found in month", min_months$date,"with a number of",min_months$n))
  }
  
  #check detection total
  detections <- sum(tenkm_dataset$presence)
  if (detections < min_detections) {
    if (verbose) {
      print(paste(tenkm_area,"did not pass the minimum species detection threshold of",min_detections,"(had",detections,"instead)"))
    }
    
    return(FALSE)
  } else if (verbose) {
    print(paste("Number of total detections for",tenkm_area,":",detections))
  }
  
  return(TRUE)
  
  
}


