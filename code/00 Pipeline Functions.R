library(readr)
library(dplyr)
library(lubridate)
library(roxygen2)
library(BTOTools)
library(tidyr)
library(mgcv)
library(geodata)

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


# takes filtered dataset
predict_arrival <- function(bird, day_k = 20, count_k = 10, zero_threshold = 0.00001, small_peak_threshold = 0.1) {
  # convert to numerical day of the year
  bird$day <- yday(bird$date)
  
  gam_bird <- gam(presence~s(day, k=day_k) + s(count, k=count_k), method="REML",family = "binomial", data = bird) 
  
  x_count <- median(bird$count)
  
  # trying to predict values
  x_day <- seq(from=0, to=365, by=0.25)
  
  y_pred <- predict(gam_bird, data.frame(day=x_day, count=x_count), type="response")
  #get rid of minute fluctuations
  y_pred <- ifelse(y_pred < zero_threshold, 0, y_pred)
  
  predicted <- data.frame(day = x_day, rate = y_pred)
  
  diffs <- diff(predicted$rate)
  #can't have diff for first value
  predicted$diff <- c(NA, diffs)
  
  max_rate <- max(predicted$rate)
  
  #when doing calculations ignore the initial downwards slope if any
  #also cuts out flat gradient ie. = 0
  first_positive <- which(predicted$diff>0)[1]
  sliced <- slice(predicted, first_positive:n())
  
  # change from increasing rate to just starting to decrease
  # also ignores small peaks
  change <- which(sliced$diff < 0 & sliced$rate > small_peak_threshold*max_rate)[1]
  first_peak <- sliced[change,]
  
  #account for increased baseline in case of eg. overwintering species
  # ten percent taken using the difference between first peak and minimum rate
  ten_percent <- (first_peak$rate-min(predicted$rate)) * 0.1 + min(predicted$rate)
  #account for edge case
  arrival_start <- sliced[which(sliced$rate >= ten_percent)[1],]
  
  arrival_date <- as.Date(arrival_start$day-1, origin = ymd(year, truncated=2))
  
  #arrival start in num, arrival start as date, list num, detection count
  results <- list(arrival_start$day, arrival_date, nrow(bird), sum(bird$presence))
  return(results)
}

# fetches shapefile of the uk (default level = 1) from gadm and crops to specified boundaries
# recommended to gc() afterwards if low memory
# assumes uk 27700 mapping
get_basemap <- function(xmin, xmax, ymin, ymax, margin=0, level = 1) {
  # dataset from 
  # https://gadm.org/download_country.html for UK specifically
  uk <- gadm(country = "United Kingdom", path = "data_in/", level=level)
  uk_sp <- st_as_sf(uk)
  uk_sp <- st_transform(uk_sp, crs = 27700)
  
  bbox <- c(xmin = xmin-margin, xmax = xmax+margin , ymin = ymin-margin, ymax = ymax+margin)
  
  uk_sp_crop <- st_crop(uk_sp, bbox)
  return(uk_sp_crop)
}


# given min and max centroid eastings and northings, and how big a square is, produces a dataframe with all easting and northings centroids
generate_prediction_coords <- function(min_easting, min_northing, max_easting, max_northing, square_size) {
  
  # add +1 since endpoints are inclusive
  num_coords_east <- round((max_easting - min_easting)/square_size) + 1
  num_coords_north <- round((max_northing - min_northing)/square_size) + 1
  total_coords <- num_coords_east * num_coords_north
  
  coords <- data.frame(easting = rep(NA, total_coords), northing = rep(NA, total_coords))
  easting <- min_easting
  northing <- min_northing
  index <- 1
  
  for (i in 1:num_coords_east) {
    
    for (j in 1: num_coords_north){
      coord <- c(easting,northing)
      coords[index,] <- coord
      
      northing <- northing + square_size
      index <- index+1
    }
    easting <- easting + square_size
    # resets northing
    northing <- min_northing
  }
  
  # add gridref for readability
  coords <- coordinates_to_gridref(coords, invar_e = "easting", invar_n = "northing",output_res = square_size/1000,region = "GB")
  
  return(coords)
  
}

