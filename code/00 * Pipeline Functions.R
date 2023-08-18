library(readr)
library(dplyr)
library(lubridate)
library(roxygen2)
library(BTOTools)
library(tidyr)
library(mgcv)
library(geodata)
library(sf)
library("scales")
library("ggplot2")

### Functions used in the pipeline.

#' Cleans presence-absence data
#'
#' Given a dataset already sorted into presence-absence lists, filters out lists with species length numbers below a specified threshold.
#' @param dataset Dataset to clean
#' @param n_threshold Threshold. If list length is equal to or below this threshold, the list is filtered out
#' @return Filtered dataset
#'
clean_data <- function(dataset, n_threshold) {
  dataset <- dataset %>% 
    filter(count > n_threshold)
  
  return(dataset)
}


#' Create a presence absence dataframe from a CSV
#' 
#' Takes a pathfile to raw csv data and summarises the data for a given year and grid reference in the form of a presence (1) or absence (0) tally for a species, with the option for the tally to be for all species or just for one focal species (in which case, a column with list length is added).
#' 
#' @param path filepath to raw csv. The csv should have columns called "obs_dt", "grid_ref", "english_name", "user_code", "sub_code", "latitude", "longitude".
#' @param square_size size of the intended grid reference to use in metres - either 10000 (10km), 20000 (20km), or 50000 (50km). 
#' @param grid_ref_in Optional filter by grid reference. Needs to be the same size specified in square_size. 
#' @param species Optional filter by 2 letter species code. If = "ALL", then returns 1/0 data for all species.
#' @param year Optional filter by year.
#' @return If species = "ALL", a dataframe where each row is a single complete list, with columns for every possible species and a 1 underneath a column to signify a species sighting, or 0 to signify no sighting. 
#' 
#' Otherwise, a dataframe is returned where each row is a single complete list, with a 'presence' column for 1/0 data for a specific species, a 'count' column to show the list length, and a 'grid_ref' column that is for the size specified in square_size. Eg. if square_size = 20000, then the grid reference would be in TL_M format.
get_presenceabsence_data <- function(path,square_size, grid_ref_in="ALL", species = "ALL", year="ALL") {
  raw <- read_csv(path)
  
  # convert dates to date format
  raw$date <- dmy(raw$obs_dt)
  
  filtered <- raw
  #optional filter by year 
  if (year!="ALL") {
    filtered <-  raw %>% 
      filter(year(date) == year)
  }
  
  filtered <- add_10km_gridref(filtered, "grid_ref")
  km_ref <- "tenkm"

  # adds respective sized grid references and changes km_ref to respective named column
  if (square_size == 20000) {
    filtered <- rescale_10km_to_20km(filtered, "tenkm")
    km_ref <- "segref"
  } else if (square_size == 50000) {
    filtered <- rescale_10km_to_50km(filtered, "tenkm")
    km_ref <- "quadref"
  }
  
  #optional filter by 10km/20km/50km grid ref 
  if (grid_ref_in!="ALL") {
      filtered <- filtered %>% 
        filter(tolower(.data[[km_ref]]) == tolower(grid_ref_in))
  } 
  
  
  filtered <- merge(filtered, global_species_lookup[c('english_name','code2ltr')], by="english_name")
  filtered$english_name <- NULL
  # remove rows with NA species code
  filtered <- filtered[!is.na(filtered$code2ltr),]
  
  # if species code = ALL, returns 1/0 data
  if (species=="ALL") {
    filtered$presence <- 1
    aggregated <- pivot_wider(data = filtered, id_cols = c(user_code, sub_code, grid_ref, latitude, longitude, .data[[km_ref]], date), names_from = code2ltr, values_from = presence, values_fill = 0)
    
  } else {
    # filter by species here - also add column to count total species in a subcode list
    aggregated <- filtered %>% 
      group_by(sub_code, date, .data[[km_ref]]) %>% 
      summarise(count = n(), 
                #if the wanted species is observed in the list or not
                presence = ifelse(any(tolower(code2ltr) == tolower(species)),
                                  1, 0)
      ) %>% rename("grid_ref" = {{km_ref}})
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
#' @param id_code Usercode or bird species (2 letter species code) to search for. Case insensitive. If id_code = "ALL", does not do any filtering.
#' @param is_bird Set to FALSE by default. Set TRUE to search for bird species.
#' @return Dataframe with columns: date and n (where n is count), sorted by date.
get_interval_lists <- function(data_list, interval,id_code = "ALL", is_bird=FALSE) {
  
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


#' Thresholding for GAM modelling
#' 
#' Checks each grid reference square for whether or not it passes specified thresholds - minimum number of lists, minimum number of detection of a species, and minimum lists per month.
#' 
#' @param dataset Presence-absence dataset for a specific species
#' @param square_area Grid reference size in metres (10000, 20000, 50000)
#' @param min_threshold Minimum amount of lists needed. If below threshold, square is rejected.
#' @param min_month_threshold Minimum amount of lists in a month needed. If below threshold, square is rejected
#' @param min_detections Minimum amount of lists with detection of species needed. If below threshold, square is rejected
#' @param verbose If TRUE, outputs text at each threshold (of why it passes or is rejected). Default set to FALSE.
#' @return TRUE if all thresholds have been passed, FALSE if not.
check_valid_thresholds <- function(dataset, square_area, min_threshold, min_month_threshold, min_detections, verbose = FALSE) {
  square_dataset <- dataset %>% 
    filter(tolower(grid_ref) == tolower(square_area))
  
  #check total lists
  total_lists <- nrow(square_dataset)
  if (total_lists < min_threshold) {
    if (verbose) {
      print(paste(square_area, "did not pass minimum list threshold of",min_threshold,"(had",total_lists,"instead)"))
    }
    return(FALSE)
  }
  else if (verbose) {
    print(paste("Number of total lists for",square_area,":",total_lists))
  }
  
  #check list coverage
  min_months <- get_interval_lists(square_dataset, "month") %>% 
    arrange(n) %>% 
    head(1)
  min_n <- min_months$n
  
  if (min_n < min_month_threshold) {
    if (verbose) {
      print(paste(square_area, "did not pass the minimum per month list threshold of",min_month_threshold,"(had",min_n,"instead)"))
    }
    return(FALSE)
  } else if (verbose) {
    print(paste("Minimum lists were found in month", min_months$date,"with a number of",min_months$n))
  }
  
  #check detection total
  detections <- sum(square_dataset$presence)
  if (detections < min_detections) {
    if (verbose) {
      print(paste(square_area,"did not pass the minimum species detection threshold of",min_detections,"(had",detections,"instead)"))
    }
    
    return(FALSE)
  } else if (verbose) {
    print(paste("Number of total detections for",square_area,":",detections))
  }
  
  return(TRUE)
  
  
}



#' Predicts estimated arrival date for a species, year and grid reference
#' 
#' Creates a GAM using presence-absence data for a specific species, year and grid reference. The model is then used to predict species curve throughout the year, which is then used to find first maximum and the date where the reporting rate is ten percent of the first maximum (this date is used as arrival date).
#' 
#' The equation used in the model is presence ~ day + count, and is binomial. The arrival date is calculated by taking the maximum rate - minimum reporting rate, then finding ten percent of that.
#' 
#' @param bird presence-absence dataset
#' @param day_k k value for the day smooth. Default is 20
#' @param count_k k value for the count smooth. Default is 10
#' @param zero_threshold Threshold that below which reporting rate numbers are ignored/turned to 0. Default is 0.00001
#' @param small_peak_threshold Threshold that when multiplied with the maximum reporting rate in the curve, any peaks below this value is ignored. Default set to 0.1 ie. 10% of the maximum reporting rate.
#' @param to_graph If TRUE, returns data for graphing of an individual square (first peak value, predicted curve, arrival start value, arrival date value). If FALSE, returns data for heatmapping of all squares (arrival start as a number, arrival start as date, species count, detection count). Default set to FALSE.
#' @return A list with a completed flag set to TRUE or FALSE, alongside relevant information depending on value of the to_graph flag.
predict_arrival <- function(bird, day_k = 20, count_k = 10, zero_threshold = 0.00001, small_peak_threshold = 0.1, to_graph= FALSE) {
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
  #maximum reporting rate in curve
  max_rate <- max(predicted$rate)
  
  #when doing calculations ignore the initial downwards slope if any
  #also cuts out flat gradient ie. = 0
  first_positive <- which(predicted$diff>0)[1]
  first_negative <- which(predicted$diff<0)[1]
  
  # checks to see if there is a significant enough peak in model by checking there is at least 1 positive and 1 negative gradient 
  if (is.na(first_positive) || is.na(first_negative)) {
    print(paste("Fitted GAM model for", bird$grid_ref[1],"has no peak"))
    
    if (to_graph) {
      return(list(completed = FALSE,  predicted = predicted))
    } else {
      return(list(completed = FALSE))
    }
  }
  
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
  
  
  #output for displaying on a graph - see Fitting GAM to Year Data code
  if (to_graph) {
    results <- list(completed = TRUE, arrival_start = arrival_start, arrival_date = arrival_date, first_peak = first_peak, predicted = predicted)
  } else {
    #arrival start in num, arrival start as date, species count, detection count
    results <- list(completed = TRUE, arrival_start$day, arrival_date, nrow(bird), sum(bird$presence))
    
  }
  return(results)
}


#' Get UK basemap for specified dimensions
#' 
#' Downloads an administrative boundaries shapefile from GADM (https://gadm.org/download_country.html) for the UK, then crops it to specified easting and northing boundaries. Ie. boundaries should be specified in 27700 mapping.
#' @param xmin Left boundary
#' @param xmax Right boundary
#' @param ymin Bottom boundary
#' @param ymax Top boundary
#' @param margin Optional extra margin to add (in metres) to each side. Default 0.
#' @param level Level of subdivision for map. Higher the number, more detailed the map, but the larger the file becomes. Default 1.
#' @return Cropped basemap as a spatial object
get_basemap <- function(xmin, xmax, ymin, ymax, margin=0, level = 1) {
  uk <- gadm(country = "United Kingdom", path = "data_in/", level=level)
  uk_sp <- st_as_sf(uk)
  uk_sp <- st_transform(uk_sp, crs = 27700)
  
  bbox <- c(xmin = xmin-margin, xmax = xmax+margin , ymin = ymin-margin, ymax = ymax+margin)
  
  uk_sp_crop <- st_crop(uk_sp, bbox)
  return(uk_sp_crop)
}


#' Create square grid for mapping
#' 
#' Given northing and easting boundaries and grid resolution, creates a square grid for spatial mapping
#' @param min_easting Minimum easting value
#' @param max_easting Maximum easting value
#' @param min_northing Minimum northing value
#' @param max_northing Maximum northing value
#' @param square_size Size of squares that should be produced in metres (10000, 20000, or 50000) 
get_grid <- function(min_easting, max_easting, min_northing, max_northing, square_size) {
  grid_corners <- data.frame(lon = c(min_easting, max_easting), lat = c(min_northing, max_northing))
  box <- st_polygon(
    list(
      cbind(
        grid_corners$lon[c(1,1,2,2,1)],
        grid_corners$lat[c(1,2,2,1,1)]
      )
    )
  )
  
  # eastings and northings are already in 27700 system
  box_coords <- st_sfc(box, crs=27700)
  
  # in metres
  grid <- create_grid_for_object(sp_object = box_coords, grid_resolution = square_size, region="GB", outvarname = "grid_ref")
  grid <- grid[!is.na(grid$grid_ref),]
  

}


#' Plot estimated arrival date heatmap
#' 
#' @param basemap Basemap
#' @param grid Square grid overlay
#' @param results GAM results with a Date column called arrival_date
#' @param show_text If set to TRUE, adds arrival date and volume of data indication (number of detections / total lists) inside each grid square. Use only when squares are large enough.
#' @return Map showing estimated arrival dates for a species, with a colour legend showing spread of estimated arrival dates
plot_coverage_date <- function(basemap, grid, results, show_text) {
  graph <- ggplot() +
  geom_sf(data=basemap) +
  geom_sf(data = grid, fill=NA,inherit.aes = FALSE)+
  labs(title=paste("Arrival date estimations for",species,"in",year), x = "Longitude", y="Latitude", subtitle = paste("Earliest estimated arrival =", min(results$arrival_date), ", Latest estimated arrival =", max(results$arrival_date))) +
  geom_sf(data=results, aes(fill=arrival_date), alpha=0.8) +
  scale_fill_date(breaks=breaks_pretty(n=6), date_labels = "%m-%d", limits = c(min(results$arrival_date), max(results$arrival_date)), name = "Arrival date")
  
  
  if (show_text) {
    graph <- graph +
      geom_sf_text(data = results, aes(label=label), size=2.8, col = "white") +
      labs(caption = "Text in squares shows month-day date and (number of detections) / (total lists)")
  }
  
  return(graph)
  
}

#' Plot estimated arrival date heatmap but with detection number as colour legend
#' 
#' Can only be used during first stage GAM modelling. Gives a clearer indication of number of detections used to calculate each arrival date estimate.
#' @param basemap Basemap
#' @param grid Square grid overlay
#' @param results GAM results with a Date column called arrival_date as well as a column called total_detections
#' @param show_text If set to TRUE, adds arrival date and volume of data indication (number of detections / total lists) inside each grid square. Use only when squares are large enough.
#' @return Map indicating squares that have had GAM models fitted on the data, as well as volume of data used for each model.
plot_coverage_detections <- function(basemap, grid, results, show_text = TRUE) {
  graph <- ggplot() +
  geom_sf(data=basemap) +
  geom_sf(data = grid, fill=NA,inherit.aes = FALSE)+
  labs(title=paste("Volume of data used to estimate arrival date for",species,"in",year), x = "Longitude", y="Latitude") +
  geom_sf(data=results, aes(fill=total_detections)) +
  scale_fill_gradient(breaks = breaks_pretty(n=6), low = "#f6c9bb", high = "#cc0000", name="Detections")
  
  
  if (show_text) {
    graph <- graph +
    geom_sf_text(data = results, aes(label=label), size=2.8, col = "white") +
    labs(subtitle = "Text in squares shows month-day date and (number of detections) / (total lists)")
  }
  
  return(graph)
}
