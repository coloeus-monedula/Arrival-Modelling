library(readr)
library(dplyr)
library(lubridate)

# cleans raw data
# returns data frame with unique rows: user_code, sub_code, date (converted into Date), latitude, longitude, grid_ref
get_user_data <- function(path) {
  raw <- read_csv(path)
  
  # remove dummy numbers
  raw <- select(.data = raw, -...1)
  # convert dates to date format
  raw$date <- dmy(raw$obs_dt)
  
  # map users to lists
  user_lists <- raw %>% 
    select(user_code, sub_code, date, latitude, longitude, grid_ref) %>% 
    distinct() %>% 
    arrange(date)
  
  
  return(user_lists)
}


#cleans raw data but for birds
#default is all birds, but can specify (by English name) bird species
#return sub_code, english_name, scientific_name, grid_ref, longitude, latitude, date (converted)
get_bird_data <- function(path, species="ALL") {
  raw <- read_csv(path)
  
  # remove dummy numbers
  raw <- select(.data = raw, -...1)
  # convert dates to date format
  raw$date <- dmy(raw$obs_dt)
  
  #optional filter by species eng name
  if (species !="ALL") {
    birds <- raw %>% 
      filter(tolower(english_name) == tolower(species))
  }
  
  birds <- birds %>% 
    select(sub_code, english_name, scientific_name, grid_ref, longitude, latitude, date) %>% 
    arrange(date)
  
  return(birds)
}