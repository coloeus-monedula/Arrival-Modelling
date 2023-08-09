# pipeline steps. 
# FOR ALL: MOVE TO "00 PIPELINE FUNCTIONS, SOURCE THAT IN HELPER FUNCS, AND ALLOW FOR VERBOSE OUTPUT"
# 1. read in presence absence data for specified species and year [x]
# 2. get list of all squares in presence absence data. for every square, filter to that and run it through the threshold checks [x]
# 3. if the threshold check passes, for each square run the GAM model. return arrival day + arrival date + volume of data metrics (total lists, total detections) [x]
# 4. plot on geospatial map. adjust threshold as needed. []
# 5. second stage smoothing. []
# 


source("code/00 Pipeline Functions.R")
library("ggmap")
library(sf)

# input data
year <-  2022
species <-  "SL"

# thresholds
min_threshold = 100
min_month_threshold = 4
min_detections = 25

# bounding box latlong - currently set to whole of UK 
left <- -8
bottom <- 50
right <-2
top <- 62
bbox <- c(left = left, bottom = bottom , right = right, top = top)

# ggmap(get_stamenmap(bbox, zoom= 7, maptype = "toner-lite"))

species_list <- get_presenceabsence_data(path = "data_in/RENEW_extract_TL.csv", species = species, year = year)
gc()

squares_list <- sort(unique(species_list$tenkm))

# empty dataframe for storing data
# only stores results where a GAM has been applied
gam_results <- data.frame(tenkm = rep(NA, length(squares_list)), arrival_start=rep(NA, length(squares_list)), arrival_date=rep(NA, length(squares_list)), total_lists=rep(NA, length(squares_list)), total_detections=rep(NA, length(squares_list)))

gam_results$arrival_date <- dmy(gam_results$arrival_date)

counter <-  1
for (tenkm_area in squares_list) {
  if (check_valid_thresholds(species_list, tenkm_area, min_threshold, min_month_threshold, min_detections, verbose=TRUE)) {
    
    #filters by tenkm area
    filtered <- species_list %>% 
      filter(tenkm == tenkm_area)
    
    # fits GAM and predicts date
    results <- predict_arrival(filtered)
    results <- c(tenkm_area, results)
    
    gam_results[counter,] <- results
    counter <- counter+1
  } 
  
}

# removes excess na rows
gam_results <- gam_results[rowSums(is.na(gam_results)) != ncol(gam_results),]

# add easting and northing
gam_results <- gridref_to_coordinates(gam_results, "tenkm")


grid_corners <- data.frame(lon = c(bottom, top), lat = c(left, right))
box <- st_polygon(
  list(
    cbind(
      grid_corners$lon[c(1,1,2,2,1)],
      grid_corners$lat[c(1,2,2,1,1)]
    )
  )
)

# initial coordinates is in latlong degrees ie. assume 4326
box_coords <- st_sfc(box, crs=4326)
# transform to make the grid
box_coords <- st_transform(box_coords, crs=27700)

# in metres
grid <- create_grid_for_object(sp_object = box_coords, grid_resolution = 10000, region="GB", outvarname = "grid_ref")
# removes NA gridref - TODO: seem to be all NAs?
grid <- grid[!is.na(grid$grid_ref),]

# GEOSPATIAL MAPPING:
#  1. get 10km grid to lay over basemap. []
#  2. merge gam results with grid. []
#  3. get basemap for whole of UK (by default, option to change dimensions if needed. corner params defined at beginning?). TODO: MAKE OWN SHAPEFILE? []
#  4. plot with ggplot. output date and data volume somehow []

# TODO: for later things need to generate plot of all tenkm squares in uk

