source("code/00 * Pipeline Functions.R")


### This is a script that contains the pipeline for generating arrival date estimates for a certain species and year throughout the UK. This can be calculated at the 10km, 20km or 50km grid reference level, and outputs 3 graphs: a heatmap of arrival date estimates created from the data, a heatmap of detection numbers for each valid grid reference, and a smoothed arrival date heatmap.

### Inputs
year <-  2022
species <-  "CC"
# 10000, 20000, or 50000m
square_size <- 10000

## thresholds
#minimum list threshold
min_threshold = 100
# minimum lists per month threshold
min_month_threshold = 4
# minumum species detections
min_detections = 25
#for precleaning
n_threshold = 1

# detail for the basemap - 1, 2, or 3
# more detail (high number) increases load
map_level = 1
# shows inside each square estimated arrival date and indicator of data volume (detection / list count). not recommended for smaller squares/maps that cover a large area. 
show_text = FALSE
# outputs text during threshold checking - useful for seeing why a grid square was rejected
verbose = TRUE


### Getting presence absence data for species
# species_list <- get_presenceabsence_data(path = "data_in/RENEW_extract.csv", square_size = square_size, species = species, year = year)
# gc()
# species_list <- clean_data(species_list, n_threshold)

# note: for bigger datasets, recommended to gather presence absence data, write it to csv and then reread it again - for some reason it runs faster
# write_csv(species_list, "data_temp/chiffchaff_2022_10km.csv")
species_list <-  read_csv("data_temp/chiffchaff_2022_10km.csv")

### Running the GAM model for each grid reference
squares_list <- sort(unique(species_list$grid_ref))


# empty dataframe for storing data
# only stores results where a GAM has been applied
gam_results <- data.frame(grid_ref = rep(NA, length(squares_list)), arrival_start=rep(NA, length(squares_list)), arrival_date=rep(NA, length(squares_list)), total_lists=rep(NA, length(squares_list)), total_detections=rep(NA, length(squares_list)))
# store arrival date in date format
gam_results$arrival_date <- dmy(gam_results$arrival_date)

# fits GAM and predicts date for all grid references that pass the threshold
counter <-  1
for (square_area in squares_list) {
  
  if (check_valid_thresholds(species_list, square_area, min_threshold, min_month_threshold, min_detections, verbose=verbose)) {
    
    #filters by grid ref
    filtered <- species_list %>% 
      filter(grid_ref == square_area)
    
    results <- predict_arrival(filtered)
    
    # if arrival date can be calculated
    if (results$completed) {
      #removes completed flag
      results <- results[names(results) !="completed"]
      
      results <- c(square_area, results)
      gam_results[counter,] <- results
      counter <- counter+1
    } 
    
  } 
  
}
# removes excess na rows
gam_results <- gam_results[rowSums(is.na(gam_results)) != ncol(gam_results),]


### Generating geospatial features for plotting

# Adding easting and northings depending on square size
if (square_size == 10000){
  gam_results <- gridref_to_coordinates(gam_results, "grid_ref")
} else if (square_size == 20000) {
  gam_results <- merge(gam_results, centroids020, by.x="grid_ref", by.y="segref")
} else if (square_size == 50000) {
  gam_results <- merge(gam_results, centroids050, by.x="grid_ref", by.y="quadref")
}

# Creating the square grid
min_easting <- min(gam_results$easting)
max_easting <- max(gam_results$easting)
min_northing <- min(gam_results$northing)
max_northing <- max(gam_results$northing)

grid <- get_grid(min_easting, max_easting, min_northing, max_northing, square_size)

# adds geometry of squares to results for plotting
gam_results <- merge(gam_results, select(grid, c("grid_ref","geometry")), by="grid_ref")
# add text for optional labels inside squares
gam_results$label <-  paste(format(as.Date(gam_results$arrival_date), "%m-%d"), "\n", gam_results$total_detections,"/",gam_results$total_lists)

# Creating basemap
basemap <- get_basemap(xmin = min_easting, xmax = max_easting, ymin=min_northing, ymax = max_northing, margin=square_size, level = map_level)
gc()


### Plotting first stage GAM graphs
grid_plot <- st_transform(grid, crs = 4326)
results_plot <- st_sf(gam_results, crs=27700, sf_column_name = "geometry")
results_plot <- st_transform(results_plot, crs=4326)
basemap_plot <- st_transform(basemap, crs=4326)

# two graphs - one shows arrival date spread and potential outliers
# the other shows number of detections used to calculate each arrival date estimate
coverage_date <- plot_coverage_date(basemap_plot, grid_plot, results_plot, show_text = show_text)
print(coverage_date)
coverage_detections <- plot_coverage_detections(basemap_plot, grid_plot, results_plot, show_text = show_text)
print(coverage_detections)


### Second Stage Smoothing
smoothing_gam <-  gam(arrival_start ~ s(easting, northing, bs="tp", k = 20), method = "REML", data = gam_results)

# could also make own grid with get_grid() if doing localised smoothing - just need to input starting gridref and end gridref easting and northings
predictions <- grid %>% 
  rename(easting = centroid_x, northing=centroid_y)

predictions$predicted <- predict(smoothing_gam, predictions, type="response")
predictions$arrival_date <- as.Date(predictions$predicted, origin = ymd(year, truncated=2))

coverage_date_smoothed <- plot_coverage_date(basemap_plot, grid_plot, predictions, show_text = show_text)
print(coverage_date_smoothed)



