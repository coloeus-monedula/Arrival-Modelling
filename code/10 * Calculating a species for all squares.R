# pipeline steps. 
# FOR ALL: MOVE TO "00 PIPELINE FUNCTIONS, SOURCE THAT IN HELPER FUNCS, AND ALLOW FOR VERBOSE OUTPUT"
# 1. read in presence absence data for specified species and year [x]
# 2. get list of all squares in presence absence data. for every square, filter to that and run it through the threshold checks [x]
# 3. if the threshold check passes, for each square run the GAM model. return arrival day + arrival date + volume of data metrics (total lists, total detections) [x]
# 4. plot on geospatial map. adjust threshold as needed. [x]
# 5. refactor code so it doesn't assume tenkm is always the square size []
# 5. second stage smoothing. []
# 


source("code/00 Pipeline Functions.R")
library("sf")
library("scales")
library("ggplot2")


# input data
year <-  2022
species <-  "SL"

# 10000, 20000, or 50000m
square_size <- 10000

# thresholds
min_threshold = 100
min_month_threshold = 4
min_detections = 25

# detail for the basemap
# more detail makes the basemap much larger though
map_level = 1


# species_list <- get_presenceabsence_data(path = "data_in/RENEW_extract_TL.csv", species = species, year = year)
# gc()

# this is whole species data for swallow 2022
# write_csv(x = species_list, file = "data_temp/swallow_2022.csv")
species_list <- read_csv(file = "data_temp/swallow_2022.csv")

squares_list <- sort(unique(species_list$tenkm))

# empty dataframe for storing data
# only stores results where a GAM has been applied
gam_results <- data.frame(tenkm = rep(NA, length(squares_list)), arrival_start=rep(NA, length(squares_list)), arrival_date=rep(NA, length(squares_list)), total_lists=rep(NA, length(squares_list)), total_detections=rep(NA, length(squares_list)))

gam_results$arrival_date <- dmy(gam_results$arrival_date)


# fits GAM and predicts date for all grid references that pass the threshold
counter <-  1
for (square_area in squares_list) {
  if (check_valid_thresholds(species_list, square_area, min_threshold, min_month_threshold, min_detections, verbose=TRUE)) {
    
    #filters by tenkm area
    filtered <- species_list %>% 
      filter(tenkm == square_area)
    
    results <- predict_arrival(filtered)
    results <- c(square_area, results)
    
    gam_results[counter,] <- results
    counter <- counter+1
  } 
  
}

# removes excess na rows
gam_results <- gam_results[rowSums(is.na(gam_results)) != ncol(gam_results),]


# ADDING ONTO THE GRID
# add eastings and northings
gam_results <- gridref_to_coordinates(gam_results, "tenkm")
min_long <- min(gam_results$easting)
max_long <- max(gam_results$easting)
min_lat <- min(gam_results$northing)
max_lat <- max(gam_results$northing)


grid_corners <- data.frame(lon = c(min_long, max_long), lat = c(min_lat, max_lat))
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

gam_results <- gam_results %>% 
  rename("grid_ref" = "tenkm")

# adds geometry of squares 
gam_results <- merge(gam_results, select(grid, c("grid_ref","geometry")), by="grid_ref")
# add text
gam_results$label <-  paste(format(as.Date(gam_results$arrival_date), "%m-%d"), "\n", gam_results$total_detections,"/",gam_results$total_lists)

basemap <- get_basemap(xmin = min_long, xmax = max_long, ymin=min_lat, ymax = max_lat, margin=square_size, level = map_level)
gc()


# PLOTTING
grid_plot <- st_transform(grid, crs = 4326)
results_plot <- st_sf(gam_results, crs=27700, sf_column_name = "geometry")
results_plot <- st_transform(results_plot, crs=4326)
basemap_plot <- st_transform(basemap, crs=4326)


coverage_base <- ggplot() +
  geom_sf(data=basemap_plot) +
  geom_sf(data = grid_plot, fill=NA,inherit.aes = FALSE)+
  labs(title=paste("Arrival date estimations for",species,"in",year), subtitle = "Text shows month-day date and (number of detections) / (total lists)", x = "Longitude", y="Latitude") 

# two graphs - one uses colour to show arrival date spread (also a sensecheck, to see if there are wildly inaccurate estimates)
# the other shows number of detections since the more detections the greater the accuracy chance
coverage_date <- coverage_base +
  geom_sf(data=results_plot, aes(fill=arrival_date)) +
  # geom_sf_text(data = results_plot, aes(label=label), size=2.8, col = "white") +
  scale_fill_date(breaks=breaks_pretty(n=6), date_labels = "%m-%d", limits = c(min(gam_results$arrival_date), max(gam_results$arrival_date)), name = "Arrival date") 

print(coverage_date)

coverage_n <- coverage_base +
  geom_sf(data=results_plot, aes(fill=total_detections)) +
  # geom_sf_text(data = results_plot, aes(label=label), size=2.8, col = "white") +
  scale_fill_gradient(breaks = breaks_pretty(n=6), low = "#f6c9bb", high = "#cc0000", name="Detections")

print(coverage_n)

ggsave(file = "results/swallow_2022.png", coverage_n, height = 13, width = 7.5)
ggsave(file = "results/swallow_2022_date.png", coverage_date, height = 13, width = 7.5)

write_csv(x = gam_results, file = "data_temp/gam_results_swallow_2022.csv")



