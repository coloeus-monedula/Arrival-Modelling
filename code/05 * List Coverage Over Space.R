library(BTOTools)
library(readr)
library(dplyr)
library(ggmap)
library(sf)

source("code/00 Helper Functions.R")



# SELECTING SPECIFIC BIRD OBSERVATIONS
# ===========================================
# this is the OG input with most user data stripped out

# raw <- read_csv("data_temp/bird_lists.csv")
# 
# bird_obs <- raw %>%
#   filter(english_name==bird_name) %>%
#   select(grid_ref, longitude, latitude, date)
# 
# write_csv(bird_obs, paste("data_temp/",bird_name,"_obs.csv", sep=""))


# getting basemap dimensions from longlat data
get_basemap <- function(bird_obs, margin) {
  max_lat <- max(bird_obs$latitude)
  min_lat <- min(bird_obs$latitude)
  max_long <- max(bird_obs$longitude)
  min_long <- min(bird_obs$longitude)
  height <- max_lat - min_lat
  width <- max_long - min_long
  
  map_box <- c(bottom = min_lat - margin*height, left = min_long - margin*width,
               top = max_lat + margin*height, right = max_long + margin*width)
  
  #basemap
  map <- get_stamenmap(map_box, zoom=10, maptype="toner-lite")
}

# NOTE: only does 10km atm
# get outermost longlat coords of the points, create a box
# padding to adjust grid if needed
get_grid <- function(bird_obs, grid_res=10000, outvarname="grid_ref") {
  
  max_lat <- max(bird_obs$latitude)
  min_lat <- min(bird_obs$latitude)
  max_long <- max(bird_obs$longitude)
  min_long <- min(bird_obs$longitude)
  height <- max_lat - min_lat
  width <- max_long - min_long
  
  box_corners = data.frame(lon= c(min_long, max_long), lat = c(max_lat, min_lat) )
  
  box <- st_polygon(
    list(
      cbind(
        box_corners$lon[c(1,1,2,2,1)],
        box_corners$lat[c(1,2,2,1,1)]
      )
    )
  )

  # initial coordinates is in latlong degrees ie. assume 4326
  box_coords <- st_sfc(box, crs=4326)
  # transform to make the grid
  box_coords <- st_transform(box_coords, crs=27700)
  
  # in metres
  grid <- create_grid_for_object(sp_object = box_coords, grid_resolution = grid_res, region="GB", outvarname = outvarname)
}


get_birds_per_grid <- function(bird_obs, grid) {
  bird_obs_10km <- add_10km_gridref(bird_obs, invar = "grid_ref")
  birds_per_grid <- bird_obs_10km %>%
    count(tenkm) %>%
    rename(num_birds = n, grid_ref = tenkm) %>% 
    arrange(grid_ref)
  
  print("Sanity check for count numbers:")
  print(sum(birds_per_grid$num_birds) == nrow(bird_obs))
  
  # add geometry of squares
  birds_per_grid <- merge(birds_per_grid, select(grid, c("grid_ref","geometry")), by = "grid_ref")
}

get_reportingrate_space <- function(lists, birds_per_grid) {
  lists_10km <- lists %>% 
    select(date,grid_ref) %>% 
    add_10km_gridref(invar = "grid_ref")
  
  # now count
  lists_per_grid <- lists_10km %>% 
    count(tenkm) %>% 
    rename(num_lists = n, grid_ref = tenkm) 
  
  # filter so only squares in which birds have been reported are used
  # then arrange in the same order as birds_per_list
  lists_per_grid <- lists_per_grid %>% 
    filter(grid_ref %in% birds_per_grid$grid_ref) %>% 
    arrange(grid_ref)
  
  
  reporting_rate <- data.frame(
    grid_ref = birds_per_grid$grid_ref,
    geometry = birds_per_grid$geometry,
    reporting_rate = birds_per_grid$num_birds/lists_per_grid$num_lists
  )
}


# FUNCTIONS END HERE
# ================================
bird_name <- "Cuckoo"
bird_obs2 <-get_bird_data("data_in/RENEW_extract_TL.csv", species = bird_name, areacode = "TL")
lists <-  get_user_data("data_in/RENEW_extract_TL.csv", areacode = "TL")
gc()

margin <- 0.04


grid <- get_grid(bird_obs)
birds_per_grid <- get_birds_per_grid(bird_obs, grid)
reporting_rate <- get_reportingrate_space(lists, birds_per_grid)


# TODO?: make it so that lists are filtered for arrival period for migrating species ie. max arrival/departure time? to get a more accurate reporting rate
# this may need a func to figure out arrival and departure times/further analysis
# or have an arbitrary cutoff point eg need to be above 100 lists


# PLOTTING
# ===========================================

map <- get_basemap(bird_obs, 0.04)


# convert to sf obj for plotting
bird_obs_sp <- st_as_sf(bird_obs, coords=c("longitude","latitude"), crs=4326)
birds_per_grid_sp <- st_sf(birds_per_grid, crs = 27700)
birds_per_grid_sp <- rename(birds_per_grid_sp, grid_ref = grid_ref)
reporting_rate_sp <- st_sf(reporting_rate, crs = 27700)


# convert back to 4326 to plot with basemap
birds_per_grid_sp <- st_transform(birds_per_grid_sp, crs = 4326)
reporting_rate_sp <- st_transform(reporting_rate_sp, crs=4326)
grid_degrees <- st_transform(grid, crs=4326)




# inherit.aes = FALSE needed to avoid compatibility issues
# see https://github.com/r-spatial/sf/issues/336 
plot_num <- ggmap(map) +
  geom_sf(data = birds_per_grid_sp, aes(fill = num_birds), inherit.aes = FALSE, alpha=0.8, col="NA") +
  geom_sf(data=bird_obs_sp, fill="#353636", size=0.7, inherit.aes = FALSE, alpha=0.5) +
  geom_sf(data=grid_degrees, col="#e60000", fill="NA", inherit.aes = FALSE)+
  scale_fill_gradient(low = "#f6c9bb", high = "#cc0000", name="Complete lists" )+
  labs(title = paste("Locations of complete lists with", bird_name, "sightings in TL region"), subtitle = "Using 10km squares", x="Latitude", y="Longitude")

plot_rate <- ggmap(map) +
  geom_sf(data = reporting_rate_sp, aes(fill = reporting_rate), inherit.aes = FALSE, alpha=0.8, col="NA") +
  geom_sf(data=bird_obs_sp, fill="#353636", size=0.7, inherit.aes = FALSE, alpha=0.5) +
  geom_sf(data=grid_degrees, col="#0000e6", fill="NA", inherit.aes = FALSE)+
  scale_fill_gradient(low = "#c2daf0", high = "#000080", name="Reporting Rate" )+
  labs(title = paste("Reporting rate for", bird_name, "in TL region"), subtitle = "Using 10km squares", x="Latitude", y="Longitude")

plot_rate

# ggsave(paste("results/",bird_name,"_plot.png", sep=""), plot_num, width = 10, height=8)
# ggsave(paste("results/",bird_name,"_plot_rate.png", sep=""), plot_rate, width = 10, height=8)


