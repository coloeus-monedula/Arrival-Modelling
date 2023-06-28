library(BTOTools)
library(readr)
library(dplyr)
library(ggmap)
library(sf)

source("code/00 Helper Functions.R")


# SELECTING SPECIFIC BIRD OBSERVATIONS
# ===========================================
# this is the OG input with most user data stripped out

bird_name <- "Buzzard"

# raw <- read_csv("data_temp/bird_lists.csv")
# 
# bird_obs <- raw %>%
#   filter(english_name==bird_name) %>%
#   select(grid_ref, longitude, latitude, date)
# 
# write_csv(bird_obs, paste("data_temp/",bird_name,"_obs.csv", sep=""))


# BASIC LONGLAT MAPPING
# ================================
bird_obs <- read_csv(paste("data_temp/",bird_name,"_obs.csv", sep=""))

# getting basemap dimensions from longlat data
max_lat <- max(bird_obs$latitude)
min_lat <- min(bird_obs$latitude)
max_long <- max(bird_obs$longitude)
min_long <- min(bird_obs$longitude)
margin <- 0.04

height <- max_lat - min_lat
width <- max_long - min_long
map_box <- c(bottom = min_lat - margin*height, left = min_long - margin*width,
         top = max_lat + margin*height, right = max_long + margin*width)

# CREATING SHAPEFILE
# ====================================================================
# get outermost longlat coords of the points, create a box
# padding to adjust grid if needed
padding <- 0
box_corners = data.frame(lon= c(min_long-padding*width, max_long+padding*width), lat = c(max_lat+padding*height, min_lat-padding*height) )

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
grid_res <- 10000
outvarname <- "square_id"
grid <- create_grid_for_object(sp_object = box_coords, grid_resolution = grid_res, region="GB", outvarname = outvarname)

bird_obs_10km <- add_10km_gridref(bird_obs, invar = "grid_ref")
birds_per_grid <- bird_obs_10km %>%
  count(tenkm) %>%
  rename(num_birds = n, square_id = tenkm)

print("Sanity check for count numbers:")
print(sum(birds_per_grid$num_birds) == nrow(bird_obs))

# add geometry of squares
birds_per_grid <- merge(birds_per_grid, select(grid, c("square_id","geometry")), by = "square_id")

# convert to sf obj for plotting
birds_per_grid_sp <- st_sf(grid_ref=birds_per_grid$square_id, num_birds=birds_per_grid$num_birds, geometry = birds_per_grid$geometry, crs = 27700)

# convert back to 4326 to plot with basemap
birds_per_grid_degrees <- st_transform(birds_per_grid_sp, crs = 4326)
bird_obs_sp_degrees <- st_transform(bird_obs_sp, crs=4326)
grid_degrees <- st_transform(grid, crs=4326)

#basemap
map <- get_stamenmap(map_box, zoom=10, maptype="toner-lite")



# REPORTING RATE
# ====================================================
# for now uses data prefiltered to show total lists
lists <-  read_csv("data_temp/user_data.csv")
lists_sp <- st_as_sf(lists, coords=c("longitude", "latitude"), crs = 4326 )
# TODO: account for fact some squares will have lists but not bird sighting

lists_sp <-  lists_sp %>%
  st_transform(crs=27700)

# grid first because we want the geometry squares
lists_sp_grid <- st_join(grid, lists_sp)


# TODO: rejig so users also has grid ref? or just add square_id to it assuming they're equal


# filters so only rows with polygons that are in the bird_obs dataframe is included
# TODO: also filter so you make it so that background lists are only done for arrival period for migrating species
# ie. max arrival/departure time?
# this may need a func to figure out arrival and departure times/analysis
# or have an arbitrary cutoff point eg need to be above 100 lists

patterns <- birds_per_grid$geometry
lists_sp_grid <- lists_sp_grid %>%
  filter(grepl(paste(patterns, collapse = "|"), geometry))


# now count
lists_per_grid <- lists_sp_grid %>% 
  filter(!is.na(sub_code)) %>% 
  count(geometry) %>% 
  rename(num_lists = n)





# PLOTTING
# ===========================================
# inherit.aes = FALSE needed to avoid compatibility issues
# see https://github.com/r-spatial/sf/issues/336 
plot <- ggmap(map) +
  geom_sf(data = birds_per_grid_degrees, aes(fill = num_birds), inherit.aes = FALSE, alpha=0.8, col="NA") +
  geom_sf(data=bird_obs_sp_degrees, fill="#353636", size=0.7, inherit.aes = FALSE, alpha=0.5) +
  geom_sf(data=grid_degrees, col="#e60000", fill="NA", inherit.aes = FALSE)+
  scale_fill_gradient(low = "#f6c9bb", high = "#cc0000", name="Complete lists" )+
  labs(title = paste("Locations of complete lists with", bird_name, "sightings in TL region"), subtitle = "Using 10km squares", x="Latitude", y="Longitude")

plot
ggsave(paste("results/",bird_name,"_plot.png", sep=""), plot, width = 10, height=8)

