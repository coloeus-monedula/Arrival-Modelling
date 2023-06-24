library(BTOTools)
library(readr)
library(dplyr)
library(ggmap)
library(sf)

# todo:clip basemap to shapefile/polygon that is made up of squares
# " It uses BTOTools::create_grid_for_object() which might be a useful way to get grid-aggregated data for then making a heatmap as ideally you need a polygon layer which contains your cells to which you would merge whatever you're wanting to plot:"
# then plot points onto shapefile


# SELECTING SPECIFIC BIRD OBSERVATIONS
# ===========================================
# this is the OG input with most user data stripped out

# raw <- read_csv("data_temp/bird_lists.csv")
# 
# bird_obs <- raw %>% 
#   filter(english_name=="Cuckoo") %>% 
#   select(grid_ref, longitude, latitude, date)
# 
# write_csv(bird_obs, "data_temp/cuckoo_obs.csv")


# BASIC LONGLAT MAPPING
# ================================
bird_obs <- read_csv("data_temp/cuckoo_obs.csv")

# getting basemap dimensions from longlat data
max_lat <- max(bird_obs$latitude)
min_lat <- min(bird_obs$latitude)
max_long <- max(bird_obs$longitude)
min_long <- min(bird_obs$longitude)
margin <- 0.05

height <- max_lat - min_lat
width <- max_long - min_long
map_box <- c(bottom = min_lat - margin*height, left = min_long - margin*width,
         top = max_lat + margin*height, right = max_long + margin*width)

map <- get_stamenmap(map_box, zoom=10, maptype="terrain")

# CREATING SHAPEFILE
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

# transform back to plot
grid_degrees <- st_transform(grid, crs=4326)

# turn points into sf
bird_obs_sp <- st_as_sf(bird_obs, coords = c("longitude", "latitude"), crs=4326)

# needs to be done with pipeline, otherwise the transform doesn't stick for some reason
bird_obs_sp <-bird_obs_sp %>% 
  st_transform(27700)

# map <- st_transform(map, value=27700)

# https://stackoverflow.com/questions/47749078/how-to-put-a-geom-sf-produced-map-on-top-of-a-ggmap-produced-raster
# https://stackoverflow.com/questions/74733975/setting-crs-for-plotting-with-stamenmaps

# inherit.aes = FALSE needed to avoid compatibility issues
# see https://github.com/r-spatial/sf/issues/336 
# ggmap(map) +
#   geom_sf(data=grid, col="red", fill="NA", inherit.aes = FALSE)+
#   geom_sf(data = bird_obs_sp, aes(col=date), inherit.aes = FALSE)+
#   scale_color_date(low="#90CAF97F", high="#1565C0") +
#   labs(title = "Locations of complete lists with Cuckoo sightings")
  



# mymap <- ggplot() +
#   geom_sf(data=grid, col="red", fill="NA") +
#   geom_sf(data=bird_obs_sp, aes(col=date))
# mymap  

bird_obs_sp_grid <- st_join(grid,bird_obs_sp)

# filters out empty/no bird sighting squares then counts the remaining squares
birds_per_grid <- bird_obs_sp_grid %>% 
  filter(!is.na(date)) %>% 
  count(geometry) %>% 
  rename(num_birds = n)
  
# should be same as number of rows of og dataframe ie. bird_obs_sp_grid
(sum(birds_per_grid$num_birds))

# names(birds_per_grid) <- c('centroid_x', 'centroid_y', 'num_birds')
birds_per_grid <- st_as_sf(birds_per_grid, coords=geometry, crs=27700)

mymap <- ggplot() +
  geom_sf(data=grid, col="red", fill="NA") +
  geom_sf(data = birds_per_grid, aes(fill = num_birds)) +
  geom_sf(data=bird_obs_sp, aes(col=date))
mymap
