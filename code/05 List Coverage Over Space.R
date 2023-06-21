# TODO: longlat mapping for bird species?
# idea is to create a heatmap

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
raw <- read_csv("data_temp/bird_lists.csv")

bird_obs <- raw %>% 
  filter(english_name=="Cuckoo") %>% 
  select(grid_ref, longitude, latitude, date)

write_csv(bird_obs, "data_temp/cuckoo_obs.csv")


bird_obs <- read_csv("data_temp/cuckoo_obs.csv")
# todo: is there a way to search up grid_ref?

# getting basemap dimensions from longlat data
max_lat <- max(bird_obs$latitude)
min_lat <- min(bird_obs$latitude)
max_long <- max(bird_obs$longitude)
min_long <- min(bird_obs$longitude)
margin <- 0.05

# epsg 4326?
height <- max_lat - min_lat
width <- max_long - min_long
box <- c(bottom = min_lat - margin*height, left = min_long - margin*width,
         top = max_lat + margin*height, right = max_long + margin*width)

map <- get_stamenmap(box, zoom=10, maptype="toner-hybrid")

# todo: set colour to change value/opacity from past to present
ggmap(map) +
  geom_point(data = bird_obs, aes(x=longitude, y=latitude, col=date))

  