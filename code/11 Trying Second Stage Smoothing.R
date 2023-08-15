source("code/00 Pipeline Functions.R")

first_gam <- read_csv("data_temp/gam_results_swallow_2022.csv")
square_size <- 10000
year <- 2022

gam_smoothing <- gam(arrival_start ~ s(easting, northing, bs="tp", k = 20), method = "REML", data = first_gam)

test <- data.frame(gridref = c("TL00", "TL99", "TL98"))
start_end <- gridref_to_coordinates(test, "gridref")
# get all squares and their easting/northing

# TL30 gets 98.5. redoing TL31 = 98.5
# TL88 previously was 2022-04-07 but now is 2022-04-12. considering this is the most surveyed square, smoothing should be weighted towards this date more



coords <- get_grid(min_easting = start_end[1,]$easting, min_northing = start_end[1,]$northing, max_easting = start_end[2,]$easting, max_northing = start_end[2,]$northing, square_size = square_size )

coords$predicted <- predict(gam_smoothing, coords, type="response")
coords$predicted_date <- as.Date(coords$predicted, origin = ymd(year, truncated=2))



# SECOND STAGE SMOOTHING
# 1. Get all grid references by finding northmost and eastmost gridref and subtract 1000? [xx]
# 2. Run through predictions [x]
# 3. Add gridref to df [x]
# 4. add indication of what was smoothed and what was raw data [] - add outline ?
# 4. Plot [] - this will use the original grid generated for the geometry