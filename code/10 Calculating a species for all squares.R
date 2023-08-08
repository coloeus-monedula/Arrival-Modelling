# pipeline steps. 
# FOR ALL: MOVE TO "00 PIPELINE FUNCTIONS, SOURCE THAT IN HELPER FUNCS, AND ALLOW FOR VERBOSE OUTPUT"
# 1. read in presence absence data for specified species and year [x]
# 2. get list of all squares in presence absence data. for every square, filter to that and run it through the threshold checks [x]
# 3. if the threshold check passes, for each square run the GAM model. return arrival day + arrival date + volume of data metrics (total lists, total detections) []
# 4. plot on geospatial map. adjust threshold as needed. []
# 5. second stage smoothing. []
# 


source("code/00 Pipeline Functions.R")

year <-  2022
species <-  "SL"

# thresholds
min_threshold = 100
min_month_threshold = 4
min_detections = 25

species_list <- get_presenceabsence_data(path = "data_in/RENEW_extract_TL.csv", species = species, year = year)

squares_list <- sort(unique(species_list$tenkm))

# empty dataframe for storing data
# only stores results where a GAM has been applied
gam_results <- data.frame(tenkm = rep(NA, length(squares_list)), arrival_day=rep(NA, length(squares_list)), arrival_date=rep(NA, length(squares_list)), total_lists=rep(NA, length(squares_list)), total_detections=rep(NA, length(squares_list)))

gam_results$arrival_date <- dmy(gam_results$arrival_date)

counter <-  1
for (tenkm_area in squares_list) {
  if (check_valid_thresholds(species_list, tenkm_area, min_threshold, min_month_threshold, min_detections, verbose=TRUE)) {
    # TODO: CALL GAM HERE
    results <- list(tenkm_area, NA, NA, NA, NA, NA)
    
    
    gam_results[counter,] <- results
    counter <- counter+1
  } 
  
}



# TODO: for later things need to generate plot of all tenkm squares in uk

