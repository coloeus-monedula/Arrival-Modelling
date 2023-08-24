source("code/00 * Pipeline Functions.R")


### This is a script that runs and fits a GAM model for a single year, species, and grid reference, and outputs a graph with weekly count, fitted prediction curve, and lines for showing where the first peak of a curve is as well as the arrival date. It is used in conjunction with the main script to investigate specific squares further. 

### Inputs
year <-  2022
species <- "CC"
grid_ref_in <- "TL88"
#10000 20000 or 50000
square_size <-  10000
n_threshold <- 1
file_path <- "data_in/RENEW_extract_TL.csv"

### Getting presence absence data and doing some cleaning

bird <- get_presenceabsence_data(file_path, square_size = square_size, grid_ref_in = grid_ref_in, species = species, year = year)
gc()
bird <- clean_data(bird, n_threshold = n_threshold)

# bird <- read_csv("data_temp/osprey_2021_10km.csv")
# bird <- bird %>%
#   filter(grid_ref == grid_ref_in)

### Creating the weekly reporting rate as a sensecheck 
# convert to numerical day of the year
bird$day <- yday(bird$date)
bird$week <- week(bird$date)

bird_weekly <- bird %>% 
  group_by(week) %>% 
  summarise(rate = mean(presence))

# midpoint for plotting
bird_weekly$day <- ((bird_weekly$week -1) * 7) + 3.5

day_k = 20
count_k = 10

#### Modelling the GAM and predicting results to plot
results <- predict_arrival(bird, to_graph = TRUE )
completed <- results$completed

if (completed) {
  arrival_date <- results$arrival_date
  arrival_start <- results$arrival_start
  first_peak <- results$first_peak
}
predicted <- results$predicted

#### Graphing
graph <- ggplot(data=predicted, aes(x=day, y=rate)) +
  geom_col(data = bird_weekly, aes(x=day, y=rate), fill="grey75")+
  geom_line() +
  labs(title=paste(species, "for", grid_ref_in, "in", year), x="Day", y="Reporting rate")

if (completed) {
  graph <- graph +
  geom_vline(xintercept = first_peak$day, col="blue") +
  geom_vline(xintercept = arrival_start$day, col="green") +
  labs(subtitle = paste("Arrival date = ", format(as.Date(arrival_date), "%b %d")))
    
}

print(graph)

# to check gam model: gam.check() and
# dsm::rqgam_check()


