source("code/00 Helper Functions.R")

library(ggplot2)
library("gridExtra")


year = "2022"
tenkm_area = "TL88"
species = "SL"
# functions that for a given square determine whether or not it should be analysed
users_TL88 <- get_user_data(path = "data_in/RENEW_extract_TL.csv", tenkm_area = tenkm_area, year = year)
bird_TL88 <- get_presenceabsence_data(path = "data_in/RENEW_extract_TL.csv", tenkm_area = tenkm_area, year = year, species = species)

# user list thresholding
total_lists <- nrow(users_TL88)
print(paste("Total lists:", total_lists))

#lists per month - find minimum
monthly_users <- get_interval_lists(users_TL88, "month")
min_lists <- monthly_users %>% 
  arrange(n) %>% 
  head(1)

print("Month with minimum lists:")
print(min_lists)


# detection thresholding
# minimum detections needed for analysis per month?
detections_monthly <- bird_TL88 %>% 
  group_by(month(date), presence) %>% 
  summarise(count = n()) %>% 
  rename(month = `month(date)`)


min_detections <- detections_monthly %>% 
  filter(presence == 1) %>% 
  arrange(count) %>% 
  head(1)

print("Month with minimum amount of non-zero detections: ")
print(min_detections)


detections_monthly$presence <- factor(detections_monthly$presence)

min_lists_plot <- ggplot(data = monthly_users, aes(x=month(date), y=n)) +
  geom_line() +
  labs(title=paste("Monthly user lists for", tenkm_area,"and year", year), x="Month", y="Number")

min_detections_plot <- ggplot(data=detections_monthly, aes(x=month, y=count, group=presence, colour=presence )) +
  geom_line() +
  labs(title=paste("Monthly lists with and without", species, "detections for", tenkm_area, "and year", year), x="Month", y="Number") +
  theme(legend.position = "bottom")

grid.arrange(min_lists_plot, min_detections_plot)
