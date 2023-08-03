source("code/00 Helper Functions.R")


# functions that for a given square determine whether or not it should be analysed
users_TL88 <- get_user_data(path = "data_in/RENEW_extract_TL.csv", tenkm_area = "TL88", year = "2022")
bird_TL88 <- get_presenceabsence_data(path = "data_in/RENEW_extract_TL.csv", tenkm_area = "TL88", year = "2022", species = "SL")

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
  summarise(count = n())

min_detections <- detections_monthly %>% 
  filter(presence == 1) %>% 
  arrange(count) %>% 
  head(1)

print("Month with minimum amount of non-zero detections: ")
print(min_detections)
