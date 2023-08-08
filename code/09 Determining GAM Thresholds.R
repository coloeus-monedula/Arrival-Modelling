source("code/00 Helper Functions.R")

library(ggplot2)
library("gridExtra")


#THINGS THAT SHOULDN'T PASS
#SI TL81 2021
# SI TL82 2021
# SI tl80 2021 - arrival date is apr 18. tl 88 is apr 27th though? has 43 records
# TL10 has 60 records. has apr 28th?


# TL10 - casual records? median for 2021 is 11 per list
# SL TL10 2021 = Mar 23
# SL TL88 2021 = Mar 28


# set limit to 30? seems to be the minimum number needed to form a graph
# test: TL88, TL80, TL10, TL 91 (2022)

# 2022 - TL87 - CC. set as per month = 9 so 108 lists minimum/roughly 100?
# 2022 - TL91 - CC. min = 4, but produces results for CC (total lists = 202). maybe the month w minimum lists (so you get birds) requirement is already covered in the "total birds" bit? 
# for 2022, TL11 and TL12 both have below min month but for SL work (for TL11)

year = "2022"
tenkm_area = "TL91"
species = "CC"
# functions that for a given square determine whether or not it should be analysed
# NOTE: DOESN'T NEED USER DATA - CAN JUST USE PRESENCE/ABSENCE DATA
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
# no. total number of detections
# roughly 30
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
