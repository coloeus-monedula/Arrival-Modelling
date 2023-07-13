source("code/00 Helper Functions.R")
source("code/00 Graphing Functions.R")


birds <- read_csv("data_temp/bird_lists.csv")
name <-  "Swallow"

cumulative <- get_movingwindow_daylists(birds, name, "10 day", TRUE)
single_id_list <- birds %>% 
  filter(english_name =="Swallow" ) %>% 
  arrange(date)

cumulative_week <- get_movingwindow_daylists(birds, name, "week", TRUE)

first <- head(cumulative, 1)$date
last <- tail(cumulative,1)$date

chart <- plot_barchart(cumulative, "date", first, last, "Cumulative sum over 10 days for observations of Swallow on complete lists", "Moving window sum")

chart

chart2 <- plot_yearly_linechart(cumulative, "date", "Cumulative sum over 10 days for observations of Swallow on complete lists", "Moving window sum")
chart2
