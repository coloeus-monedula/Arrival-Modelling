library(svglite)
source("code/00 Graphing Functions.R")
source("code/00 Helper Functions.R")

# columns pre-selected data frames - could just do raw data and sort that way
user <- read_csv("data_temp/user_data.csv")
u_code <- "u10912"

birds <- read_csv("data_temp/bird_lists.csv")
# TODO: allow for both english name or scientific name? or just do scientific name?
bird_name <- "Cetti's Warbler"

# 
# (get_summary_info(user, u_code))
# month_count <-  get_monthly_lists(user, u_code)
# chart <- plot_monthcount_barchart(month_count, u_code)
# chart

# (get_summary_info(birds, bird_name, is_bird=TRUE))
# month_count <-  get_monthly_lists(birds, bird_name, TRUE)
# chart <- plot_monthcount_barchart(month_count, bird_name, TRUE)
# chart

(get_summary_info(birds, bird_name, is_bird=TRUE))
month_count <-  get_interval_lists(birds, bird_name, "month", TRUE)
month_count2 <-  get_interval_lists(birds, bird_name, "month", TRUE)
week_count <-  get_interval_lists(birds, bird_name, "week", TRUE)

tenday_count <-  get_interval_lists(birds, bird_name, "10 days", TRUE)
chart <- plot_monthcount_barchart(month_count, bird_name, TRUE)
chart

write_csv(month_count, file = "data_temp/cuckoo.csv")

ggsave("results/cuckoo.png", chart, width= 10, height=7)
