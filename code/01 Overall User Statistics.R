raw <-  read_csv("data_in/RENEW_extract_TL.csv")

# remove dummy numbers
raw <- select(.data = raw, -...1)

# convert dates to date format
raw$date <- dmy(raw$obs_dt)
str(raw)
# check conversion worked
which(is.na(raw$date))

# find total date ranges and total users
min_date <- min(raw$date)
max_date <- max(raw$date)
num_users <-n_distinct(raw$user_code)


# map users to lists
user_lists <- raw %>% 
  select(user_code, sub_code) %>% 
  distinct()

# counts the amount of lists users have made. from most to least
user_lists_summary <- user_lists %>% 
  count(user_code) %>% 
  arrange(desc(n))

# adds earliest list and latest list dates
 earliest_latest <- raw %>% 
  select(user_code, date) %>% 
  group_by(user_code) %>% 
  summarise(earliest_list = min(date), latest_list = max(date))

user_lists_summary <- merge(user_lists_summary, earliest_latest) %>% 
  arrange(desc(n))

write_csv(x = user_lists_summary, file = "data_temp/user_summary.csv")


# TODO: make a observation chart based on aggregate month??

dates 




