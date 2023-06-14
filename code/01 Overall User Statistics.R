# raw <-  read_csv("data_in/RENEW_extract_TL.csv")
raw <-  read_csv("BTO/Exploring-TL/data_in/RENEW_extract_TL.csv")


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
  select(user_code, sub_code, date) %>% 
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



  
# TODO: make sorted?
dates <- user_lists[["date"]]
total_months <- as.integer(ceiling(time_length(interval(min_date, max_date), unit="month")))

# empty dataframe
month_lists_count <- data.frame(matrix(ncol=2, nrow = 0))
colnames(month_lists_count) <- c("month_of", "n")
month_lists_count$month_of <- dmy(month_lists_count$month_of)


add_to_month_list<- function(date) {
  floored_date <- floor_date(date, unit = "month")
  # TODO: bugfix - not making new months
  if(floored_date %in% month_lists_count$month_of) {
    month_lists_count <<- month_lists_count %>% 
      filter(month_of==floored_date) %>% 
      mutate(n=n+1)
  } else {
    new_row <-  c(floored_date, 1)
    month_lists_count <<- month_lists_count %>% 
      add_row(month_of = floored_date, n = 1)
  }
}

lapply(dates[1:1000], FUN = add_to_month_list)

