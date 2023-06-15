raw <-  read_csv("data_in/RENEW_extract_TL.csv")
# raw <-  read_csv("BTO/Exploring-TL/data_in/RENEW_extract_TL.csv")


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
  select(user_code, sub_code, date, latitude, longitude) %>% 
  distinct()

# save to temp data
write_csv(x = user_lists, file = "data_temp/user_data.csv")

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

write_csv(x = user_lists_summary, file = "results/user_summary.csv")

# turn into vector to do lapply on
dates <- user_lists[["date"]]
total_months <- as.integer(ceiling(time_length(interval(min_date, max_date), unit="month")))

# empty dataframe
month_lists_count <- data.frame(matrix(ncol=2, nrow = 0))
colnames(month_lists_count) <- c("month_of", "n")
month_lists_count$month_of <- dmy(month_lists_count$month_of)


# TODO: refactor to allow this to be user-specific?
add_to_month_list<- function(date) {
  floored_date <- floor_date(date, unit = "month")
  if(floored_date %in% month_lists_count$month_of) {
    month_lists_count <<- month_lists_count %>%
      mutate(n=ifelse(month_of==floored_date,n+1,n))
  } else {
    new_row <-  c(floored_date, 1)
    month_lists_count <<- add_row(month_lists_count, month_of = floored_date, n = 1)
  }
}

lapply(dates, FUN = add_to_month_list)
month_lists_count <- arrange(month_lists_count, month_of)
write_csv(x = month_lists_count, file = "results/lists_by_month.csv")

TL_lists <- ggplot(data = month_lists_count, aes(x = month_of, y=n)) +
  geom_col() +
  labs(title = "Number of complete lists made in TL by month",
       x = "Date by month",
       y= "Number of lists") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=270)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_date(date_labels = "%b '%y", 
               breaks = seq(ymd("2017/01/01"), ymd("2022/12/01"), by = "month"), 
               limits=ymd(c("2016/12/01", "2023/01/01")),
               expand = c(0, 0))


ggsave("results/TL_lists.png", TL_lists, width= 10, height=7)
ggsave("results/TL_lists.svg", TL_lists, width= 10, height=7)
