# uses the total amount of lists by month to get reporting rate for a bird species

source("code/00 Packages and Functions.R")

complete_lists <- read_csv("results/lists_by_month.csv")
bird_obs <- read_csv("data_temp/cuckoo.csv")

# removing NAs and arranging by date
bird_obs <- bird_obs %>% 
  filter(!is.na(month_of)) %>% 
  arrange(desc(month_of))

# only contains months where bird was sighted
# arranges by date in the same way the bird absolute numbers are
patterns <- bird_obs$month_of
bird_sighted_months <- complete_lists %>% 
  filter(grepl(paste(patterns,collapse = "|"), month_of)) %>% 
  arrange(desc(month_of))

# divides absolute numbers by total lists for that month
# assumes complete lists can only note a bird once
reporting_rate_num <- bird_obs[,"n"] / bird_sighted_months[,"n"]
reporting_rate <- data.frame (
  month_of = bird_obs$month_of,
  reporting_rate = reporting_rate_num
)

earliest <- tail(reporting_rate, 1)$month_of
latest <- head(reporting_rate, 1)$month_of
chart <- plot_barchart(reporting_rate,"Cuckoo", earliest, latest, 
                       "Reporting rate for Cuckoo in TL",
                       "Reporting Rate")

ggsave("results/cuckoo_rate.png", chart, width= 10, height=7)

