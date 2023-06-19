# todo: most common bird sightings
# bird views by month
# similar to 01 file


# CLEANING DATA
# ===========================
raw <-  read_csv("data_in/RENEW_extract_TL.csv")
# remove dummy numbers
raw <- select(.data = raw, -...1)
# convert dates to date format
raw$date <- dmy(raw$obs_dt)
str(raw)

bird_lists <- raw %>% 
  select(sub_code, english_name, scientific_name, grid_ref, longitude, latitude, date)

write_csv(bird_lists, file="data_temp/bird_lists.csv")



# OVERALL SUMMARY STATISTICS
# =================================
birds <- read_csv("data_temp/bird_lists.csv")

#count number of each bird species
birds_summary <- birds %>% 
  count(english_name) %>% 
  arrange(desc(n))

#earliest and latest observation dates
earliest_latest <- birds %>% 
  select(english_name, date, scientific_name) %>% 
  group_by(english_name) %>% 
  summarise(earliest_obs = min(date), latest_obs = max(date))

birds_summary <-merge(birds_summary, earliest_latest) %>% 
  arrange(desc(n))

# some english names have scientific names in brackets afterwards 
# they then get repeated in the scientific name column
# this will need to be cleaned
# also a lot of unknown species
write_csv(x = birds_summary, file = "results/bird_summary.csv")


# no chart of all bird sightings as it's more useful to do individual species vs all species

