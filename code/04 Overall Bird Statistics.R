# todo: most common bird sightings
# bird views by month
# similar to 01 file

raw <-  read_csv("data_in/RENEW_extract_TL.csv")

# remove dummy numbers
raw <- select(.data = raw, -...1)

# convert dates to date format
raw$date <- dmy(raw$obs_dt)
str(raw)

bird_lists <- raw %>% 
  select(sub_code, english_name, grid_ref, longitude, latitude, date)

write_csv(bird_lists, file="data_temp/bird_lists.csv")
