#read the data
raw <- read.csv('data_in/RENEW_extract_TL.csv', stringsAsFactors = FALSE)

#convert date string to date type and filter to one year for this simple example
raw$obs_dt2 <- as.Date(raw$obs_dt, format="%d/%m/%Y")

#filter to 2022
raw2022 <- subset(raw, year(obs_dt2)==2022)

#drop unwanted columns
raw2022$X <- NULL
raw2022$scientific_name <- NULL
raw2022$obs_dt <- NULL


#get 10-km reference - need to run separate processes on the rows with tetrad vs 1-km gridrefs
raw2022$gl <- nchar(raw2022$grid_ref)
seta <- subset(raw2022, gl == 5)
seta$tenkm <- substr(seta$grid_ref,1,4)
setb <- subset(raw2022, gl == 6)
setb <- rescale_1km_to_10km(setb, invar = 'grid_ref')
raw2022 <- rbind(seta, setb)
raw2022$gl <- NULL

#filter to TL88
raw2022TL88 <- subset(raw2022, tenkm == 'TL88')

#get day of the year and week of the year columns
raw2022TL88$daynum <- yday(raw2022TL88$obs_dt2)
raw2022TL88$weeknum <- week(raw2022TL88$obs_dt2)



#pivot wider to get presence absence for each species on each list
#easier if done with species code as these will become unambiguous column names
raw2022TL88 <- merge(raw2022TL88, global_species_lookup[c('english_name','code2ltr')], by = 'english_name')
raw2022TL88$english_name <- NULL
raw2022TL88$present <- 1

#using value_fill to ensure we get zeroes in the list*species permutations where a species is absent
raw2022TL88wide <- pivot_wider(data = raw2022TL88, id_cols = c(user_code, sub_code, grid_ref, latitude, longitude, uncertainty_radius, tenkm, obs_dt2, daynum, weeknum), names_from = code2ltr, values_from = present, values_fill = list(present = 0))
raw2022TL88wide <- as.data.frame(raw2022TL88wide)
head(raw2022TL88wide)



focal_spp <- 'SL'

#get the column containing the species of interest
coln <- which(names(raw2022TL88wide)==focal_spp)
raw2022TL88wide$focal <- raw2022TL88wide[,coln]

#calculate simple weekly reporting rate as sense check
reprate_week <- setNames(aggregate(data = raw2022TL88wide, focal ~ weeknum, mean), c('weeknum', 'reprate'))
#make a midpoint daynum for each week to we can plot the bars in the same positions as the daily model later
reprate_week$daynum <- ((reprate_week$weeknum - 1) * 7) + 3.5

#fit the GAM
mod <- gam(focal ~ s(daynum, k=10), family = 'binomial', data = raw2022TL88wide)

#make a dataframe of the daynums for prediction, with a 0.25 increment
newdf <- data.frame(daynum = seq(from = 0, to = 365, by = 0.25))

#make predictions for these new data - type is response to get the values on the response scale (not the binomial scale which is the default)
preds <- predict(mod, newdata = newdf, type='response')
#bind with the dates
preds <- cbind(newdf, preds)
names(preds) <- c('daynum', 'reprate_smoothed')

#and plot to check
ggplot() +
  geom_col(data = reprate_week, aes(x = daynum, y = reprate), fill='grey75') +
  geom_line(data = preds, aes(x = daynum, y = reprate_smoothed), col='red') +
  theme_classic()

#looks good - so now do the differentials to get the first peak
diffs <- diff(preds$reprate_smoothed)
#find the index for the first time the diff turns negative
i <- which(diffs < 0)[1]
#and use this to get the daynum this corresponds to, and the reporting rate it corresponds to
firstpeak <- preds[i,]

#get the daynum where reporting rate is 10% of the first peak
targetrr <- firstpeak$reprate_smoothed * 0.1
i2 <- which(preds$reprate_smoothed >= targetrr)[1]
arrivalstart <- preds[i2,]
as.Date(arrivalstart$daynum-1, origin = "2022-01-01")

#plot
ggplot() +
  geom_col(data = reprate_week, aes(x = daynum, y = reprate), fill='grey75') +
  geom_line(data = preds, aes(x = daynum, y = reprate_smoothed), col='red') +
  geom_vline(xintercept = firstpeak$daynum, col='blue') +
  geom_vline(xintercept = arrivalstart$daynum, col='green') +
  theme_classic()
