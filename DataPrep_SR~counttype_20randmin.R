################################
## 20 random minutes ARU data- prep for Mixed models, Point Abbaye 2019 data
## 
## author: Ellie Roark
## created: 5 Feb 2020
## last modified: 5 Feb 2020
## 
## inputs: *ARU20randmin.csv- file with bird species identifications from 20 
##          random minutes of aru data for each recorder for each survey day
##         *filename_key_20randmin.csv- key containing date, original file name
##          and other essential information to de-anonymize the file names in 
##          ARU20randmin.csv
##         
## outputs: *
##            
## TODO: * check through de-anonymized data and ensure there are no NAs
################################

### read in random minute aru observations and file name key

aru20r <- read_csv(file = "./2019data/ARU20randmin.csv")
filenames <- read_csv(file = "./2019data/filename_key_20randmin.csv", 
                      col_types = cols(.default = "?", folder = "c"))

##cut columns down to just anon_filename and species_code
aru20r <- subset(aru20r, select = c(filename, species_code))

##change filename to anon_name in aru20r
aru20r <- rename(aru20r, anon_name = "filename")

### un-anonymize the aru file names---------------------------------------------

#get rid of columns we don't need in filenames
filenames <- subset(filenames, select = c(selec, sound.files, anon_name, date))

#join filenames to allaru to add a column that has the un-anonymized file names
aru20r <- left_join(aru20r, filenames, by = "anon_name")

### end un-anonymize------------------------------------------------------------

### add point id, count type, aru id, weather, aru_sample columns---------------

#create dataframe with key for the anonymized recorder names
rec <- c("rec1", "rec2", "rec3", "rec4")
aru_id <- c("swift02", "swift01", "AM", "swift03")
recs <- data.frame(aru_id, rec)

#create an aru_id col for allaru
aru20r$rec <- gsub("day.._?", "", aru20r$anon_name)
aru20r$rec <- gsub("_.*_.*", "", aru20r$rec)

#de-anonymize aru_id
aru20r <- left_join(aru20r, recs, by = "rec")

#remove "rec" column-- aru_id is all that's needed
aru20r$rec <- NULL

#create count type col
aru20r$count_type <- "arurand"

# fetch the point id for a particular date and aru from the allaru df
locs <- data.frame(allaru$point_id, allaru$date, allaru$aru_id)
locs <- rename(locs, point_id = "allaru.point_id", date = "allaru.date", 
               aru_id = "allaru.aru_id")

#create an aruday column that says both the date and the aru
locs$aruday <- paste(as.character(locs$date), as.character(locs$aru_id), 
                     sep = "_")
aru20r$aruday <- paste(as.character(aru20r$date), as.character(aru20r$aru_id), 
                       sep = "_")

#get rid of unnecessary columns and rows in locs
locs$date <- NULL
locs$aru_id <- NULL
locs <- unique(locs)

#add point id to aru20r by joining locs and aru20r by aruday
aru20r <- left_join(aru20r, locs, by = "aruday")

#add a "ptct_id" column that pastes day and point_id (location) together
aru20r$ptct_id <- paste(as.character(aru20r$date), aru20r$point_id, sep = "_")

# create a new df with just weather variables, aru_id and date from ptct data
weathervar <- allaru[ ,
                   c("aru_id", "date", "wind", "rain", "noise")]
# create aruday for weathervar
weathervar$aruday <- paste(as.character(weathervar$date), weathervar$aru_id, 
                           sep = "_")

#use only unique rows of weather data
weathervar <- distinct(weathervar)

#remove aru_id and date, since they are no longer needed in weathervar
weathervar$aru_id <- NULL
weathervar$date <- NULL

# join to aru20r df to add weather and start time variables
aru20r <- left_join(aru20r, weathervar, by = "aruday")

# make aru_sample col
aru20r$aru_sample <- TRUE

### end add pt id, count type, aru_id, etc. cols--------------------------------

### add up species detected per 20 random minutes-------------------------------
## make new df that will have one row per aruday (effectively a count id). 
# make unique ptct_id column
aruday <- unique(aru20r$aruday)
# make sp_detected col
sp_detected <- NA
# combine them into df
spdet_20r <- data.frame(aruday, sp_detected)

## for loop that will add up the number of species detected per point count
for (i in 1:nrow(spdet_20r)){
  thisct <- spdet_20r$aruday[i]
  thisdat <- aru20r[aru20r$aruday == thisct, ]
  n_sp <- length(unique(thisdat$species_code))
  if (any(thisdat$species_code == "no birds")){
    spdet_20r$sp_detected[i] <- n_sp-1
  }
  else {spdet_20r$sp_detected[i] <- n_sp}
}

### end total species detected per count ---------------------------------------

### merge with spdet_paired dataframe-----------------------------------------
## PREP for join-- add missing variables back to spdet20r
# create dfs with place and date variables
covs <- select(aru20r, c("aruday", "date", "point_id", "ptct_id", "aru_id", "count_type",
                          "wind", "rain", "noise", "aru_sample"))

# get rid of dupicate rows in the variable dataframes
covs <- distinct(covs)

# join variable dfs to spdet dfs
spdet_20r <- left_join(spdet_20r, covs, by = "aruday")

##TODO: create day of year and min past sun cols in spdet_20r

# actually merge spdet_aru and spdet_ptct
spdet_3ct <- full_join(spdet_paired, spdet_20r, by = NULL)

### end merge dataframes--------------------------------------------------------
# 
# ### make additional date and time cols needed for analysis----------------------
# # make a day of year column 
# spdet_paired$day_of_yr <- yday(spdet_paired$date)
# 
# ## make a min_past_sun col that gives start time as minutes past sunrise
# # combine month, day, year and sunrise time into one column
# sunrise_times$year <- "2019"
# sunrise_times$date <- paste(as.character(sunrise_times$month), sep = "-", 
#                             as.character(sunrise_times$Day))
# sunrise_times$date <- paste(as.character(sunrise_times$date), sep = "-", 
#                             as.character(sunrise_times$year))
# sunrise_times$sunrisedt <- paste(as.character(sunrise_times$date), sep = " ", 
#                                  as.character(sunrise_times$Rise))
# 
# # convert min_past_sun datetime col into a positx value
# sunrise_times$sunrisedt <- as.POSIXct(sunrise_times$sunrisedt, 
#                                       format='%B-%d-%Y %H:%M:%S')
# 
# sunrise_times$date <- as.Date(sunrise_times$date, format='%B-%d-%Y')
# 
# # make start_time in spdet_paired into a datetime col with positx structure
# spdet_paired$start_time <- paste(spdet_paired$date, sep = " ", 
#                                  spdet_paired$start_time)
# 
# spdet_paired$start_time <- as.POSIXct(spdet_paired$start_time, 
#                                       format= '%Y-%m-%d %H:%M:%S')
# 
# # left join so that sunrise times are associated with a date in spdet_paired df
# #drop unncessary cols from sunrise_times
# sunrise_times <- select(sunrise_times, -c(month, Day, Rise, Set, year))
# 
# spdet_paired <- left_join(spdet_paired, sunrise_times, by= "date")
# 
# 
# # subtract sunrise time from start time to get min_past_sun
# spdet_paired$min_past_sun <- difftime(spdet_paired$start_time, 
#                                       spdet_paired$sunrisedt, units = "mins")
# 
# spdet_paired <- select(spdet_paired, -c(sunrisedt))
# 
# # convert to numeric
# spdet_paired$min_past_sun <- as.numeric(spdet_paired$min_past_sun)
# 
# ### end date and time col creation-----------------------------------------------
# 
# ## organize df so that it's sorted by date and ptctid
# spdet_paired <- spdet_paired[order(spdet_paired$date, spdet_paired$ptct_id,
#                                    spdet_paired$anon_filename),]
# 
# 
# # ### clean up environment
# # rm(filenames, longaru, widearu, nobird_aruct, nobird_ptct,
# #    spdet_aru, spdet_ptct, sunrise_times, var_aru, var_pt, weathervar, allaru,
# #    ptct)
# 
# 
# 
# 
# 
# 
