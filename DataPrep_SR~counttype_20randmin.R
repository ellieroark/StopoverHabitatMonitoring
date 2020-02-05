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

#####TODO: CHECK THROUGH ALL DE-ANONYMIZED DATA AND ENSURE NO NAs

### add point id, count type, aru id, weather, aru_sample columns---------------

#create dataframe with key for the anonymized recorder names
rec <- c("rec1", "rec2", "rec3", "rec4")
aru_id <- c("SWIFT02", "SWIFT01", "AM", "SWIFT03")
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

# create a point_id column by fetching the point id for a particular date and 
# aru from the allaru df
locs <- data.frame(allaru$point_id, allaru$date, allaru$aru_id)
locs <- rename(locs, point_id = "allaru.point_id", date = "allaru.date", 
               aru_id = "allaru.aru_id")
locs$aruday <- paste0(locs$date, locs$aru_id, sep = "_")
aru20r$aruday <- paste0(aru20r$date, aru20r$aru_id, sep = "_")
aru20r <- left_join(aru20r, locs, by)

# create a new df with just weather variables and ptctids
weathervar <- ptct[which(ptct$aru_sample == TRUE), 
                   c("ptct_id", "start_time", "wind", "rain", "noise")]
weathervar <- distinct(weathervar)
# join to allaru df to add weather and start time variables
allaru <- left_join(allaru, weathervar, by = "ptct_id")

# make aru_sample col
allaru$aru_sample <- TRUE

### end add pt id, count type, aru_id, etc. cols--------------------------------

### small clean-up chores-------------------------------------------------------

# change pass sp to "passerine sp."
allaru[which(allaru$species_code == "pass sp"), "species_code"] <- "passerine sp."

# change allcaps SWIFT02 etc to lower case
allaru[which(allaru$aru_id == "SWIFT01"), "aru_id"] <- "swift01"
allaru[which(allaru$aru_id == "SWIFT02"), "aru_id"] <- "swift02"
allaru[which(allaru$aru_id == "SWIFT03"), "aru_id"] <- "swift03"

# get rid of aru_id when count_type is "point"
ptct[which(ptct$count_type == "point"), "aru_id"] <- NA

# make aru_sample FALSE for point counts when the ARU malfunctioned
ptct[which(ptct$ptct_id == "2019-04-16_p4"), "aru_sample"] <- FALSE
ptct[which(ptct$ptct_id == "2019-04-17_p1"), "aru_sample"] <- FALSE
ptct[which(ptct$ptct_id == "2019-05-22_p15"), "aru_sample"] <- FALSE

### end small cleanup chores----------------------------------------------------

### remove "no birds" counts from the dataframes before totalling sp. detected--
## create df with counts on which no birds were detected 
# aru cts with no birds
nobird_aruct <- allaru[which(allaru$species_code == "no birds"), 
                       c("anon_filename", "original_name", "date", "point_id", 
                         "ptct_id", "aru_id", "count_type", "start_time", 
                         "wind", "rain", "noise", "aru_sample")]
# pt cts with no birds
nobird_ptct <- ptct[which(ptct$species_code == "no birds"), 
                    c("date", "point_id", "ptct_id", 
                      "aru_id", "count_type", "start_time", "wind", 
                      "rain", "noise", "aru_sample")]

## add a "sp_detected" col for the no birds counts and make it equal to zero
nobird_aruct$sp_detected <- 0
nobird_ptct$sp_detected <- 0

## drop no birds counts from the allaru and ptct dfs
allaru_nb <- allaru[allaru$ptct_id %nin% nobird_aruct$ptct_id, ]
ptct_nb <- ptct[ptct$ptct_id %nin% nobird_ptct$ptct_id, ]

### end remove no birds counts from dfs-----------------------------------------

### add up species detected per point count for allaru and ptct-----------------

#total sp per pt ct for allaru
spdet_aru <- allaru_nb %>% 
  group_by(anon_filename) %>% 
  summarise(sp_detected = n())

#total sp per ptct for point counts
spdet_ptct <- ptct_nb %>%
  group_by(ptct_id) %>%
  summarise(sp_detected = n())


### end total species detected per count ---------------------------------------

### merge data frames-----------------------------------------------------------
## PREP for join-- add missing variables back to spdet_aru + spdet_ptct

# create dfs with place and date variables
var_aru <- select(allaru_nb, c("anon_filename","original_name", "date", "point_id",
                               "ptct_id", "aru_id", "count_type", "start_time", 
                               "wind", "rain", "noise", "aru_sample"))
var_pt <- select(ptct_nb, c("date", "point_id", "start_time", "aru_sample", 
                            "aru_id", "wind", "rain", "noise", "ptct_id", 
                            "count_type"))
# get rid of dupicate rows in the variable dataframes
var_aru <- distinct(var_aru)
var_pt <- distinct(var_pt)

# join variable dfs to spdet dfs
spdet_aru <- left_join(spdet_aru, var_aru, by = "anon_filename")
spdet_ptct <- left_join(spdet_ptct, var_pt, by = "ptct_id")

# add nobird counts back into spdet counts
spdet_aru <- rbind(spdet_aru, nobird_aruct)
spdet_ptct <- rbind(spdet_ptct, nobird_ptct)

# get rid of "original name" col in spdet_aru before pairing
spdet_aruF <- select(spdet_aru, -c("original_name"))

# actually merge spdet_aru and spdet_ptct
spdet_paired <- full_join(spdet_ptct, spdet_aruF, by = NULL)

### end merge dataframes--------------------------------------------------------

### make additional date and time cols needed for analysis----------------------
# make a day of year column 
spdet_paired$day_of_yr <- yday(spdet_paired$date)

## make a min_past_sun col that gives start time as minutes past sunrise
# combine month, day, year and sunrise time into one column
sunrise_times$year <- "2019"
sunrise_times$date <- paste(as.character(sunrise_times$month), sep = "-", 
                            as.character(sunrise_times$Day))
sunrise_times$date <- paste(as.character(sunrise_times$date), sep = "-", 
                            as.character(sunrise_times$year))
sunrise_times$sunrisedt <- paste(as.character(sunrise_times$date), sep = " ", 
                                 as.character(sunrise_times$Rise))

# convert min_past_sun datetime col into a positx value
sunrise_times$sunrisedt <- as.POSIXct(sunrise_times$sunrisedt, 
                                      format='%B-%d-%Y %H:%M:%S')

sunrise_times$date <- as.Date(sunrise_times$date, format='%B-%d-%Y')

# make start_time in spdet_paired into a datetime col with positx structure
spdet_paired$start_time <- paste(spdet_paired$date, sep = " ", 
                                 spdet_paired$start_time)

spdet_paired$start_time <- as.POSIXct(spdet_paired$start_time, 
                                      format= '%Y-%m-%d %H:%M:%S')

# left join so that sunrise times are associated with a date in spdet_paired df
#drop unncessary cols from sunrise_times
sunrise_times <- select(sunrise_times, -c(month, Day, Rise, Set, year))

spdet_paired <- left_join(spdet_paired, sunrise_times, by= "date")


# subtract sunrise time from start time to get min_past_sun
spdet_paired$min_past_sun <- difftime(spdet_paired$start_time, 
                                      spdet_paired$sunrisedt, units = "mins")

spdet_paired <- select(spdet_paired, -c(sunrisedt))

# convert to numeric
spdet_paired$min_past_sun <- as.numeric(spdet_paired$min_past_sun)

### end date and time col creation-----------------------------------------------

## organize df so that it's sorted by date and ptctid
spdet_paired <- spdet_paired[order(spdet_paired$date, spdet_paired$ptct_id,
                                   spdet_paired$anon_filename),]


# ### clean up environment
# rm(filenames, longaru, widearu, nobird_aruct, nobird_ptct,
#    spdet_aru, spdet_ptct, sunrise_times, var_aru, var_pt, weathervar, allaru,
#    ptct)






