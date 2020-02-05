################################
## Data Prep for Mixed Model analysis, Point Abbaye 2019
## 
## author: Ellie Roark
## created: 15 August 2019
## last modified: 15 Oct 2019
## 
## inputs: *Corrected_PointCounts_PtAbbaye2019.csv - spreadsheet with in-person
##          point count data from the 2019 field season 
##         *ARUPointCounts_WIDE_PtAbbaye2019.csv - spreadsheet with 
##          wide format observations from identical 10 min ARU point counts 
##         *ARUPointCounts_PtAbbaye2019.csv- spreadsheet with long format obser-
##          vations from identical 10 min ARU pt cts
##         *anonymized_file_key.csv - spreadsheet that links anonymized file names
##          to full, original files names for ARU point counts
##         *Sunrisetimes_PtAbbaye2019.csv- spreadsheet with sunrise times for 
##          April and May in L'Anse, Michigan, 2019. From the US Naval Observatory.
##
##         
## outputs: *spdet_paired - dataframe containing  the number of species detected
##           in each paired point count and ARU count, respectively, for the 2019
##           field season.
##            
## TODO: * 
################################

library(Hmisc)
library(tidyverse)
library(lubridate)


# setwd("/home/emer/Dropbox/Ellie Roark/R/PointAbbaye/")

widearu <- read_csv(file = "./2019data/ARUPointCounts_WIDE_PtAbbaye2019.csv")
longaru <- read_csv(file = "./2019data/ARUPointCounts_PtAbbaye2019.csv")
ptct <- read_csv(file = "./2019data/Corrected_PointCounts_PtAbbaye2019.csv")
filenames <- read_csv(file = "./2019data/anonymized_file_key.csv")
sunrise_times <- read_csv(file = "./2019data/SunriseTimes_PtAbbaye2019.csv")

#coerce date col in ptct df to date format
ptct$date <- as.Date(ptct$date, format = "%m/%d/%Y")

### combine widearu and longaru dfs---------------------------------------------

## cut longaru cols down to just the anon_file_name and species_code
longaru <- subset(longaru, select = -c(minute_detected, det_code, comments, 
                                       minute_half))
## remove duplicate records, keeping only unique species in each anon_filename
longaru <- distinct(longaru)

#add .wav to anon filenames in widearu
widearu$anon_filename <- paste(widearu$anon_filename, ".wav", sep = "")

##cut widearu cols down to just anon_filename and species_code
widearu <- subset(widearu, select = c(anon_filename, species_code))

#change colname so that the two dfs match
longaru <- rename(longaru, anon_filename = "anon_file_name")

## merge the two data frames
allaru <- rbind(longaru, widearu)

### end combine widearu + longaru dfs-------------------------------------------

### un-anonymize the aru file names---------------------------------------------

#get rid of columns we don't need in filenames
filenames <- subset(filenames, select = c(original_name, anon_name, date))
#change column name to match allaru
filenames <- rename(filenames, anon_filename = "anon_name")

#join filenames to allaru to add a column that has the un-anonymized file names
allaru <- left_join(allaru, filenames, by = "anon_filename")

### end un-anonymize------------------------------------------------------------

### add point id, count type, aru id, weather, aru_sample columns---------------

#create a point_id column for allaru from the point id indicated in original_name
allaru$point_id <- gsub(".*_...._", "", allaru$original_name)
allaru$point_id <- gsub(".wav", "", allaru$point_id)

#create a point count id column by pasting the point_id and date cols
allaru$ptct_id <- paste(as.character(allaru$date), allaru$point_id, sep = "_")
ptct$ptct_id <- paste(as.character(ptct$date), ptct$point_id, sep = "_")

#create an aru_id col for allaru
allaru$aru_id <- gsub("_........_...._...", "", allaru$original_name)
allaru$aru_id <- gsub("wav", "", allaru$aru_id)
allaru$aru_id <- gsub("[:.:]", "", allaru$aru_id)

#create count type col
allaru$count_type <- "aru"
ptct$count_type <- "point"

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


### clean up environment
rm(filenames, longaru, widearu, nobird_aruct, nobird_ptct, allaru_nb, ptct_nb,
   spdet_aruF, sunrise_times, var_aru, var_pt, weathervar)


