################################
## Data Cleaning, Point Abbaye 2019-- OBSOLETE SCRIPT AS OF 6 SEPT 2019
## DO NOT RUN
## 
## author: Ellie Roark
## created: 15 August 2019
## last modified: 2 Sept 2019
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
## outputs: *allaru - dataframe with all ARU observations
##          *ptct - dataframe with all point count observations
##          *OLDspdet_paired - dataframe containing  the number of species detected
##           in each paired point count and ARU count, respectively, for the 2019
##           field season.
##            
## TODO: * 
################################

library(Hmisc)
library(tidyverse)
library(lubridate)


setwd("C:/Users/Ellie/Dropbox/Ellie Roark/R/PointAbbaye/")

widearu <- read_csv(file = "./2019data/ARUPointCounts_WIDE_PtAbbaye2019.csv")
longaru <- read_csv(file = "./2019data/ARUPointCounts_PtAbbaye2019.csv")
ptct <- read_csv(file = "./2019data/Corrected_PointCounts_PtAbbaye2019.csv")
filenames <- read_csv(file = "./2019data/anonymized_file_key.csv")
sunrise_times <- read_csv(file = "./2019data/SunriseTimes_PtAbbaye2019.csv")

#coerce date col in ptct df to date format
ptct$date <- as.Date(ptct$date, format = "%m/%d/%Y")

### combine widearu and longaru dfs---------------------------------------------

##combine "minute half" and "minute detected" cols in longaru
longaru$minute_half[longaru$minute_half == "1"] <- "00"
longaru$minute_half[longaru$minute_half == "2"] <- "30"
longaru$minute_detected <- paste(longaru$minute_detected, longaru$minute_half, 
                                 sep = ":")
longaru$minute_detected[longaru$minute_detected == "NA:NA"] <- NA
longaru <- select(longaru, -c(minute_half))

#add .wav to anon filenames in widearu
widearu$anon_filename <- paste(widearu$anon_filename, ".wav", sep = "")


##switch widearu to long format
aru <- gather(widearu, minute_detected, det_code, "0:00":"9:30", factor_key = TRUE)

#remove duplicate records created for each value of min_detected
#start by making the det_code for "no birds" counts N for NONE
aru[which(aru$species_code == "no birds"), "det_code"] <- "N"
#drop all rows that have an "NA" det_code because those are now rows where no 
# species was observed
aru <- drop_na(aru, det_code)
#drop species common name col
aru <- select(aru, -c(species_common_name))

##join longaru to aru to create a single aru df
#change column names to match
longaru <- rename(longaru, anon_filename = "anon_file_name")
#merge the two dfs into aru
allaru <- merge(aru, longaru, all = TRUE)

### end combine widearu + longaru dfs-------------------------------------------

### un-anonymize the aru file names---------------------------------------------

#get rid of columns we don't need in filenames
filenames <- select(filenames ,-c(month, day, wk_half))
#change column name to match allaru
filenames <- rename(filenames, anon_filename = "anon_name")

#join filenames to allaru to add a column that has the un-anonymized file names
allaru <- left_join(allaru, filenames, by = "anon_filename")

### end un-anonymize------------------------------------------------------------

### add point id, count type, aru id columns------------------------------------

#create a point_id column for allaru from the point id indicated in original_name
allaru$point_id <- gsub(".*_...._", "", allaru$original_name)
allaru$point_id <- gsub(".wav", "", allaru$point_id)

#create a point count id column by pasting the point_id and date cols
allaru$ptct_id <- paste(allaru$date, allaru$point_id, sep = "_")
ptct$ptct_id <- paste(ptct$date, ptct$point_id, sep = "_")

#create an aru_id col for allaru
allaru$aru_id <- gsub("_........_...._...", "", allaru$original_name)
allaru$aru_id <- gsub("wav", "", allaru$aru_id)
allaru$aru_id <- gsub("[:.:]", "", allaru$aru_id)

#create count type col
allaru$count_type <- "aru"
ptct$count_type <- "point"
 
### end add pt id, count type, aru_id cols--------------------------------------

### small clean-up chores-------------------------------------------------------

# change pass sp to "passerine sp."
allaru[which(allaru$species_code == "pass sp"), "species_code"] <- "passerine sp."

# change allcaps SWIFT02 etc to lower case
allaru[which(allaru$aru_id == "SWIFT01"), "aru_id"] <- "swift01"
allaru[which(allaru$aru_id == "SWIFT02"), "aru_id"] <- "swift02"
allaru[which(allaru$aru_id == "SWIFT03"), "aru_id"] <- "swift03"

## create a list of ptct ids for which no birds were detected 
# all aru ptct ids with no birds
nobird_aruct <- allaru[which(allaru$species_code == "no birds"), "ptct_id"]
# ptct ids with no birds
nobird_ptct <- ptct[which(ptct$species_code == "no birds"), "ptct_id"]

 
### end small cleanup chores----------------------------------------------------


### add up species detected per point count in both data frames-----------------

#total sp per pt ct for allaru and create Day of year column
spdet_aru <- allaru %>% 
  select(anon_filename, ptct_id, species_code, date, point_id, count_type, 
         aru_id) %>%
  unique() %>%
  group_by(date, point_id, ptct_id, count_type, aru_id, anon_filename) %>% 
  summarise(n()) %>%
  mutate(day_of_yr = yday(date))

#total sp per ptct for point counts and create Day of year col
# try doing this with group_by %>% mutate(n = n()) %>% distinct or unique
spdet_ptct <- ptct %>%
  select(ptct_id, species_code, date, point_id, count_type, aru_sample, aru_id, 
         start_time, wind, rain, noise) %>%
  unique() %>%
  group_by(date, point_id, ptct_id, count_type, aru_sample, aru_id, start_time, 
           wind, rain, noise) %>%
  summarise(n()) %>%
  mutate(day_of_yr = yday(date))

#rename n() column to something sensible
colnames(spdet_aru)[which(colnames(spdet_aru) == "n()")] <- 
  "sp_detected"
colnames(spdet_ptct)[which(colnames(spdet_ptct) == "n()")] <- 
  "sp_detected"

# make species detected counts for which there are no birds be 0
spdet_aru[spdet_aru$ptct_id %in% nobird_aruct, "sp_detected"] <- 0
spdet_ptct[spdet_ptct$ptct_id %in% nobird_ptct$ptct_id, "sp_detected"] <- 0

### end total species detected per count ---------------------------------------

### merge data frames-----------------------------------------------------------
## PREP for join-- add missing variables to spdet_aru
# create a new df with just weather variables and ptctids
addcols <- spdet_ptct[which(spdet_ptct$aru_sample == TRUE) 
                      ,c("ptct_id", "start_time", "wind", "rain", "noise")]
# join to spdet_aru df to add weather and start time variables
spdet_aru <- left_join(spdet_aru, addcols, by = "ptct_id")

# make aru_sample col
spdet_aru$aru_sample <- TRUE

# actually merge spdet_aru and spdet_ptct
OLDspdet_paired <- full_join(spdet_ptct, spdet_aru, by = NULL)

### end merge dataframes--------------------------------------------------------

### make min_past_sun col that gives the start time as minutes past sunrise-----
# combine month, day, year and sunrise time into one column
sunrise_times$year <- "2019"
sunrise_times$date <- paste(sunrise_times$month, sep = "-", 
                                 sunrise_times$Day)
sunrise_times$date <- paste(sunrise_times$date, sep = "-", 
                                 sunrise_times$year)
sunrise_times$sunrisedt <- paste(sunrise_times$date, sep = " ", 
                                 sunrise_times$Rise)
  
# convert min_past_sun datetime col into a positx value
sunrise_times$sunrisedt <- as.POSIXct(sunrise_times$sunrisedt, 
                                      format='%B-%d-%Y %H:%M:%S')

sunrise_times$date <- as.Date(sunrise_times$date, format='%B-%d-%Y')

# create datetime col in OLDspdet_paired and convert to positx 
OLDspdet_paired$start_time <- paste(OLDspdet_paired$date, sep = " ", 
                                 OLDspdet_paired$start_time)

OLDspdet_paired$start_time <- as.POSIXct(OLDspdet_paired$start_time, 
                                      format= '%Y-%m-%d %H:%M:%S')

# left join so that sunrise times are associated with a date in OLDspdet_paired df
#drop unncessary cols from sunrise_times
sunrise_times <- select(sunrise_times, -c(month, Day, Rise, Set, year))

OLDspdet_paired <- left_join(OLDspdet_paired, sunrise_times, by= "date")


# subtract sunrise time from start time to get min_past_sun
OLDspdet_paired$min_past_sun <- difftime(OLDspdet_paired$start_time, 
                                      OLDspdet_paired$sunrisedt, units = "mins")

OLDspdet_paired <- select(OLDspdet_paired, -c(sunrisedt))

# convert to numeric
OLDspdet_paired$min_past_sun <- as.numeric(OLDspdet_paired$min_past_sun)
 
### end min_past_sun col creation-----------------------------------------------

## additional small clean up chores---------------------------------------------
# get rid of aru_id when count_type is "point"
OLDspdet_paired[which(OLDspdet_paired$count_type == "point"), "aru_id"] <- "none"
 
## end additional small clean up chores-----------------------------------------

### clean up environment
rm(aru, filenames, longaru, widearu, nobird_aruct, nobird_ptct, addcols, 
   spdet_aru, spdet_ptct, sunrise_times)

warning("Data not prepped for Mixed Models as of 30 Aug 2019. See lab notebook 
        from that date for some specific decisions that need to be made.")
