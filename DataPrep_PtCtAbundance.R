################################
## Data Prep for Abundance estimation for Point Counts
## Point Abbaye 2019 data
## 
## author: Ellie Roark
## created: 10 Feb 2020
## last modified: 06 Jan 2021
## 
## inputs: *Corrected_PointCounts_PtAbbaye2019.csv- original data file with all 
##           point count data from the 2019 field season at Point Abbaye
##         *SunriseTimes_PtAbbaye2019.csv- csv with sunrise times from the US
##           Navy (see citation list for details)          
##           
## outputs: *
##            
## TODO: * 
################################
library(Hmisc)
library(tidyverse)
library(lubridate)
library(car)
library(MASS)
library(gbm)
library(data.table)
library(mgcv)

#set.seed(28022020) # no longer setting seed because I set it in main workflow
# script

ptct <- read_csv(file = "./data/Corrected_PointCounts_PtAbbaye2019.csv")
sunrise_times <- read_csv(file = "./data/SunriseTimes_PtAbbaye2019.csv")

ptct$count <- as.numeric(ptct$count)
ptct$date <- as.Date(ptct$date, format = "%m/%d/%Y")

# make day of yr col for ptct obs
ptct$day_of_yr <- yday(ptct$date)
# make pt ct id column
ptct$ptct_id <- paste(as.character(ptct$date), ptct$point_id, sep = "_")

## prep data frames
# make wind df to average windspeed for each day to use in day-aggregate models
windday <- ptct %>%
        dplyr::select(wind, day_of_yr, ptct_id)

windday <- distinct(windday)

#create new col windspeed in knots
windday$windknots <- NA


#convert beaufort wind speed ratings to the median speed (in knots) for each
# beaufort score and make that the value for the "windknots" column
windday[which(windday$wind == "0"), "windknots"] <- "0"
windday[which(windday$wind == "1"), "windknots"] <- "2"
windday[which(windday$wind == "2"), "windknots"] <- "5"
windday[which(windday$wind == "3"), "windknots"] <- "8.5"
windday[which(windday$wind == "4"), "windknots"] <- "13.5"
windday[which(windday$wind == "5"), "windknots"] <- "19"

windday$windknots <- as.numeric(windday$windknots)

#compute the average windspeed in knots per day and the median Beaufort force
# per day
windday <- windday %>%
        dplyr::select(wind, windknots, day_of_yr) %>%
        group_by(day_of_yr) %>%
        summarise(windknots = mean(windknots), wind = median(wind))

#fix median values for day of yr 110 and day of yr 120 (which have invalid
# Beaufort forces of 1.5 and 2.5) by rounding up to the nearest beaufort force
windday[which(windday$day_of_yr == 110), "wind"] <- 2
windday[which(windday$day_of_yr == 120), "wind"] <- 3

#change wind to numeric with 3 levels (1 (for values 0-1), 2 (for 
# value 2), 3 (for values 3+))
windday[which(windday$wind <= "1"), "wind"] <- "1"
windday[which(windday$wind >= "3"), "wind"] <- "3"
windday$wind <- as.numeric(windday$wind)

# add centered and squared day of year cols to windday df 
windday$day_of_yr_c <- windday$day_of_yr - mean(windday$day_of_yr)
windday$day_sq <- windday$day_of_yr_c^2

#change rain into factor with two groups, wet and dry
ptct[which(ptct$rain == "Rain/Snow"), "rain"] <- "wet"
ptct[which(ptct$rain == "Drizzle"), "rain"] <- "wet"
ptct[which(ptct$rain == "Fog"), "rain"] <- "wet"
ptct$rain <- factor(as.character(ptct$rain), 
                    levels = c("Dry", "wet"))

#change noise into factor variable with three groups: 0, 1, >2
ptct[which(ptct$noise >= 2), "noise"] <- ">2"
ptct$noise <- factor(as.character(ptct$noise), 
                     levels = c("0", "1", ">2"))

#make cloud cover a factor variable
ptct$cloud_cover <- factor(as.character(ptct$cloud_cover), 
                    levels = c("0-33", "33-66", "66-100"))

# make a centered day of year and a squared day of year
ptct$day_of_yr_c <- ptct$day_of_yr - mean(ptct$day_of_yr)
ptct$day_sq <- ptct$day_of_yr_c^2

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

# make start_time in ptct into a datetime col with positx structure
ptct$start_time <- paste(ptct$date, sep = " ", 
                                 ptct$start_time)

ptct$start_time <- as.POSIXct(ptct$start_time, 
                                      format= '%Y-%m-%d %H:%M:%S')

# left join so that sunrise times are associated with a date in ptct df
#drop unncessary cols from sunrise_times
sunrise_times <- dplyr::select(sunrise_times, -c(month, Day, Rise, Set, year))

ptct <- left_join(ptct, sunrise_times, by= "date")


# subtract sunrise time from start time to get min_past_sun
ptct$min_past_sun <- difftime(ptct$start_time, 
                                      ptct$sunrisedt, units = "mins")

ptct <- dplyr::select(ptct, -c(sunrisedt))

# convert to numeric
ptct$min_past_sun <- as.numeric(ptct$min_past_sun)


## Create dataframe of Golden-crowned Kinglet counts----------------------------
#subset to gcki
gcki <- ptct[which(ptct$species_code == "GCKI"), ]

#get unique ptctids for counts on which there were no GCKIs
nogckict <- ptct[which(ptct$ptct_id %nin% gcki$ptct_id), ]

#get rid of species-specific variables
dropa <- c("species_code","species_common_name", "minute_detected", "det_code", 
             "rel_bearing", "distance", "minute_detected", "comments", "count")
nogckict <- nogckict[ , !(names(nogckict) %in% dropa)]

# get only a single row for each point count
nogckict <- dplyr::distinct(nogckict)

# add count variable back in, specifying that these are counts with 0 GCKIs
nogckict$count <- 0

# get rid of species-specific variables in gcki df
dropb <- c("species_code","species_common_name", "minute_detected", "det_code", 
           "rel_bearing", "distance", "minute_detected", "comments")
gcki <- gcki[ , !(names(gcki) %in% dropb)]

# join gcki and nogckict dfs to create a dataframe that has all point counts 
# with the number of GCKIs detected 
gcki <- full_join(gcki, nogckict)

## end create GCKI dataframe----------------------------------------------------

## create df for wiwr-----------------------------------------------------------
#subset to wiwr
wiwr <- ptct[which(ptct$species_code == "WIWR"), ]

#get unique ptctids for counts on which there were no WIWRs
nowiwrct <- ptct[which(ptct$ptct_id %nin% wiwr$ptct_id), ]

#get rid of species-specific variables
dropa <- c("species_code","species_common_name", "minute_detected", "det_code", 
           "rel_bearing", "distance", "minute_detected", "comments", "count")
nowiwrct <- nowiwrct[ , !(names(nowiwrct) %in% dropa)]

# get only a single row for each point count
nowiwrct <- dplyr::distinct(nowiwrct)

# add count variable back in, specifying that these are counts with 0 WIWRs
nowiwrct$count <- 0

# get rid of species-specific variables in wiwr df
dropb <- c("species_code","species_common_name", "minute_detected", "det_code", 
           "rel_bearing", "distance", "minute_detected", "comments")
wiwr <- wiwr[ , !(names(wiwr) %in% dropb)]

# join wiwr and nowiwrct dfs to create a dataframe that has all point counts 
# with the number of WIWRs detected 
wiwr <- full_join(wiwr, nowiwrct)

## end create wiwr df-----------------------------------------------------------

## create dataframes with average # of individs. per pt ct each day, instead of 
## just  # of individuals per count---------------------------------------------
#  add up number of individs detected per day for each 
sum_wiwr <- wiwr %>%
        group_by(day_of_yr) %>%
        summarize(resp = mean(count), count = sum(count))

sum_gcki <- gcki %>%
        group_by(day_of_yr) %>%
        summarize(resp = mean(count), count = sum(count))



# add windspeed rating in knots to sum_ dfs. 
sum_gcki <- left_join(sum_gcki, windday, by = "day_of_yr")
sum_wiwr <- left_join(sum_wiwr, windday, by = "day_of_yr")

### for loop that loops through all species in ptct and creates a "sum_[sp. code]"
### dataframe for each species
# create a list to hold a data frame for each species
sum_species_dfs <- list()
sp_codes <- unique(ptct$species_code)
sp_codes <- sp_codes[!grepl(". .", sp_codes)]
for(i in 1:length(sp_codes)) {
        this_sp <- sp_codes[i]
        
        #subset to this species
        sp_df <- ptct[which(ptct$species_code == this_sp), ]
        
        #get unique ptctids for counts on which there were no this_sp
        nospct <- ptct[which(ptct$ptct_id %nin% sp_df$ptct_id), ]
        
        #get rid of species-specific variables
        dropa <- c("species_code","species_common_name", "minute_detected", "det_code", 
                   "rel_bearing", "distance", "minute_detected", "comments", "count")
        nospct <- nospct[ , !(names(nospct) %in% dropa)]
        
        # get only a single row for each point count
        nospct <- dplyr::distinct(nospct)
        
        # add count variable back in, specifying that these are counts with 0 this_sp
        nospct$count <- 0
        
        # get rid of species-specific variables in sp_df
        dropb <- c("species_code","species_common_name", "minute_detected", "det_code", 
                   "rel_bearing", "distance", "minute_detected", "comments")
        sp_df <- sp_df[ , !(names(sp_df) %in% dropb)]
        
        # join sp_df and nospct dfs to create a dataframe that has all point counts 
        # with the number of this_sp detected 
        sp_df <- full_join(sp_df, nospct)
        
        #  summarize number of individs detected per day
        sum_sp <- sp_df %>%
                group_by(day_of_yr) %>%
                summarize(resp = mean(count), count = sum(count))
        
        # add windspeed rating in knots to sum_ dfs. 
        sum_sp <- left_join(sum_sp, windday, by = "day_of_yr")
        sum_species_dfs[[i]] <- sum_sp # add species df to list
}
names(sum_species_dfs) <- sp_codes

## end create # of individs per day dfs-----------------------------------------



## remove no....ct dfs, now that that data is incorporated into each species df
rm(nogckict, nowiwrct, dropa, dropb, sunrise_times, ptct)


