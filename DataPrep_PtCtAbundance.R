################################
## Data Prep for Abundance estimation for Point Counts
## Point Abbaye 2019 data
## 
## author: Ellie Roark
## created: 10 Feb 2020
## last modified: 6 Mar 2020
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

rm(list = ls())
set.seed(28022020) # 28 Feb 2020

ptct <- read_csv(file = "./2019data/Corrected_PointCounts_PtAbbaye2019.csv")
sunrise_times <- read_csv(file = "./2019data/SunriseTimes_PtAbbaye2019.csv")

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

#create new col that holds beaufort wind force, so that "wind" col can hold
# windspeed in knots
windday$beaufort <- windday$wind


#convert beaufort wind speed ratings to the median speed (in knots) for each 
# beaufort score and make that the value for the "wind" column
windday[which(windday$beaufort == "0"), "wind"] <- "0"
windday[which(windday$beaufort == "1"), "wind"] <- "2"
windday[which(windday$beaufort == "2"), "wind"] <- "5"
windday[which(windday$beaufort == "3"), "wind"] <- "8.5"
windday[which(windday$beaufort == "4"), "wind"] <- "13.5"
windday[which(windday$beaufort == "5"), "wind"] <- "19"

windday$wind <- as.numeric(windday$wind)

#compute the average windspeed in knots per day
windday <- windday %>%
        dplyr::select(wind, day_of_yr) %>%
        group_by(day_of_yr) %>%
        summarise(wind = mean(wind))

#change wind from dbl to factor with 3 levels (0-1, 2, 3+) for use in all other
# models
ptct[which(ptct$wind <= "1"), "wind"] <- "0-1"
ptct[which(ptct$wind >= "3"), "wind"] <- "3+"
ptct$wind <- factor(as.character(ptct$wind), 
                    levels = c("0-1", "2", "3+"))

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

## create HETH df---------------------------------------------------------------
#subset to HETH
heth <- ptct[which(ptct$species_code == "HETH"), ]

#get unique ptctids for counts on which there were no HETHs
nohethct <- ptct[which(ptct$ptct_id %nin% heth$ptct_id), ]

#get rid of species-specific variables
dropa <- c("species_code","species_common_name", "minute_detected", "det_code", 
           "rel_bearing", "distance", "minute_detected", "comments", "count")
nohethct <- nohethct[ , !(names(nohethct) %in% dropa)]

# get only a single row for each point count
nohethct <- dplyr::distinct(nohethct)

# add count variable back in, specifying that these are counts with 0 HETHs
nohethct$count <- 0

# get rid of species-specific variables in heth df
dropb <- c("species_code","species_common_name", "minute_detected", "det_code", 
           "rel_bearing", "distance", "minute_detected", "comments")
heth <- heth[ , !(names(heth) %in% dropb)]

# join heth and nohethct dfs to create a dataframe that has all point counts 
# with the number of HETHs detected 
heth <- full_join(heth, nohethct)

## end create HETH df-----------------------------------------------------------

## create BCCH df---------------------------------------------------------------
#subset to BCCH
bcch <- ptct[which(ptct$species_code == "BCCH"), ]

#get unique ptctids for counts on which there were no BCCHs
nobcchct <- ptct[which(ptct$ptct_id %nin% bcch$ptct_id), ]

#get rid of species-specific variables
dropa <- c("species_code","species_common_name", "minute_detected", "det_code", 
           "rel_bearing", "distance", "minute_detected", "comments", "count")
nobcchct <- nobcchct[ , !(names(nobcchct) %in% dropa)]

# get only a single row for each point count
nobcchct <- dplyr::distinct(nobcchct)

# add count variable back in, specifying that these are counts with 0 BCCHs
nobcchct$count <- 0

# get rid of species-specific variables in bcch df
dropb <- c("species_code","species_common_name", "minute_detected", "det_code", 
           "rel_bearing", "distance", "minute_detected", "comments")
bcch <- bcch[ , !(names(bcch) %in% dropb)]

# join bcch and nobcchct dfs to create a dataframe that has all point counts 
# with the number of BCCHs detected 
bcch <- full_join(bcch, nobcchct)

## end create BCCH df-----------------------------------------------------------

## create YBSA df---------------------------------------------------------------
#subset to ybsa
ybsa <- ptct[which(ptct$species_code == "YBSA"), ]

#get unique ptctids for counts on which there were no YBSAs
noybsact <- ptct[which(ptct$ptct_id %nin% ybsa$ptct_id), ]

#get rid of species-specific variables
dropa <- c("species_code","species_common_name", "minute_detected", "det_code", 
           "rel_bearing", "distance", "minute_detected", "comments", "count")
noybsact <- noybsact[ , !(names(noybsact) %in% dropa)]

# get only a single row for each point count
noybsact <- dplyr::distinct(noybsact)

# add count variable back in, specifying that these are counts with 0 YBSAs
noybsact$count <- 0

# get rid of species-specific variables in ybsa df
dropb <- c("species_code","species_common_name", "minute_detected", "det_code", 
           "rel_bearing", "distance", "minute_detected", "comments")
ybsa <- ybsa[ , !(names(ybsa) %in% dropb)]

# join ybsa and noybsact dfs to create a dataframe that has all point counts 
# with the number of YBSAs detected 
ybsa <- full_join(ybsa, noybsact)

## end create YBSA df----------------------------------------------------------- 

## create dataframes with # of individs. per day, instead of per count----------
#  add up number of individs detected per day for each 
sum_wiwr <- wiwr %>%
        group_by(day_of_yr) %>%
        summarize(count = sum(count))

sum_ybsa <- ybsa %>%
        group_by(day_of_yr) %>%
        summarize(count = sum(count))

sum_bcch <- bcch %>%
        group_by(day_of_yr) %>%
        summarize(count = sum(count))

sum_heth <- heth %>%
        group_by(day_of_yr) %>%
        summarize(count = sum(count))
sum_gcki <- gcki %>%
        group_by(day_of_yr) %>%
        summarize(count = sum(count))

# add centered and squared day of year cols to windday df before joining to 
# sum_dfs
windday$day_of_yr_c <- windday$day_of_yr - mean(windday$day_of_yr)
windday$day_sq <- windday$day_of_yr_c^2



# add windspeed rating in knots to sum_ dfs. 
sum_gcki <- left_join(sum_gcki, windday, by = "day_of_yr")
sum_wiwr <- left_join(sum_wiwr, windday, by = "day_of_yr")
sum_bcch <- left_join(sum_bcch, windday, by = "day_of_yr")
sum_ybsa <- left_join(sum_ybsa, windday, by = "day_of_yr")
sum_heth <- left_join(sum_heth, windday, by = "day_of_yr")

## end create # of individs per day dfs-----------------------------------------

## remove no....ct dfs, now that that data is incorporated into each species df
rm(nobcchct, nogckict, nowiwrct, noybsact, nohethct, dropa, dropb, 
   sunrise_times)

rm(bcch, heth, ptct, sum_bcch, sum_heth, sum_ybsa, ybsa)

