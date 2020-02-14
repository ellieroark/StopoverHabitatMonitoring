################################
## Abundance estimation for Point Counts
## Point Abbaye 2019 data
## 
## author: Ellie Roark
## created: 10 Feb 2020
## last modified: 11 Feb 2020
## 
## inputs: *MUST FIRST RUN DataPrep_SR~counttype_10mincts_2019.R
##          AND ARUDuplicateReview2019.R
##          
##
##         
## outputs: *
##            
## TODO: * 
################################
library(Hmisc)
library(tidyverse)
library(lubridate)

ptct <- read_csv(file = "./2019data/Corrected_PointCounts_PtAbbaye2019.csv")

ptct$count <- as.numeric(ptct$count)
ptct$date <- as.Date(ptct$date, format = "%m/%d/%Y")

# make day of yr col for ptct obs
ptct$day_of_yr <- yday(ptct$date)
# make pt ct id column
ptct$ptct_id <- paste(as.character(ptct$date), ptct$point_id, sep = "_")

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


## Exploratory plots -----------------------------------------------------------
hist(ptct$count, main = "number of individs")

#subset to only sp observations of WIWR
wiwr <- ptct[which(ptct$species_code == "WIWR"), ]

#subset to ybsa
ybsa <- ptct[which(ptct$species_code == "YBSA"), ]

#subset to Bcch
bcch <- ptct[which(ptct$species_code == "BCCH"), ]

#subset to heth
heth <- ptct[which(ptct$species_code == "HETH"), ]

#subset to gcki
gcki <- ptct[which(ptct$species_code == "GCKI"), ]

# plot wren counts over time
plot(wiwr$day_of_yr, wiwr$count, 
                  main = "wiwr per pt count over time")

plot(ybsa$day_of_yr, ybsa$count, 
                  main = "ybsa per pt count over time")

plot(bcch$day_of_yr, bcch$count, 
     main = "bcch per pt count over time")

plot(heth$day_of_yr, heth$count, 
     main = "heth per pt count over time")

plot(gcki$day_of_yr, gcki$count, 
     main = "gcki per pt count over time")

# add up number of individs detected per day for each 
sum_wiwr <- wiwr %>% 
  group_by(day_of_yr) %>% 
  summarise(count = n())

sum_ybsa <- ybsa %>% 
  group_by(day_of_yr) %>% 
  summarise(count = n())

sum_bcch <- bcch %>% 
  group_by(day_of_yr) %>% 
  summarise(count = n())

sum_heth <- heth %>% 
  group_by(day_of_yr) %>% 
  summarise(count = n())

sum_gcki <- gcki %>% 
  group_by(day_of_yr) %>% 
  summarise(count = n())

plot(sum_gcki$day_of_yr, sum_gcki$count, 
     main = "gcki per day over time")
plot(sum_ybsa$day_of_yr, sum_ybsa$count, 
     main = "ybsa per day over time")
plot(sum_wiwr$day_of_yr, sum_wiwr$count, 
     main = "wiwr per day over time")
plot(sum_bcch$day_of_yr, sum_bcch$count, 
     main = "bcch per day over time")
plot(sum_heth$day_of_yr, sum_heth$count, 
     main = "heth per day over time")

## end Exploratory plots -------------------------------------------------------
