################################
## Species Richness estimates- Point Abbaye 2019
## 
## author: Ellie Roark
## created: 15 Aug 2019
## last modified: 15 Aug 2019
## 
## inputs: *Corrected_PointCounts_PtAbbaye2019.csv- spreadsheet with 
##          observations from in person point counts during the 2019 field
##          season at Point Abbaye
##        
## outputs: *
##            
## TODO: * COLLAPSE rows in wide data frameso that for the single day test each 
##          ptct has its own row with all species occurences in it. 
###########################

library(Hmisc)
library(tidyverse)
library(lubridate)
library(vegan)

setwd("home/emer/Dropbox/Ellie Roark/R/PointAbbaye/")
allptcts <- read_csv(file = "./2019data/Corrected_PointCounts_PtAbbaye2019.csv",
                     col_names = TRUE)

## get species richness estimate for a single day of point count data ---------

# subset to a single day of data
testday_ptct <- allptcts[which(allptcts$date == "04/24/2019"), ]

# convert test day data from long to wide
testday_ptctspec <- spread(testday_ptct, species_code, count)

testday_ptctspec <- testday_ptctspec %>%
    select(date, point_id, AMRO:YBSA)

# make NAs be 0
testday_ptctspec[is.na(testday_ptctspec)] <- 0

#make count tallies for species numeric
testday_ptctspec[, 3:ncol(testday_ptctspec)] <- sapply(testday_ptctspec[
  , 3:ncol(testday_ptctspec)], as.numeric)

#total count tallies for each species by point_id
# this code has the right idea but only for one species-- and it remakes the df
# so that the other species no longer exist in it
# possible solutions: create a bunch of data frames and merge??
#                     use base R (aggregate or something else?)
testday_ptctspec <- testday_ptctspec %>%
  group_by(point_id, date) %>%
  summarise(sumAMRO = sum(AMRO))



# make a test data frame with total counts for all species on single date
test <- testday_ptctspec[1, ]
test[, 2:ncol(test)] <- colSums(testday_ptctspec[, 2:ncol(testday_ptctspec)])

#use estimateR to get SR estimate for single day abundances
SR_1day <- estimateR(test, smallsample = TRUE)

# collapse rows so that each date is only one row
# testday_ptctspec <- testday_ptctspec %>%
#   select(date, point_id, lat, long, start_time, end_time, aru_sample, aru_id,
#          cloud_cover, wind, rain, noise, ct_bearing) %>%
#   group_by(date)
#   summarise()


#create separate dfs for metadata and numeric count data for each species
# nonnum_cols <- c("date", "point_id", "lat", "long", "start_time", "end_time",
#                    "aru_sample", "aru_id", "cloud_cover", "wind", "rain",
#                    "noise", "ct_bearing", "species_common_name", "det_code", 
#                    "rel_bearing", "distance", "minute_detected", "comments")
# 
# metadata_cols <- testday_ptctspec[ , which(colnames(testday_ptctspec) %in% 
#                                              nonnum_cols)]
# 
# testday_ptctspec <- testday_ptctspec[ , which(colnames(testday_ptctspec) %nin% 
#                                              nonnum_cols)]
# 
# #get rid of NAs in species dataframe 
# testday_ptctspec[is.na(testday_ptctspec)] <- 0
# 
# #make df numeric
# testday_ptctspec <- as.data.frame(sapply(testday_ptctspec, as.numeric))


#test species richness for single day
SRest <- with(metadata_cols, specpool(testday_ptctspec, smallsample = TRUE))
SRest
  
## end single day pt ct SR estimate--------------------------------------------

## SR estimate for four days of data-------------------------------------------

# subset to four days of data
allptcts$date <- as.Date(allptcts$date, "%m/%d/%Y")
test4day_ptct <- allptcts %>%
  filter(date >= "2019-04-24" & date <= "2019-04-27")

# make count col numeric
test4day_ptct$count <- as.numeric(as.character(test4day_ptct$count))

# total the number of observations for each sp in this df
specobs_test4day <- test4day_ptct %>% 
  group_by(species_code) %>% 
  summarise(count = sum(count))

## end test SR estimate with four days of data----------------------------------


## SR for the full season ------------------------------------------------------

# get species codes with count numbers to do the math by hand
allptcts$count <- as.numeric(as.character(allptcts$count)) 

specobs_all <- allptcts %>% 
  group_by(species_code) %>% 
  summarise(count = sum(count))

# get species where there are only 1 or 2 obs
singleton_all <- specobs_all[which(specobs_all$count == 1), ]
doubleton_all <- specobs_all[which(specobs_all$count == 2), ]
 
## end SR for the full season -------------------------------------------------




