################################
## Species Richness estimates- TEST SCRIPT Point Abbaye 2019
## 
## author: Ellie Roark
## created: 15 Aug 2019
## last modified: 20 Aug 2019
## 
## inputs: *Corrected_PointCounts_PtAbbaye2019.csv- spreadsheet with 
##          observations from in person point counts during the 2019 field
##          season at Point Abbaye
##        
## outputs: *
##            
## TODO: *  
###########################

library(Hmisc)
library(tidyverse)
library(lubridate)
library(vegan)

setwd("/home/emer/Dropbox/Ellie Roark/R/PointAbbaye/")
allptcts <- read_csv(file = "./2019data/Corrected_PointCounts_PtAbbaye2019.csv",
                     col_names = TRUE)

# drop counts with no birds
allptcts <- allptcts[which(allptcts$species_code %nin% 
                             c("no birds")), ]

## get species richness estimate for a single day of point count data with
## abundance -------------------------------------------------------------------

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
testday_bypt <- aggregate(. ~  date + point_id, data = testday_ptctspec, sum)

#total count tallies for each species by date
testday_byday <- select(testday_ptctspec, -c(point_id))
testday_byday <- aggregate(. ~  date, data = testday_byday, sum)

#drop cols with non-numeric data
testday_bypt <- select(testday_bypt, -c(date, point_id))
testday_byday <- select(testday_byday, -c(date))

#use estimateR to get SR estimate for single day abundances
SR_byday <- estimateR(testday_byday, smallsample = TRUE)
SR_byday

#use estimateR to get SR est for 3 points on a single day
SR_bypt <- estimateR(testday_bypt, smallsample = TRUE)
SR_bypt

## end single day pt ct SR estimate--------------------------------------------

## SR estimate for four days of data-------------------------------------------

# subset to four days of data
allptcts$date <- as.Date(allptcts$date, "%m/%d/%Y")
test4day_ptct <- allptcts %>%
  filter(date >= "2019-04-24" & date <= "2019-04-27")

# convert test day data from long to wide
test4day_matrix <- spread(test4day_ptct, species_code, count)

test4day_matrix <- test4day_matrix %>%
  select(date, point_id, AMRO:YBSA)

# make NAs be 0
test4day_matrix[is.na(test4day_matrix)] <- 0

#make count tallies for species numeric
test4day_matrix[, 3:ncol(test4day_matrix)] <- sapply(test4day_matrix[
  , 3:ncol(test4day_matrix)], as.numeric)

#total count tallies for each species by point_id
test4day_bypt <- aggregate(. ~  date + point_id, data = test4day_matrix, sum)

#total count tallies for each species by date
test4day_byday <- select(test4day_matrix, -c(point_id))
test4day_byday <- aggregate(. ~  date, data = test4day_byday, sum)

#drop cols with non-numeric data
test4day_bypt <- select(test4day_bypt, -c(date, point_id))
test4day_byday <- select(test4day_byday, -c(date))

#total count tallies for each species for the full 4 day period
test4day_4day <- colSums(test4day_byday)

#use estimateR to get SR estimate for daily abundances
SR4day_byday <- estimateR(test4day_byday, smallsample = TRUE)
SR4day_byday

#use estimateR to get SR est for each pt on a single day
SR4day_bypt <- estimateR(test4day_bypt, smallsample = TRUE)
SR4day_bypt

#use estimateR to get SR est for full four day period
SR4day_4day <- estimateR(test4day_4day)
SR4day_4day

## end test SR estimate with four days of data----------------------------------


## test SR for full season, daily-----------------------------------------------

# convert test day data from long to wide
allptcts_matrix <- spread(allptcts, species_code, count)

allptcts_matrix <- allptcts_matrix %>%
  select(date, point_id, AMCR:YRWA)

# make NAs be 0
allptcts_matrix[is.na(allptcts_matrix)] <- 0

#make count tallies for species numeric
allptcts_matrix[, 3:ncol(allptcts_matrix)] <- sapply(allptcts_matrix[
  , 3:ncol(allptcts_matrix)], as.numeric)

#total count tallies for each species by point_id
allptcts_bypt <- aggregate(. ~  date + point_id, data = allptcts_matrix, sum)

#total count tallies for each species by date
allptcts_byday <- select(allptcts_matrix, -c(point_id))
allptcts_byday <- aggregate(. ~  date, data = allptcts_byday, sum)

#drop cols with non-numeric data
allptcts_byday <- select(allptcts_byday, -c(date))

#get estimateR SR est for each day
SRall_byday <- estimateR(allptcts_byday)
SRall_byday


## end test SRdaily for full season --------------------------------------------

## SR for the full season ------------------------------------------------------

# # get species codes with count numbers to do the math by hand
# allptcts$count <- as.numeric(as.character(allptcts$count)) 
# 
# specobs_all <- allptcts %>% 
#   group_by(species_code) %>% 
#   summarise(count = sum(count))
# 
# # get species where there are only 1 or 2 obs
# singleton_all <- specobs_all[which(specobs_all$count == 1), ]
# doubleton_all <- specobs_all[which(specobs_all$count == 2), ]
 
# add up point counts tallied by day so that I have one value for each species
# for the entire season
allptcts_totals <- colSums(allptcts_byday)

# apply estimateR to full season
SRall_total <- estimateR(allptcts_totals)
SRall_total
 
## end SR for the full season -------------------------------------------------




