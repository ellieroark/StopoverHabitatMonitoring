################################
## Descriptive analysis for final report to Copper Country Audubon
## 
## author: Ellie Roark
## created: 21 July 2018 (mostly)
## last modified: 14 Sept 2018
## 
## inputs: *DataPrep_SR~counttype_10mincts_2019.R - reads in aru and point count
##          data and returns dataframes with # of species detected
##         
## outputs: *
################################

library(Hmisc)
library(tidyverse)

setwd("/home/emer/Dropbox/Ellie Roark/R/PointAbbaye/")
source("./SRModels/SRModels/DataPrep_SR~counttype_10mincts_2019.R")

##total number of species detected over the whole season
length(unique(allaru$species_code))
length(unique(ptct$species_code))
length(unique(allaru$species_code %nin% ptct$species_code))

##total number of point count observations per species
as.numeric(ptct$count)
obs_per_sp <- ptct %>% 
  group_by(species_code) %>% 
  summarise(count = sum(n()))


