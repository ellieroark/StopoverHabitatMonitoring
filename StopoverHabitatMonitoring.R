################################
## StopoverHabitatMonitoring.R
## 
## author: Ellie Roark
## created: 15 Jan 2020
## last modified: 15 Jan 2020
## 
## This script organizes the workflow for the ARU vs Point Counts in migration
## stopover habitat analysis.
##            
## TODO: * 
################################

setwd("/home/emer/Dropbox/Ellie Roark/R/PointAbbaye/")


# script reads in original data for 10 minute point counts and 10 minute aru 
# counts and creates spdet_paired dataframe, which tallies the number of species
# detected per count for both ARU and point counts. 
source("./StopoverHabitatMonitoring/DataPrep_SR~counttype_10mincts_2019.R")

# script identifies duplicated 10 min ARU counts and randomly selects one of the 
# duplicates to include in the spdet_paired dataframe.
# 
## MUST FIRST RUN DataPrep_SR~counttype_10mincts_2019.R
source("./StopoverHabitatMonitoring/ARUDuplicateReview2019.R")

# script creates table of species detected during field season by detection 
# method
# 
## MUST FIRST RUN DataPrep_SR~counttype_10mincts_2019.R
## AND ARUDuplicateReview2019.R
source("./StopoverHabitatMonitoring/SpeciesDiversity~counttype_2019.R")

# script subsets spdet_paired to only paired point counts and aru counts, and 
# creates spdet_all, which contains ALL counts from the season (including point
# counts with no accompanying ARU count). script then fits poisson GLM and 
# poisson GLMM to model species richness, with count type, date, time and weather
# as predictors
# 
## MUST FIRST RUN DataPrep_SR~counttype_10mincts_2019.R
## AND ARUDuplicateReview2019.R
source("./StopoverHabitatMonitoring/GLMM_SR_10mincts_2019.R")


# script creates plots for StopoverHabitatMonitoring Manuscript
# 
## MUST FIRST RUN DataPrep_SR~counttype_10mincts_2019.R
## AND ARUDuplicateReview2019.R
## AND GLMM_SR_10mincts_2019.R
source("./StopoverHabitatMonitoring/plots_yellowpaper_PtAbbaye2019.R")
