################################
## StopoverHabitatMonitoring.R
## 
## author: Ellie Roark & Willson Gaul
## contact: ellieroark@gmail.com
## created: 15 Jan 2020
## last modified: 20 June 2020
## 
## This script organizes the workflow for the ARU vs Point Counts in migration
## stopover habitat analysis.
##            
## TODO: * 
################################

set.seed(1542020) #set seed: 15 Apr 2020

### Species Richness Analyses--------------------------------------------------

# script reads in original d  ata for 10 minute point counts and 10 minute aru 
# counts and creates spdet_paired dataframe, which tallies the number of species
# detected per count for both ARU and point counts. 
source("./DataPrep_SR~counttype_10mincts_2019.R")

# script identifies duplicated 10 min ARU counts and randomly selects one of the 
# duplicates to include in the spdet_paired dataframe.
# 
## MUST FIRST RUN DataPrep_SR~counttype_10mincts_2019.R
source("./ARUDuplicateReview2019.R")

# script reads in original data for 20 random minutes of aru listening and 
# tallies the number of species detected in each 20 random minute count
# 
# MUST FIRST RUN DataPrep_SR~counttype_10mincts_2019.R
source("./DataPrep_SR~counttype_20randmin.R")

# ONLY NEED TO RUN ONCE TO GENERATE TABLE
# script creates table of species detected during field season by detection 
# method
# 
## MUST FIRST RUN DataPrep_SR~counttype_10mincts_2019.R
## AND ARUDuplicateReview2019.R
source("./SpeciesDiversity~counttype_2019.R")

# script subsets spdet_paired to only paired point counts and aru counts, and 
# creates spdet_all, which contains ALL counts from the season (including point
# counts with no accompanying ARU count). script then fits poisson GLM and 
# poisson GLMM to model species richness, with count type, date, time and weather
# as predictors
# 
## MUST FIRST RUN DataPrep_SR~counttype_10mincts_2019.R
## AND ARUDuplicateReview2019.R
source("./GLMM_SR_10mincts_2019.R")


# script that fits poisson GLMM to model species richness by count type with 
# THREE count types- aru (10 consecutive min aru cts) point (10 min in person 
# point counts) and arurand (20 random minutes from each recorder on each day)
# 
## MUST FIRST RUN DataPrep_SR~counttype_10mincts_2019.R
## AND ARUDuplicateReview2019.R
## AND DataPrep_SR~counttype_20randmin.R
source("./GLMM_SR_20randommin.R")

### End Species Richness Analyses-----------------------------------------------


### Abundance Analyses----------------------------------------------------------

# script that cleans point count data to prep it for abundance models
# 
## MUST FIRST RUN [nothing]
source("./DataPrep_PtCtAbundance.R")

# script that cleans aru data to prep it for abundance models
# 
## MUST FIRST RUN DataPrep_PtCtAbundance.R
source("./DataPrep_ARUabundance.R")

# script that runs boosted regression trees to model abundance of GCKI and WIWR
# 
## MUST FIRST RUN DataPrep_PtCtAbundance.R
## AND DataPrep_ARUabundance.R
source("./BRT_abundance.R")

# run generalized additive model to for abundance of GCKI and WIWR
# 
## MUST FIRST RUN DataPrep_PtCtAbundance.R
## AND DataPrep_ARUabundance.R
## AND BRT_Abundance.R
source("./GAM_abundance.R")


### End Abundance Analyses------------------------------------------------------


### Plots-----------------------------------------------------------------------

# script creates plots for StopoverHabitatMonitoring Manuscript
# 
## MUST FIRST RUN DataPrep_SR~counttype_10mincts_2019.R
## AND ARUDuplicateReview2019.R
## AND DataPrep_SR~counttype_20randmin.R
## AND GLMM_SR_10mincts_2019.R
## AND GLMM_SR_20randommin.R
## AND DataPrep_PtCtAbundance.R
## AND DataPrep_ARUabundance.R
## AND BRT_Abundance.R
source("./plots_yellowpaper_PtAbbaye2019.R")

# script creates plots for supplementary materials
#
## MUST FIRST RUN DataPrep_SR~counttype_10mincts_2019.R
## AND ARUDuplicateReview2019.R
## AND DataPrep_SR~counttype_20randmin.R
## AND GLMM_SR_10mincts_2019.R
## AND GLMM_SR_20randommin.R
## AND DataPrep_PtCtAbundance.R
## AND DataPrep_ARUabundance.R
## AND BRT_Abundance.R
## AND GAM_Abundance.R
## AND plots_yellowpaper_PtAbbaye2019.R
source("./suppmaterials_plots_yellowpaper.R")

### End Plots-------------------------------------------------------------------
