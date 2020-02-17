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
#subset to gcki
heth <- ptct[which(ptct$species_code == "HETH"), ]

#get unique ptctids for counts on which there were no GCKIs
nohethct <- ptct[which(ptct$ptct_id %nin% heth$ptct_id), ]

#get rid of species-specific variables
dropa <- c("species_code","species_common_name", "minute_detected", "det_code", 
           "rel_bearing", "distance", "minute_detected", "comments", "count")
nohethct <- nohethct[ , !(names(nohethct) %in% dropa)]

# get only a single row for each point count
nohethct <- dplyr::distinct(nohethct)

# add count variable back in, specifying that these are counts with 0 GCKIs
nohethct$count <- 0

# get rid of species-specific variables in gcki df
dropb <- c("species_code","species_common_name", "minute_detected", "det_code", 
           "rel_bearing", "distance", "minute_detected", "comments")
heth <- heth[ , !(names(heth) %in% dropb)]

# join gcki and nogckict dfs to create a dataframe that has all point counts 
# with the number of GCKIs detected 
heth <- full_join(heth, nohethct)

## end create HETH df-----------------------------------------------------------

## Exploratory plots -----------------------------------------------------------
hist(ptct$count, main = "number of individs")


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

# #TODO fix so this actually sums the count column
#  add up number of individs detected per day for each 
# sum_wiwr <- wiwr %>% 
#   group_by(day_of_yr) %>% 
#   sum(count)
# 
# sum_ybsa <- ybsa %>% 
#   group_by(day_of_yr) %>% 
#   summarise(count = n())
# 
# sum_bcch <- bcch %>% 
#   group_by(day_of_yr) %>% 
#   summarise(count = n())
# 
# sum_heth <- heth %>% 
#   group_by(day_of_yr) %>% 
#   summarise(count = n())
# 
# sum_gcki <- gcki %>% 
#   group_by(day_of_yr) %>% 
#   summarise(count = n())

# plot(sum_gcki$day_of_yr, sum_gcki$count, 
#      main = "gcki per day over time")
# plot(sum_ybsa$day_of_yr, sum_ybsa$count, 
#      main = "ybsa per day over time")
# plot(sum_wiwr$day_of_yr, sum_wiwr$count, 
#      main = "wiwr per day over time")
# plot(sum_bcch$day_of_yr, sum_bcch$count, 
#      main = "bcch per day over time")
# plot(sum_heth$day_of_yr, sum_heth$count, 
#      main = "heth per day over time")

## end Exploratory plots -------------------------------------------------------

## GLM for GCKI counts over time------------------------------------------------
## prep data frame ensuring all variables are factors
#change wind from dbl to factor with 3 levels (0-1, 2, 3+)
gcki[which(gcki$wind <= "1"), "wind"] <- "0-1"
gcki[which(gcki$wind >= "3"), "wind"] <- "3+"
gcki$wind <- factor(as.character(gcki$wind), 
                            levels = c("0-1", "2", "3+"))

#change rain into factor with two groups, wet and dry
gcki[which(gcki$rain == "Rain/Snow"), "rain"] <- "wet"
gcki[which(gcki$rain == "Drizzle"), "rain"] <- "wet"
gcki[which(gcki$rain == "Fog"), "rain"] <- "wet"
gcki$rain <- factor(as.character(gcki$rain), 
                            levels = c("Dry", "wet"))

#change noise into factor variable with three groups: 0, 1, >2
gcki[which(gcki$noise >= 2), "noise"] <- ">2"
gcki$noise <- factor(as.character(gcki$noise), 
                             levels = c("0", "1", ">2"))

# make a centered day of year and a squared day of year
gcki$day_of_yr_c <- gcki$day_of_yr - mean(gcki$day_of_yr)
gcki$day_sq <- gcki$day_of_yr_c^2

## model for the number of GCKI detected per point count, depending on weather
gcki.ct <- glm(count ~ 1 + wind + rain + noise + day_of_yr_c + day_sq,
                data = gcki, 
                family = "poisson")

summary(gcki.ct)
plot(gcki.ct)
