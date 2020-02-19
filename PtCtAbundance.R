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
library(car)
library(MASS)
library(gbm)

setwd("/home/emer/Dropbox/Ellie Roark/R/PointAbbaye/")

ptct <- read_csv(file = "./2019data/Corrected_PointCounts_PtAbbaye2019.csv")
sunrise_times <- read_csv(file = "./2019data/SunriseTimes_PtAbbaye2019.csv")

ptct$count <- as.numeric(ptct$count)
ptct$date <- as.Date(ptct$date, format = "%m/%d/%Y")

# make day of yr col for ptct obs
ptct$day_of_yr <- yday(ptct$date)
# make pt ct id column
ptct$ptct_id <- paste(as.character(ptct$date), ptct$point_id, sep = "_")

## prep data frame ensuring all variables are factors
#change wind from dbl to factor with 3 levels (0-1, 2, 3+)
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

## remove no....ct dfs, now that that data is incorporated into each species df
rm(nobcchct, nogckict, nowiwrct, noybsact, nohethct, dropa, dropb, 
   sunrise_times)

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
## model for the number of GCKI detected per point count, depending on weather
gcki.ct <- glm(count ~ 1 + wind + rain + noise + cloud_cover + day_of_yr_c + 
                 day_sq,
                data = gcki, 
                family = "poisson")

summary(gcki.ct)
plot(gcki.ct)

vif(gcki.ct)
resids <- rstandard(gcki.ct)

p.resids <- residuals(gcki.ct, type = "deviance")
gcki$pois_devresids <- p.resids

gcki[gcki$pois_devresids > 2, ]

hist(resids, main = "Standardized Residuals for sp_det model", xlab = 
       "standardized residuals")
boxplot(resids, main = "standardized residuals for sp_det model", ylab = 
          "standardize residuals")


gcount <-  table(gcki$count)
barplot(gcount, main = "distribution of # of gcki per count", xlab = "count", 
        ylab = "frequency")

# check unconditional mean and variance for sp_detected (response variable)
# (we ultimately care only about CONDITIONAL mean and variance being equal after
# model is fit but this is a good indicator of whether it might be a problem)
mean(gcki$count)
var(gcki$count)
# doesn't look great! overdispersion may be a problem here.

#look at deviance statistic of fit model divided by its d.f. to see if ratio
# is over 1
gcki.ct$deviance
gcki.ct$df.residual
#(this is the ratio we care about)
with(gcki.ct, deviance/df.residual)
#(this gives us a p-value for that ratio)
with(gcki.ct, pchisq(deviance, df.residual, lower.tail = FALSE))
## this shows us that we might have overdispersion; ratio is 1.3 instead of 1.

## test of quasi poisson glm

gcki.ct.qp <- glm(count ~ 1 + wind + rain + noise + cloud_cover + day_of_yr_c + 
                   day_sq,
               data = gcki, 
               family = "quasipoisson")
summary(gcki.ct.qp)
plot(gcki.ct.qp)

## test of negative binomial glm 
gcki.ct.nb <- glm.nb(count ~ 1 + wind + rain + noise + cloud_cover + day_of_yr_c + 
                    day_sq,
                  data = gcki)

summary(gcki.ct.nb)
plot(gcki.ct.nb)
## end GLM for GCKI counts------------------------------------------------------



## Boosted Regression Tree for GCKI counts--------------------------------------

gcki.brt <- gbm(count ~ 1 + wind + rain + noise + day_of_yr_c + cloud_cover +
                        day_sq + min_past_sun, 
                distribution = "poisson", 
                data = gcki, 
                interaction.depth = 3, 
                n.trees = 1000, 
                n.minobsinnode = 5, 
                shrinkage = 0.01, 
                bag.fraction = 0.8)

## end BRT for GCKI counts-------------------------------------------------------

## predictions with BRT---------------------------------------------------------
# create new data to predict with
pred_gcki <- data.frame(day_of_yr = seq(min(gcki$day_of_yr), 
                                        max(gcki$day_of_yr), by = 1))
pred_gcki$day_of_yr_c <- pred_gcki$day_of_yr-mean(pred_gcki$day_of_yr)
pred_gcki$day_sq <- pred_gcki$day_of_yr_c^2
pred_gcki$min_past_sun <- median(gcki$min_past_sun)
pred_gcki$wind <- as.factor("0-1")
pred_gcki$rain <- as.factor("Dry")
pred_gcki$noise <- as.factor("0")
pred_gcki$cloud_cover <- as.factor("0-33")
#coerce factor variables to contain the same number of levels as the original 
pred_gcki$wind <- factor(pred_gcki$wind, 
                         levels = c("0-1", "2", "3+"),
                         labels = c("0-1", "2", "3+"))
pred_gcki$rain <- factor(pred_gcki$rain, 
                         levels = c("Dry", "wet"),
                         labels = c("Dry", "wet"))
pred_gcki$noise <- factor(pred_gcki$noise, 
                          levels = c("0", "1", ">2"), 
                          labels = c("0", "1", ">2"))
pred_gcki$cloud_cover <- factor(pred_gcki$cloud_cover, 
                          levels = c("0-33", "33-66", "66-100"), 
                          labels = c("0-33", "33-66", "66-100"))


p1 <- predict(gcki.brt, newdata = pred_gcki, n.trees = 1000, type = "response")
## end predictions with BRT-----------------------------------------------------

