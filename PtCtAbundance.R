################################
## Abundance estimation for Point Counts
## Point Abbaye 2019 data
## 
## author: Ellie Roark, Willson Gaul
## created: 10 Feb 2020
## last modified: 5 Mar 2020
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

setwd("/home/emer/Dropbox/Ellie Roark/R/PointAbbaye/")

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
   sunrise_times, windday)

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




plot(sum_gcki$day_of_yr, sum_gcki$count,
     main = "gcki per day over time")
ggplot(data = sum_gcki, aes(x = day_of_yr, y = count)) + 
        geom_point() + 
        geom_smooth()

plot(sum_ybsa$day_of_yr, sum_ybsa$count,
     main = "ybsa per day over time")
ggplot(data = sum_ybsa, aes(x = day_of_yr, y = count)) + 
        geom_point() + 
        geom_smooth()

plot(sum_wiwr$day_of_yr, sum_wiwr$count,
     main = "wiwr per day over time")
ggplot(data = sum_wiwr, aes(x = day_of_yr, y = count)) + 
        geom_point() + 
        geom_smooth()

plot(sum_bcch$day_of_yr, sum_bcch$count,
     main = "bcch per day over time")
ggplot(data = sum_bcch, aes(x = day_of_yr, y = count)) + 
        geom_point() + 
        geom_smooth()

plot(sum_heth$day_of_yr, sum_heth$count,
     main = "heth per day over time")
ggplot(data = sum_heth, aes(x = day_of_yr, y = count)) + 
        geom_point() + 
        geom_smooth()

## end Exploratory plots -------------------------------------------------------

## GLM for GCKI counts over time------------------------------------------------
# ## model for the number of GCKI detected per point count, depending on weather
# gcki.ct <- glm(count ~ 1 + wind + rain + noise + cloud_cover + day_of_yr_c + 
#                  day_sq + min_past_sun,
#                 data = gcki, 
#                 family = "poisson")
# 
# summary(gcki.ct)
# #plot(gcki.ct)
# 
# vif(gcki.ct)
# #resids <- rstandard(gcki.ct)
# 
# p.resids <- residuals(gcki.ct, type = "deviance")
# gcki$pois_devresids <- p.resids
# 
# gcki[gcki$pois_devresids > 2, ]
# 
# hist(p.resids, main = "Deviance Residuals for gcki model", xlab = 
#        "residuals")
# boxplot(p.resids, main = "Deviance residuals for gcki model", ylab = 
#           "residuals")
# 
# 
# gcount <-  table(gcki$count)
# barplot(gcount, main = "distribution of # of gcki per count", xlab = "count", 
#         ylab = "frequency")
# 
# # check unconditional mean and variance for sp_detected (response variable)
# # (we ultimately care only about CONDITIONAL mean and variance being equal after
# # model is fit but this is a good indicator of whether it might be a problem)
# mean(gcki$count)
# var(gcki$count)
# # doesn't look great! overdispersion may be a problem here.
# 
# #look at deviance statistic of fit model divided by its d.f. to see if ratio
# # is over 1
# gcki.ct$deviance
# gcki.ct$df.residual
# #(this is the ratio we care about)
# with(gcki.ct, deviance/df.residual)
# #(this gives us a p-value for that ratio)
# with(gcki.ct, pchisq(deviance, df.residual, lower.tail = FALSE))
# ## this shows us that we might have overdispersion; ratio is 1.4 instead of 1.
# 
# 
# ## test of quasi poisson glm for gcki per point count model
# gcki.ct.qp <- glm(count ~ 1 + wind + rain + noise + cloud_cover + day_of_yr_c + 
#                    day_sq,
#                data = gcki, 
#                family = "quasipoisson")
# summary(gcki.ct.qp)
# #plot(gcki.ct.qp)
# 
# ## test of negative binomial glm 
# gcki.ct.nb <- glm.nb(count ~ 1 + wind + rain + noise + cloud_cover + 
#                              day_of_yr_c + day_sq,
#                   data = gcki)
# 
# summary(gcki.ct.nb)
# 
# # get fitted values for nb model
# gcki$fv <- predict(gcki.ct.nb, type="response")
# 
# #make data long format so that I can plot fit val AND original counts over time
# mgcki <- data.frame(day_of_yr = gcki$day_of_yr, 
#                     fv = gcki$fv, 
#                     count = gcki$count)
# mgcki <- pivot_longer(mgcki, cols = fv:count, names_to = 'data_type')
# 
# 
# #plot fitted values for nb model over time, alongside actual values
# nbgcki_time <- ggplot(mgcki, aes(x=day_of_yr, y=value, colour=data_type)) + 
#         geom_point() + 
#         theme_bw() +
#         scale_colour_viridis_d() + 
#         scale_y_continuous() + 
#         ylab("Number of GCKI") +
#         xlab("Day of Year")
# nbgcki_time
# 
# plot(gcki$count ~ gcki$fv)
# abline(0, 1)
# 
# td <- data.frame(simulate(gcki.ct.nb, nsim = 10))
# td$obs <- gcki$count
# td$doy <- gcki$day_of_yr
# td <- pivot_longer(td, 1:(ncol(td)-2), names_to = "case", values_to = "n_sp")
# 
# ggplot(data = td, aes(x = n_sp, y = obs)) + 
#         geom_point() + 
#         geom_jitter() + 
#         geom_smooth()
# 
# ggplot() + 
#         geom_point(data = td, aes(x = doy, y = n_sp), alpha = 0.1) + 
#         geom_point(data = td[td$case == "obs", ], aes(x = doy, y = n_sp), 
#                    col = "red")
# 

## end GLM for GCKI counts------------------------------------------------------

## GLM for GCKI **per day** counts----------------------------------------------
## model of GCKI ** per day ** over time
# gcki.day <- glm(count ~ 1 + wind + day_of_yr_c + day_sq,
#                 data = sum_gcki, 
#                 family = "poisson")
# 
# summary(gcki.day)
# #plot(gcki.day)
# 
# vif(gcki.day)
# resids <- rstandard(gcki.day)
# 
# p.day.resids <- residuals(gcki.day, type = "deviance")
# sum_gcki$pois_devresids <- p.day.resids
# 
# sum_gcki[sum_gcki$pois_devresids > 2, ]
# 
# hist(p.day.resids, main = "Deviance Residuals for gcki (day) model", xlab = 
#              "residuals")
# boxplot(p.day.resids, main = "Deviance residuals for gcki (day) model", ylab = 
#                 "residuals")
# 
# 
# gdaycount <-  table(sum_gcki$count)
# barplot(gdaycount, main = "distribution of # of gcki per day", xlab = "count", 
#         ylab = "frequency")
# 
# # check unconditional mean and variance for sp_detected (response variable)
# # (we ultimately care only about CONDITIONAL mean and variance being equal after
# # model is fit but this is a good indicator of whether it might be a problem)
# mean(sum_gcki$count)
# var(sum_gcki$count)
# # doesn't look great! overdispersion may be a problem here.
# 
# #look at deviance statistic of fit model divided by its d.f. to see if ratio
# # is over 1
# gcki.day$deviance
# gcki.day$df.residual
# #(this is the ratio we care about)
# with(gcki.day, deviance/df.residual)
# #(this gives us a p-value for that ratio)
# with(gcki.day, pchisq(deviance, df.residual, lower.tail = FALSE))
# ## this is TERRIBLE-- ratio is 2.4, definitely different from 1!!! 
# ## Poisson assumptions pretty definitively not met. 
# 

# test neg binom model for GCKI per day to see if it helps with overdispersion
gcki.day.nb <- glm.nb(count ~ 1 + wind + day_of_yr_c + day_sq,
                data = sum_gcki)

#get fitted values
sum_gcki$nbfv <- predict(gcki.day.nb, type="response")

plot(sum_gcki$count ~ sum_gcki$nbfv)
abline(0, 1)

td <- data.frame(simulate(gcki.day.nb, nsim = 10))
td$obs <- sum_gcki$count
td$doy <- sum_gcki$day_of_yr
td <- pivot_longer(td, 1:(ncol(td)-2), names_to = "case", values_to = "n_sp")

ggplot(data = td, aes(x = n_sp, y = obs)) +
        geom_point() +
        geom_jitter() +
        geom_smooth()

ggplot() +
        geom_point(data = td, aes(x = doy, y = n_sp), alpha = 0.1) +
        geom_point(data = td[td$case == "obs", ], aes(x = doy, y = n_sp),
                   col = "red")

#look at deviance statistic of fit model divided by its d.f. to see if ratio
# is over 1
gcki.day.nb$deviance
gcki.day.nb$df.residual
#(this is the ratio we care about)
with(gcki.day.nb, deviance/df.residual)
#(this gives us a p-value for that ratio)
with(gcki.day.nb, pchisq(deviance, df.residual, lower.tail = FALSE))
## this is way better than poisson-- ratio is 1.09, pvalue is .3-- not sig.
## different from 1.

## end GLM for GCKI per day-----------------------------------------------------

## Boosted Regression Tree for GCKI per COUNT-----------------------------------

gcki.brt <- gbm(count ~ 1 + wind + rain + noise + day_of_yr_c + cloud_cover +
                        day_sq + min_past_sun, 
                distribution = "poisson", 
                data = gcki, 
                interaction.depth = 3, 
                n.trees = 1000, 
                n.minobsinnode = 5, 
                shrinkage = 0.01, 
                bag.fraction = 0.8)

## end BRT for GCKI per COUNT model---------------------------------------------

## predictions with BRT **per count model***-----------------------------------
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

# get predictions with new data
pred_gcki$p1 <- predict(gcki.brt, newdata = pred_gcki, n.trees = 1000, 
                        type = "response")

#plot predictions over time 
predgcki_time <- ggplot(pred_gcki, aes(x=day_of_yr, y=p1)) + 
        geom_line() + 
        geom_point(data = gcki,
                   aes(x = day_of_yr, y = count)) +
        theme_bw() +
        scale_colour_viridis_d() + 
        scale_y_continuous() + 
        ylab("Number of GCKI") +
        xlab("Day of Year")
predgcki_time
## end predictions with BRT **GCKI per count model**----------------------------

## Boosted Regression Tree for GCKI per DAY-----------------------------------

# assign days to 3-day blocks
days <- data.frame(day = min(sum_gcki$day_of_yr):max(sum_gcki$day_of_yr), 
                   block = NA)
n_blocks <- nrow(days)/3
blocks <- rep(1:n_blocks, 3)
blocks <- blocks[order(blocks)]
start_row <- sample(1:nrow(days), size = 1)
days$block[start_row:nrow(days)] <- blocks[1:length(start_row:nrow(days))]
days$block[1:(start_row - 1)] <- blocks[(length(start_row:nrow(days)) + 1):
                                                nrow(days)]

# assign blocks to CV folds
fold_assignments <- data.frame(
        block = unique(days$block), 
        fold = sample(rep_len(1:5, length.out = length(unique(days$block)))))
days <- left_join(days, fold_assignments, by = "block")
rm(blocks, n_blocks, start_row, fold_assignments)

# join CV fold info onto bird data
sum_gcki <- left_join(sum_gcki, days, by = c("day_of_yr" = "day"))

# fit brt to data in CV folds
brt_test_folds <- unique(sum_gcki$fold)
names(brt_test_folds) <- as.character(brt_test_folds)

fit_brt <- function(test_fold, sp_data) {
        train_dat <- sp_data[sp_data$fold != test_fold, ]
        f_m <- gbm(count ~ 1 + wind + day_of_yr_c, 
                                   distribution = "poisson", 
                                   data = train_dat, 
                                   interaction.depth = 1, 
                                   n.trees = 2000, 
                                   n.minobsinnode = 1, 
                                   shrinkage = 0.001, 
                                   bag.fraction = 0.8)
        test_pred <- sp_data[sp_data$fold == test_fold, ]
        test_pred$OOB_preds <- predict(f_m, newdata = test_pred, 
                                       n.trees = 2000, type = "response")
        list(mod = f_m, test_predictions = test_pred)
}

brt_test_folds <- lapply(brt_test_folds, fit_brt, sp_data = sum_gcki)
## end BRT for GCKI per DAY model---------------------------------------------


## evaluate BRT ----------------------------------------------------------------
# get predictions to test data
gcki_brt_predictions <- bind_rows(lapply(brt_test_folds, 
                                     FUN = function(x) x$test_predictions))
gcki_brt_predictions$error = gcki_brt_predictions$OOB_preds - gcki_brt_predictions$count

# calculate r^2 (square of Pearson correlation coefficient, see Bahn & 
# McGill 2013)
gcki_brt_r2 <- cor(gcki_brt_predictions$day_of_yr, gcki_brt_predictions$OOB_preds, 
                   method = "pearson")^2
# calculate R^2 (coefficient of determination, see Bahn & McGill 2013)
gcki_brt_R2 <- 1 - (sum(gcki_brt_predictions$error^2) / 
                            (sum((gcki_brt_predictions$count - 
                                         mean(gcki_brt_predictions$count))^2)))
## end evaluate BRT ------------------------------------------------------------

## predictions with BRT **per DAY model***-----------------------------------
# create new data to predict with
pgcki_day <- data.frame(day_of_yr = seq(min(sum_gcki$day_of_yr), 
                                        max(sum_gcki$day_of_yr), by = 1))
pgcki_day$day_of_yr_c <- pgcki_day$day_of_yr-mean(pgcki_day$day_of_yr)
pgcki_day$wind <- mean(sum_gcki$wind)
pgcki_day <- left_join(pgcki_day, days, by = c("day_of_yr" = "day"))

# get predictions with new data.  Predict to days using the model for which 
# those days were not in the training set
gcki_preds_newdata <- bind_rows(
        lapply(1:length(brt_test_folds), 
               FUN = function(x, newdata, brts) {
                       test_data <- newdata[newdata$fold == x, ]
                       test_data$predictions <- predict(brts[[x]]$mod, 
                                                        newdata = test_data, 
                                                        n.trees = 2000, 
                                                        type = "response")
                       test_data}, 
               newdata = pgcki_day, brts = brt_test_folds))

#plot predictions over time 
pgcki_day_time <- ggplot(gcki_preds_newdata, aes(x=day_of_yr, y=predictions)) + 
        geom_line() + 
        geom_point(data = sum_gcki,
                   aes(x = day_of_yr, y = count)) +
        theme_bw() +
        scale_colour_viridis_d() + 
        scale_y_continuous() + 
        ylab("Number of GCKI per day") +
        xlab("Day of Year")
pgcki_day_time

# plot predicted vs. observed values
ggplot(data = gcki_brt_predictions, aes(x = count, y = OOB_preds)) + 
        geom_point() + 
        geom_smooth() + 
        geom_abline(intercept = 0, slope = 1) + 
        ggtitle("BRT predicted vs. observed") + 
        ylim(c(0, 9))
## end predictions with BRT **GCKI per DAY model**----------------------------

## GAM with GCKI per day--------------------------------------------------------
k <- 6 # k should be large enough that EDF is a good bit less than k-1.  

## test GAM fitting with all data
# for interaction discussion, see Section 5.6.3 and p 344 of Wood
#  + ti(wind, day_of_yr_c, k = k)
gcki.day.gam <- gam(count ~ 1 + s(wind, k = k) + s(day_of_yr_c, k = k), 
                    data = sum_gcki, 
                    family = "nb", select = TRUE)


# fit brt to data in CV folds
gam_test_folds <- unique(sum_gcki$fold)
names(gam_test_folds) <- as.character(gam_test_folds)

fit_gam <- function(test_fold, sp_data) {
        train_dat <- sp_data[sp_data$fold != test_fold, ]
        f_m <- gam(count ~ 1 + s(wind, k = k) + s(day_of_yr_c, k = k), 
                   data = sp_data, 
                   family = "nb", select = TRUE)
        test_pred <- sp_data[sp_data$fold == test_fold, ]
        test_pred$OOB_preds <- predict(f_m, newdata = test_pred, 
                                           type = "response")
        list(mod = f_m, test_predictions = test_pred)
}

gam_test_folds <- lapply(gam_test_folds, fit_gam, sp_data = sum_gcki)
## end GAM with GCKI per day----------------------------------------------------

## evaluate GAM (GCKI per day model)--------------------------------------------
gcki.day.gam
gam.check(gcki.day.gam)
plot(gcki.day.gam, pages = 1, all.terms = T)

# get predictions to test data
gcki_gam_predictions <- bind_rows(lapply(gam_test_folds, 
                                         FUN = function(x) x$test_predictions))
gcki_gam_predictions$error = gcki_gam_predictions$OOB_preds - gcki_gam_predictions$count

# calculate r^2 (square of Pearson correlation coefficient, see Bahn & 
# McGill 2013)
gcki_gam_r2 <- cor(gcki_gam_predictions$day_of_yr, gcki_gam_predictions$OOB_preds, 
                   method = "pearson")^2
# calculate R^2 (coefficient of determination, see Bahn & McGill 2013)
gcki_gam_R2 <- 1 - (sum(gcki_gam_predictions$error^2) / 
                            (sum((gcki_gam_predictions$count - 
                                          mean(gcki_gam_predictions$count))^2)))
## end evaluate GAM (GCKI per day model)----------------------------------------

##predictions with GAM (GCKI per day model)-------------------------------------

## end predictions with GAM (GCKI per day model)--------------------------------


## join brt and gam predictions and errors -------------------------------------
gcki_gam_predictions$method = "gam"
gcki_brt_predictions$method = "brt"
gcki_predictions <- bind_rows(gcki_gam_predictions, gcki_brt_predictions)
gcki_predictions <- pivot_longer(gcki_predictions, 
                                 cols = c(error, OOB_preds), 
                                 names_to = "metric")

ggplot(data = gcki_predictions[gcki_predictions$metric == "error", ], 
       aes(x = day_of_yr, y = value, color = method)) + 
        geom_point() + 
        ggtitle("Prediction error")

ggplot(data = gcki_predictions[gcki_predictions$metric == "OOB_preds", ], 
       aes(x = day_of_yr, y = value, color = method)) + 
        geom_point()  + 
        geom_smooth() + 
        geom_point(aes(x = day_of_yr, y = count), color = "black") + 
        ggtitle("Predictions")

plot(gcki_gam_predictions$OOB_preds ~ gcki_brt_predictions$OOB_preds)
plot(gcki_gam_predictions$error ~ gcki_brt_predictions$error)
## end brt and gam predictions plot --------------------------------------------

## GLM for WIWR counts over time------------------------------------------------
## model for the number of GCKI detected per point count, depending on weather
# wiwr.ct <- glm(count ~ 1 + wind + rain + noise + cloud_cover + day_of_yr_c + 
#                        day_sq,
#                data = wiwr, 
#                family = "poisson")
# 
# summary(wiwr.ct)
# #plot(gcki.ct)
# 
# vif(wiwr.ct)
# wiwr.s.resids <- rstandard(wiwr.ct)
# 
# wiwr.p.resids <- residuals(wiwr.ct, type = "deviance")
# wiwr$pois_devresids <- wiwr.p.resids
# 
# wiwr[wiwr$pois_devresids > 2, ]
# 
# hist(wiwr.p.resids, main = "Deviance Residuals for wiwr model", xlab = 
#              "residuals")
# boxplot(wiwr.p.resids, main = "Deviance residuals for wiwr model", ylab = 
#                 "residuals")
# 
# 
# wcount <-  table(wiwr$count)
# barplot(gcount, main = "distribution of # of wiwr per count", xlab = "count", 
#         ylab = "frequency")
# 
# # check unconditional mean and variance for sp_detected (response variable)
# # (we ultimately care only about CONDITIONAL mean and variance being equal after
# # model is fit but this is a good indicator of whether it might be a problem)
# mean(wiwr$count)
# var(wiwr$count)
# # doesn't look great! overdispersion may be a problem here.
# 
# #look at deviance statistic of fit model divided by its d.f. to see if ratio
# # is over 1
# wiwr.ct$deviance
# wiwr.ct$df.residual
# #(this is the ratio we care about)
# with(wiwr.ct, deviance/df.residual)
# #(this gives us a p-value for that ratio)
# with(wiwr.ct, pchisq(deviance, df.residual, lower.tail = FALSE))
# ## ??? ratio is .8, p-value is .9....
# 
# ## test of negative binomial glm 
# wiwr.ct.nb <- glm.nb(count ~ 1 + wind + rain + noise + cloud_cover + 
#                              day_of_yr_c + day_sq + min_past_sun,
#                      data = wiwr)
# ## warning about theta iteration reached...
# summary(wiwr.ct.nb)
# 
# # get fitted values for nb model
# wiwr$fv <- predict(wiwr.ct.nb, type="response")
# 
# #make data long format so that I can plot fit val AND original counts over time
# mwiwr <- data.frame(day_of_yr = wiwr$day_of_yr, 
#                     fv = wiwr$fv, 
#                     count = wiwr$count)
# mwiwr <- pivot_longer(mwiwr, cols = fv:count, names_to = 'data_type')
# 
# 
# #plot fitted values for nb model over time, alongside actual values
# nbwiwr_time <- ggplot(mwiwr, aes(x=day_of_yr, y=value, colour=data_type)) + 
#         geom_point() + 
#         theme_bw() +
#         scale_colour_viridis_d() + 
#         scale_y_continuous() + 
#         ylab("Number of WIWR") +
#         xlab("Day of Year")
# nbwiwr_time
# 
# plot(wiwr$count ~ wiwr$fv)
# abline(0, 1)
# 
# td2 <- data.frame(simulate(wiwr.ct.nb, nsim = 10))
# td2$obs <- wiwr$count
# td2$doy <- wiwr$day_of_yr
# td2 <- pivot_longer(td2, 1:(ncol(td2)-2), names_to = "case", values_to = "n_sp")
# 
# ggplot(data = td2, aes(x = n_sp, y = obs)) + 
#         geom_point() + 
#         geom_jitter() + 
#         geom_smooth()
# 
# ggplot() + 
#         geom_point(data = td, aes(x = doy, y = n_sp), alpha = 0.1) + 
#         geom_point(data = td[td$case == "obs", ], aes(x = doy, y = n_sp), 
#                    col = "red")


## end GLM for WIWR counts------------------------------------------------------
