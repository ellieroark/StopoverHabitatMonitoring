################################
## PLOTS: ARUs vs pt cts
## Point Abbaye 2019
## 
## author: Ellie Roark
## created: 27 Nov 2019
## last modified: 27 Nov 2019
## 
## inputs: *ARUDuplicateReview2019- script that randomly selects which of the 
##            duplicate ARU recordings to use, and sources:
##            DataCleaningPtAbbaye2019.R- which loads the aru and point ct data 
##            from 2019 and returns the following:
##            - allaru: df of every 10 min paired ct aru observation
##            - ptct: df of every point count observation
##            - spdet_paired: df with # of species detected per count by count
##              type
##
##         
## outputs: *a number of plots for use in publications from the Pt Abbaye 2019
##            ARU vs Pt ct project.
##            
## TODO: * 
################################

library(tidyverse)
library(Hmisc)
library(lme4)


#setwd("/home/emer/Dropbox/Ellie Roark/R/PointAbbaye/")

### clean up environment
rm(allaru_s, spdet_arudup, arudiag, drop_counts, i, lm_on, mmdiag, plotson,
   poisdiag, regdiag)

#### SUMMARY PLOTS FOR SPECIES RICHNESS GLMM------------------------------------
##boxplot for sp detected by each count type
count_type_box <- ggplot(spdet_paired, aes(x=count_type, y=sp_detected)) + 
  geom_boxplot() + scale_y_continuous(breaks = c(1, 5, 10)) +
  labs(x = "Count type", y = "Number of species detected") +
  theme_bw()
count_type_box

##species detected over time, stratified by count type, with plotted GLMM
## prediction line, from maxt2.spdetmm model! formula: 
## sp_detected ~ count_type + wind + rain + noise + day_of_yr_s +  
## rain:count_type + day_of_yr_s:count_type + min_past_sun_s +  
##  (1 | day_of_yr_s/point_id)
#first, create new df with response and predictor variable columns
mean_doy <- mean(spdet_all$day_of_yr)
sd_doy_c <- sd(spdet_all$day_of_yr_c)
aru.pred <- data.frame(day_of_yr = seq(min(spdet_all$day_of_yr), 
                                        max(spdet_all$day_of_yr), by = 1))
aru.pred$day_of_yr_c <- aru.pred$day_of_yr-mean_doy
aru.pred$day_of_yr_s <- aru.pred$day_of_yr_c/sd_doy_c
#ptct.pred <- data.frame(day_of_yr_s = spdet_all$day_of_yr_s)
aru.pred$day_sq_s <- aru.pred$day_of_yr_s^2
aru.pred$min_past_sun_s <- median(spdet_all$min_past_sun_s)
aru.pred$count_type <- as.factor("aru")
aru.pred$wind <- as.factor("0-1")
aru.pred$rain <- as.factor("Dry")
aru.pred$noise <- as.factor("0")
aru.pred$point_id <- as.factor("FB1")
#coerce factor variables to contain the same number of levels as the original 
aru.pred$count_type <- factor(aru.pred$count_type, 
                               levels = c("point", "aru"), 
                               labels = c("point", "aru"))
aru.pred$wind <- factor(aru.pred$wind, 
                         levels = c("0-1", "2", "3+"),
                         labels = c("0-1", "2", "3+"))
aru.pred$rain <- factor(aru.pred$rain, 
                         levels = c("Dry", "wet"),
                         labels = c("Dry", "wet"))
aru.pred$noise <- factor(aru.pred$noise, 
                          levels = c("0", "1", ">2"), 
                          labels = c("0", "1", ">2"))
aru.pred$point_id <- factor(aru.pred$point_id, 
                             levels = c("p1", "p2", "p3", "p4", "p5", "p6",
                                        "p8", "p9", "p10", "p11", "p12", 
                                        "p13","p14", "p15", "p17", "p18", 
                                        "FB1", "FB2"),
                             labels = c("p1", "p2", "p3", "p4", "p5", "p6",
                                        "p8", "p9", "p10", "p11", "p12", 
                                        "p13","p14", "p15", "p17", "p18", 
                                        "FB1", "FB2"))



#next, feed that new data into the predict function for lmer package
#make a new col with predictions
aru.pred$preds <- predict(r1.spdetmm, newdata = aru.pred, type = "response", 
                          re.form = NA)


## now do the same thing with pt cts
# ptct.pred <- data.frame(day_of_yr_s = seq(-1.86, 1.61, by = 0.01))
ptct.pred <- data.frame(day_of_yr = seq(min(spdet_all$day_of_yr), 
                                        max(spdet_all$day_of_yr), by = 1))
ptct.pred$day_of_yr_c <- ptct.pred$day_of_yr-mean_doy
ptct.pred$day_of_yr_s <- ptct.pred$day_of_yr_c/sd_doy_c
rm(mean_doy, sd_doy_c) # clean workspace

#ptct.pred <- data.frame(day_of_yr_s = spdet_all$day_of_yr_s)
ptct.pred$day_sq_s <- ptct.pred$day_of_yr_s^2
ptct.pred$min_past_sun_s <- median(spdet_all$min_past_sun_s)
ptct.pred$count_type <- as.factor("point")
ptct.pred$wind <- as.factor("0-1")
ptct.pred$rain <- as.factor("Dry")
ptct.pred$noise <- as.factor("0")
ptct.pred$point_id <- as.factor("FB1")
#coerce factor variables to contain the same number of levels as the original 
ptct.pred$count_type <- factor(ptct.pred$count_type, 
                               levels = c("point", "aru"), 
                               labels = c("point", "aru"))
ptct.pred$wind <- factor(ptct.pred$wind, 
                         levels = c("0-1", "2", "3+"),
                         labels = c("0-1", "2", "3+"))
ptct.pred$rain <- factor(ptct.pred$rain, 
                         levels = c("Dry", "wet"),
                         labels = c("Dry", "wet"))
ptct.pred$noise <- factor(ptct.pred$noise, 
                          levels = c("0", "1", ">2"), 
                          labels = c("0", "1", ">2"))
ptct.pred$point_id <- factor(ptct.pred$point_id, 
                             levels = c("p1", "p2", "p3", "p4", "p5", "p6",
                                        "p8", "p9", "p10", "p11", "p12", 
                                        "p13","p14", "p15", "p17", "p18", 
                                        "FB1", "FB2"),
                             labels = c("p1", "p2", "p3", "p4", "p5", "p6",
                                        "p8", "p9", "p10", "p11", "p12", 
                                        "p13","p14", "p15", "p17", "p18", 
                                        "FB1", "FB2"))

#now make a new col with predictions
ptct.pred$preds <- predict(r1.spdetmm, type = "response",
                           newdata = ptct.pred, re.form = NA)

#bind ptct.pred to aru.pred
ptct.pred <- bind_rows(ptct.pred, aru.pred)

spdet_all$fitval <- predict(r1.spdetmm) # this gets fitted values

#plot of fitted values
spdet_time_fitv <- ggplot(spdet_all, aes(x=day_of_yr_s, y=fitval, 
                                        color=count_type)) + 
  geom_point(shape=1) +
  theme_bw() +
  scale_colour_hue(h = c(0, 360), l=30) # Use a slightly darker palette than normal
spdet_time_fitv


#create scatterplot of sp detected over time with count type as color
spdet_time_ptct <- ggplot(ptct.pred, aes(x=day_of_yr, y=preds, 
                                         color=count_type, 
                                         group = count_type)) + 
  # geom_point(shape=1) +
  geom_line() + 
  geom_point(data = spdet_all,
             aes(x = day_of_yr, y = sp_detected, color = count_type)) +
  theme_bw() +
  scale_colour_viridis_d() + 
  scale_y_continuous(breaks = c(0, 4, 8, 12)) + 
  ylab("Number of Species") +
  xlab("Day of Year")
spdet_time_ptct

spdet_time_aru <- ggplot(aru.pred, aes(x=day_of_yr_s, y=preds, 
                                         color=count_type)) + 
  geom_point(shape=1) +
  theme_bw() +
  scale_colour_hue(h = c(0, 360), l=30) # Use a slightly darker palette than normal
spdet_time_aru

##test adding preds to original data frame
spdet_all$predicted <- predict(r1.spdetmm, newdata = spdet_all,
                               type = "response")

## plot preds for original data
spdet_time_pred <- ggplot(spdet_all, aes(x=day_of_yr, y=predicted, 
                                         color=count_type)) + 
  geom_point(shape=1) +
  theme_bw() +
  scale_colour_hue(h = c(0, 360), l=30) + # Use a slightly darker palette than normal
  scale_y_continuous(breaks = c(0, 4, 8, 12))
spdet_time_pred

# get mean for daily predictions from each count type
arupred <- spdet_all[which(spdet_all$count_type == "aru"), ]
ptctpred <- spdet_all[which(spdet_all$count_type == "point"), ]

# aggregate(ptctpred$predicted, by = list(ptctpred$date), FUN = mean)
# #OR
# arupred %>%
#   group_by(day_of_yr) %>%
#   summarise(mean(predicted))


## plot of coefficients for GLMM (10 min vs 10 min)
## example from wg:
## # make a data frame to use in ggplot
r1_df <- data.frame(matrix(nrow = 12, ncol = 4))
colnames(r1_df) <- c("variable", "coef_estimate", "l_bound", "h_bound")
r1_df$variable <- c("Count type (ARU)", "Wind (2)", "Wind (3+)",
                      "Rain (Wet)", "Noise (1)", "Noise (>2)", "Day of year", 
                      "Day of year squared", "Minutes past sunrise", 
                      "Count type (ARU)*Rain (Wet)", 
                      "Count type (ARU)*Day of Year",
                      "Count type (ARU)*Day of year squared")

# get coefficient point estimate and lower and upper CI bounds for each
# predictor variable
for(i in 1:nrow(r1_df)) {
  # get coefficient point estimate
  r1_df$coef_estimate[i] <- summary(r1.spdetmm)$coefficients[1 + i]
  r1_df$l_bound[i] <- CI.r1[2 + i, "2.5 %"]
  r1_df$h_bound[i] <- CI.r1[2 + i, "97.5 %"]
}

# make graph
print(ggplot(data = r1_df,
             aes(x = factor(variable,
                            levels = c("Count type (ARU)", "Wind (2)", "Wind (3+)",
                                       "Rain (Wet)", "Noise (1)", "Noise (>2)", 
                                       "Day of year", "Day of year squared", 
                                       "Minutes past sunrise", 
                                       "Count type (ARU)*Rain (Wet)", 
                                       "Count type (ARU)*Day of Year",
                                       "Count type (ARU)*Day of year squared")),
                 y = coef_estimate)) +
        geom_point() +
        geom_linerange(aes(x = factor(variable,
                                      levels = c("Count type (ARU)", "Wind (2)",
                                                 "Wind (3+)", "Rain (Wet)", 
                                                 "Noise (1)", "Noise (>2)", 
                                                 "Day of year", 
                                                 "Day of year squared", 
                                                 "Minutes past sunrise", 
                                                 "Count type (ARU)*Rain (Wet)", 
                                                 "Count type (ARU)*Day of Year",
                                                 "Count type (ARU)*Day of year squared")),
                           ymin = l_bound,
                           ymax = h_bound)) +
        xlab("Predictor Variable") +
        ylab("Coefficient estimate") +
        theme_bw() +
        ylim(-1, 1) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)))

#### END summary plots for species richness GLMM-------------------------------

