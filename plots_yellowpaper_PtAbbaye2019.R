################################
## PLOTS: ARUs vs pt cts
## Point Abbaye 2019
## 
## author: Ellie Roark
## created: 27 Nov 2019
## last modified: 16 Apr 2020
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


### SUMMARY PLOTS FOR ABUNDANCE MODELS------------------------------------------
## scatterplot of number of GCKI per day (point counts) over time, with fitted 
## average model (BRT) as a line
#fits_gcki_brt <- readRDS("fits_gcki_brt.rds")

# get standardized predictions for predictions to test data from all 1000 models
gcki_ptct_preds_brt <- bind_rows(lapply(fits_gcki_brt, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$standardized_preds}))
}))

# get average prediction for each day from the 200 iterations of the 5-fold CV
warning("Add standard error to this at some point.  14 April WG.")
gcki_ptct_preds_brt <- group_by(gcki_ptct_preds_brt, day_of_yr) %>%
  summarise(mean_pred = mean(predictions))

ggplot(data = gcki_ptct_preds_brt, aes(x = day_of_yr, y = mean_pred)) + 
  geom_line() + 
  geom_point(data = sum_gcki, aes(x = day_of_yr, y = meandet)) + 
  ggtitle("Golden-crowned Kinglet\nPoint counts (10 min); BRT") + 
  xlab("Julian day of year") + 
  ylab("Mean number of individuals per point count") + 
  theme_bw()

b200_df <- bind_rows(lapply(fits_gcki_brt$`200`, 
                            FUN = function(x) {x$standardized_preds}))
ggplot(data = b200_df, aes(x = day_of_yr, y = predictions)) + 
  geom_line() + 
  geom_point(data = sum_gcki, aes(x = day_of_yr, y = meandet)) + 
  ggtitle("Golden-crowned Kinglet\nPoint counts (10 min)\nBRT ex. model 200") + 
  xlab("Julian day of year") + 
  ylab("Mean number of individuals per point count") + 
  theme_bw()

## GCKIPointCounts over time- observed vs predicted values- one set of five-
## fold CV models
# put test predictions from all five folds into one df
p200_df <- bind_rows(lapply(fits_gcki_brt$`200`, 
                          FUN = function(x) {x$test_predictions}))

ggplot(data = p200_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count") +
  ggtitle("model group 200")

p19_df <- bind_rows(lapply(fits_gcki_brt$`19`, 
                          FUN = function(x) {x$test_predictions}))

ggplot(data = p19_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count")+
  ggtitle("model group 19")

p112_df <- bind_rows(lapply(fits_gcki_brt$`112`, 
                          FUN = function(x) {x$test_predictions}))

ggplot(data = p112_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count")+
  ggtitle("model group 112")

p94_df <- bind_rows(lapply(fits_gcki_brt$`94`, 
                          FUN = function(x) {x$test_predictions}))

ggplot(data = p94_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count") +
  ggtitle("model group 94")

# get predictions to independent test data from all 1000 models
gcki_ptct_testpreds_brt <- bind_rows(lapply(fits_gcki_brt, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$test_predictions}))
}))

ggplot(data = gcki_ptct_testpreds_brt, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count") + 
  ggtitle("Golden-crowned Kinglets\nPoint counts (10 min); 1000 BRTs")


##### THIS is a plot of observed values vs STANDARDIZED predictions
# ## GCKIPointCounts over time- observed vs predicted values- mean of all models
# gcki_ptct_preds_brt <- left_join(gcki_ptct_preds_brt, sum_gcki, by = "day_of_yr")
# 
# ggplot(data = gcki_ptct_preds_brt, aes(x = meandet, y = mean_pred)) + 
#   geom_point() + 
#   geom_smooth() + 
#   geom_abline(intercept = 0, slope = 1) + 
#   theme_bw()+
#   xlab("mean observed individuals per point count") +
#   ylab("mean predicted individuals per point count") +
#   ggtitle("GCKI point counts (10 min)\nBRT predicted vs. observed") +
#   ylim(c(0, 2))
rm(fits_gcki_brt)


## scatterplot of number of GCKI per day (ARU consecutive 10 min) over time, 
## with fitted average model (BRT) as a line
# get standardized predictions for predictions to test data from all 1000 models
#fits_arugcki_brt <- readRDS("fits_arugcki_brt.rds")

gcki_aru10c_preds_brt <- bind_rows(lapply(fits_arugcki_brt, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$standardized_preds}))
}))

# get average prediction for each day from the 200 iterations of the 5-fold CV
warning("Add standard error to this at some point.  14 April WG.")
gcki_aru10c_preds_brt <- group_by(gcki_aru10c_preds_brt, day_of_yr) %>%
  summarise(mean_pred = mean(predictions))

ggplot(data = gcki_aru10c_preds_brt, aes(x = day_of_yr, y = mean_pred)) + 
  geom_line() + 
  geom_point(data = sum_arugcki, aes(x = day_of_yr, y = meandet)) + 
  ggtitle("Golden-crowned Kinglet\nARU - consecutive 10 min BRT") + 
  xlab("Julian day of year") + 
  ylab("Mean number of 30-second intervals\nwith a vocalization") + 
  theme_bw()

## GCKIARU10c over time- observed vs predicted values- one set of five-
## fold CV models
# put test predictions from all five folds into one df
p140_df <- bind_rows(lapply(fits_arugcki_brt$`140`, 
                            FUN = function(x) {x$test_predictions}))

ggplot(data = p140_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals\nwith a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization") +
  ggtitle("model group 140")

p71_df <- bind_rows(lapply(fits_arugcki_brt$`71`, 
                           FUN = function(x) {x$test_predictions}))

ggplot(data = p71_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals\nwith a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization")+
  ggtitle("model group 71")

p6_df <- bind_rows(lapply(fits_arugcki_brt$`6`, 
                            FUN = function(x) {x$test_predictions}))

ggplot(data = p6_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals\nwith a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization")+
  ggtitle("model group 6")

p199_df <- bind_rows(lapply(fits_arugcki_brt$`199`, 
                           FUN = function(x) {x$test_predictions}))

ggplot(data = p199_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals\nwith a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization") +
  ggtitle("model group 199")

# get predictions to independent test data from all 1000 models
gcki_aru10c_testpreds_brt <- bind_rows(lapply(fits_arugcki_brt, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$test_predictions}))
}))

ggplot(data = gcki_aru10c_testpreds_brt, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlab("mean predicted 30-sec intervals\nwith a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization") + 
  ggtitle("Golden-crowned Kinglets\nARU- 10 consecutive min; 1000 BRTs")


# ## GCKI ARU 10c BRT- observed vs predicted values
# gcki_aru10c_preds_brt <- left_join(gcki_aru10c_preds_brt, sum_gcki, 
#                                    by = "day_of_yr")
# 
# gcki_aru10c_preds_brt <- gcki_aru10c_preds_brt[complete.cases(gcki_aru10c_preds_brt), ]
# 
# ggplot(data = gcki_aru10c_preds_brt, aes(x = meandet, y = mean_pred)) + 
#   geom_point() + 
#   geom_smooth() + 
#   geom_abline(intercept = 0, slope = 1) + 
#   theme_bw()+
#   xlab("mean observed vocalizations\nper 30 second interval") +
#   ylab("mean predicted vocalizations\nper 30 second interval") +
#   ggtitle("GCKI ARU- 10 consecutive min\nBRT predicted vs. observed") +
#   ylim(c(0, 1))
 



## scatterplot of number of WIWR per day (Point Counts 10 min) over time, 
## with fitted average model (BRT) as a line
# get standardized predictions for predictions to test data from all 1000 models
wiwr_ptct_preds_brt <- bind_rows(lapply(fits_wiwr_brt, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$standardized_preds}))
}))

# get average prediction for each day from the 200 iterations of the 5-fold CV
warning("Add standard error to this at some point.  14 April WG.")
wiwr_ptct_preds_brt <- group_by(wiwr_ptct_preds_brt, day_of_yr) %>%
  summarise(mean_pred = mean(predictions))

ggplot(data = wiwr_ptct_preds_brt, aes(x = day_of_yr, y = mean_pred)) + 
  geom_line() + 
  geom_point(data = sum_wiwr, aes(x = day_of_yr, y = meandet)) + 
  ggtitle("Winter Wren\nPoint counts (10 min); BRT") + 
  xlab("Julian day of year") + 
  ylab("Mean number of individuals per point count") + 
  theme_bw()


## WIWRPointCounts over time- observed vs predicted values- one set of five-
## fold CV models
# put test predictions from all five folds into one df
p4_df <- bind_rows(lapply(fits_wiwr_brt$`4`, 
                            FUN = function(x) {x$test_predictions}))

ggplot(data = p4_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count") +
  ggtitle("model group 4")

p18_df <- bind_rows(lapply(fits_wiwr_brt$`18`, 
                           FUN = function(x) {x$test_predictions}))

ggplot(data = p18_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count")+
  ggtitle("model group 18")

p112_df <- bind_rows(lapply(fits_wiwr_brt$`112`, 
                            FUN = function(x) {x$test_predictions}))

ggplot(data = p112_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count")+
  ggtitle("model group 112")

p94_df <- bind_rows(lapply(fits_wiwr_brt$`94`, 
                           FUN = function(x) {x$test_predictions}))

ggplot(data = p94_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count") +
  ggtitle("model group 94")

# get predictions to independent test data from all 1000 models
wiwr_ptct_testpreds_brt <- bind_rows(lapply(fits_wiwr_brt, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$test_predictions}))
}))

ggplot(data = wiwr_ptct_testpreds_brt, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count") + 
  ggtitle("Winter Wren\nPoint counts (10 min); 1000 BRTs")

# ## WIWRPointCounts over time- observed vs predicted values
# wiwr_ptct_preds_brt <- left_join(wiwr_ptct_preds_brt, sum_wiwr, by = "day_of_yr")
# 
# ggplot(data = wiwr_ptct_preds_brt, aes(x = meandet, y = mean_pred)) + 
#   geom_point() + 
#   geom_smooth() + 
#   geom_abline(intercept = 0, slope = 1) + 
#   theme_bw()+
#   xlab("mean observed individuals per point count") +
#   ylab("mean predicted individuals per point count") +
#   ggtitle("WIWR point counts (10 min)\nBRT predicted vs. observed") +
#   ylim(c(0, 2))



## scatterplot of number of WIWR per day (ARU consecutive 10 min) over time, 
## with fitted average model (BRT) as a line
# get standardized predictions for predictions to test data from all 1000 models
wiwr_aru10c_preds_brt <- bind_rows(lapply(fits_aruwiwr_brt, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$standardized_preds}))
}))

# get average prediction for each day from the 200 iterations of the 5-fold CV
warning("Add standard error to this at some point.  14 April WG.")
wiwr_aru10c_preds_brt <- group_by(wiwr_aru10c_preds_brt, day_of_yr) %>%
  summarise(mean_pred = mean(predictions))

ggplot(data = wiwr_aru10c_preds_brt, aes(x = day_of_yr, y = mean_pred)) + 
  geom_line() + 
  geom_point(data = sum_aruwiwr, aes(x = day_of_yr, y = meandet)) + 
  ggtitle("Winter Wren\nARU - consecutive 10 min; BRT") + 
  xlab("Julian day of year") + 
  ylab("Mean number of 30-second intervals\nwith a vocalization") + 
  theme_bw()

## WIWRARU10c over time- observed vs predicted values- one set of five-
## fold CV models
# put test predictions from all five folds into one df
p136_df <- bind_rows(lapply(fits_aruwiwr_brt$`136`, 
                            FUN = function(x) {x$test_predictions}))

ggplot(data = p136_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals\nwith a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization") +
  ggtitle("model group 136")

p2_df <- bind_rows(lapply(fits_aruwiwr_brt$`2`, 
                           FUN = function(x) {x$test_predictions}))

ggplot(data = p2_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals\nwith a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization")+
  ggtitle("model group 2")

p27_df <- bind_rows(lapply(fits_aruwiwr_brt$`27`, 
                          FUN = function(x) {x$test_predictions}))

ggplot(data = p27_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals\nwith a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization")+
  ggtitle("model group 27")

p198_df <- bind_rows(lapply(fits_aruwiwr_brt$`198`, 
                            FUN = function(x) {x$test_predictions}))

ggplot(data = p198_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals\nwith a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization") +
  ggtitle("model group 198")

# get predictions to independent test data from all 1000 models
wiwr_aru10c_testpreds_brt <- bind_rows(lapply(fits_aruwiwr_brt, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$test_predictions}))
}))

ggplot(data = wiwr_aru10c_testpreds_brt, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlab("mean predicted 30-sec intervals\nwith a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization") + 
  ggtitle("Winter Wren\nARU- 10 consecutive min; 1000 BRTs")


# ## WIWRARU10c BRT- observed vs predicted values
# wiwr_aru10c_preds_brt <- left_join(wiwr_aru10c_preds_brt, sum_wiwr, 
#                                    by = "day_of_yr")
# 
# ggplot(data = wiwr_aru10c_preds_brt, aes(x = meandet, y = mean_pred)) + 
#   geom_point() + 
#   geom_smooth() + 
#   geom_abline(intercept = 0, slope = 1) + 
#   theme_bw()+
#   xlab("mean observed vocalizations\nper 30 second interval") +
#   ylab("mean predicted vocalizations\nper 30 second interval") +
#   ggtitle("WIWR ARU- 10 consecutive min\nBRT predicted vs. observed") +
#   ylim(c(0, 2))


################################################################################
##GAM abundance summary plots
## scatterplot of number of GCKI per day (point counts) over time, with fitted 
## average model (BRT) as a line
#fits_gcki_gam <- readRDS("fits_gcki_brt.rds")

# get standardized predictions for predictions to test data from all 1000 models
gcki_ptct_preds_gam <- bind_rows(lapply(fits_gcki_gam, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$standardized_preds}))
}))

# get average prediction for each day from the 200 iterations of the 5-fold CV
warning("Add standard error to this at some point.  14 April WG.")
gcki_ptct_preds_gam <- group_by(gcki_ptct_preds_gam, day_of_yr) %>%
  summarise(mean_pred = mean(predictions))

ggplot(data = gcki_ptct_preds_gam, aes(x = day_of_yr, y = mean_pred)) + 
  geom_line() + 
  geom_point(data = sum_gcki, aes(x = day_of_yr, y = meandet)) + 
  ggtitle("Golden-crowned Kinglet\nPoint counts (10 min); GAM") + 
  xlab("Julian day of year") + 
  ylab("Mean number of individuals per point count") + 
  theme_bw()

g77_df <- bind_rows(lapply(fits_gcki_gam$`200`, 
                            FUN = function(x) {x$standardized_preds}))
ggplot(data = g77_df, aes(x = day_of_yr, y = predictions)) + 
  geom_line() + 
  geom_point(data = sum_gcki, aes(x = day_of_yr, y = meandet)) + 
  ggtitle("Golden-crowned Kinglet\nPoint counts (10 min)\nGAM ex. model 77") + 
  xlab("Julian day of year") + 
  ylab("Mean number of individuals per point count") + 
  theme_bw()


## GCKIPointCounts over time- observed vs predicted values- one set of five-
## fold CV models-- GAM
# put test predictions from all five folds into one df
g4_df <- bind_rows(lapply(fits_gcki_gam$`4`, 
                          FUN = function(x) {x$test_predictions}))

ggplot(data = g4_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count") +
  ggtitle("model group 4")

g18_df <- bind_rows(lapply(fits_gcki_gam$`18`, 
                           FUN = function(x) {x$test_predictions}))

ggplot(data = g18_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count")+
  ggtitle("model group 18")

g112_df <- bind_rows(lapply(fits_gcki_gam$`112`, 
                            FUN = function(x) {x$test_predictions}))

ggplot(data = g112_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count")+
  ggtitle("model group 112")

p94_df <- bind_rows(lapply(fits_wiwr_brt$`94`, 
                           FUN = function(x) {x$test_predictions}))

ggplot(data = p94_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count") +
  ggtitle("model group 94")

# get predictions to independent test data from all 1000 models
gcki_ptct_testpreds_gam <- bind_rows(lapply(fits_gcki_gam, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$test_predictions}))
}))

ggplot(data = gcki_ptct_testpreds_gam, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count") + 
  ggtitle("Golden-crowned Kinglet\nPoint counts (10 min); 1000 GAMs")


#GCKI- ARu 10 min cts
gcki_aru10c_preds_gam <- bind_rows(lapply(fits_arugcki_gam, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$standardized_preds}))
}))

# get average prediction for each day from the 200 iterations of the 5-fold CV
warning("Add standard error to this at some point.  14 April WG.")
gcki_aru10c_preds_gam <- group_by(gcki_aru10c_preds_gam, day_of_yr) %>%
  summarise(mean_pred = mean(predictions))

ggplot(data = gcki_aru10c_preds_gam, aes(x = day_of_yr, y = mean_pred)) + 
  geom_line() + 
  geom_point(data = sum_arugcki, aes(x = day_of_yr, y = meandet)) + 
  ggtitle("Golden-crowned Kinglet\nARU - consecutive 10 min GAM") + 
  xlab("Julian day of year") + 
  ylab("Mean number of 30-second intervals\nwith a vocalization") + 
  theme_bw()

## GCKIARU10c over time- observed vs predicted values- one set of five-
## fold CV models
# put test predictions from all five folds into one df
p140_df <- bind_rows(lapply(fits_arugcki_brt$`140`, 
                            FUN = function(x) {x$test_predictions}))

ggplot(data = p140_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals\nwith a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization") +
  ggtitle("model group 140")

p71_df <- bind_rows(lapply(fits_arugcki_brt$`71`, 
                           FUN = function(x) {x$test_predictions}))

ggplot(data = p71_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals\nwith a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization")+
  ggtitle("model group 71")

p6_df <- bind_rows(lapply(fits_arugcki_brt$`6`, 
                          FUN = function(x) {x$test_predictions}))

ggplot(data = p6_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals\nwith a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization")+
  ggtitle("model group 6")

p199_df <- bind_rows(lapply(fits_arugcki_brt$`199`, 
                            FUN = function(x) {x$test_predictions}))

ggplot(data = p199_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals\nwith a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization") +
  ggtitle("model group 199")

# get predictions to independent test data from all 1000 models
gcki_aru10c_testpreds_brt <- bind_rows(lapply(fits_arugcki_brt, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$test_predictions}))
}))

ggplot(data = gcki_aru10c_testpreds_brt, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlab("mean predicted 30-sec intervals\nwith a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization") + 
  ggtitle("Golden-crowned Kinglets\nARU- 10 consecutive min; 1000 GAMs")


#### WIWR GAM Point counts
# get standardized predictions for predictions to test data from all 1000 models
wiwr_ptct_preds_gam <- bind_rows(lapply(fits_wiwr_gam, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$standardized_preds}))
}))

# get average prediction for each day from the 200 iterations of the 5-fold CV
warning("Add standard error to this at some point.  14 April WG.")
wiwr_ptct_preds_gam <- group_by(wiwr_ptct_preds_gam, day_of_yr) %>%
  summarise(mean_pred = mean(predictions))

ggplot(data = wiwr_ptct_preds_gam, aes(x = day_of_yr, y = mean_pred)) + 
  geom_line() + 
  geom_point(data = sum_wiwr, aes(x = day_of_yr, y = meandet)) + 
  ggtitle("Winter Wren\nPoint counts (10 min); GAM") + 
  xlab("Julian day of year") + 
  ylab("Mean number of individuals per point count") + 
  theme_bw()

## WIWRPointCounts over time- observed vs predicted values- one set of five-
## fold CV models-- GAM
# put test predictions from all five folds into one df
gw8_df <- bind_rows(lapply(fits_wiwr_gam$`8`, 
                          FUN = function(x) {x$test_predictions}))

ggplot(data = gw8_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count") +
  ggtitle("model group 8")

gw37_df <- bind_rows(lapply(fits_wiwr_gam$`37`, 
                           FUN = function(x) {x$test_predictions}))

ggplot(data = gw37_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count")+
  ggtitle("model group 37")

gw103_df <- bind_rows(lapply(fits_wiwr_gam$`103`, 
                            FUN = function(x) {x$test_predictions}))

ggplot(data = gw103_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count")+
  ggtitle("model group 103")

gw96_df <- bind_rows(lapply(fits_wiwr_gam$`96`, 
                           FUN = function(x) {x$test_predictions}))

ggplot(data = gw96_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count") +
  ggtitle("model group 96")

# get predictions to independent test data from all 1000 models
wiwr_ptct_testpreds_gam <- bind_rows(lapply(fits_wiwr_gam, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$test_predictions}))
}))

ggplot(data = wiwr_ptct_testpreds_gam, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count") + 
  ggtitle("Winter Wren\nPoint counts (10 min); 1000 GAMs")

#### WIWR GAM ARU 10 consec. min
# get standardized predictions for predictions to test data from all 1000 models
wiwr_aru10c_preds_gam <- bind_rows(lapply(fits_aruwiwr_gam, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$standardized_preds}))
}))

# get average prediction for each day from the 200 iterations of the 5-fold CV
warning("Add standard error to this at some point.  14 April WG.")
wiwr_aru10c_preds_gam <- group_by(wiwr_aru10c_preds_gam, day_of_yr) %>%
  summarise(mean_pred = mean(predictions))

ggplot(data = wiwr_aru10c_preds_gam, aes(x = day_of_yr, y = mean_pred)) + 
  geom_line() + 
  geom_point(data = sum_aruwiwr, aes(x = day_of_yr, y = meandet)) + 
  ggtitle("Winter Wren\nARU- 10 consecutive min; GAM") + 
  xlab("Julian day of year") + 
  ylab("Mean number of 30-sec intervals\nwith a vocalization") + 
  theme_bw()

## WIWR ARU 10 consec.- observed vs predicted values- one set of five-
## fold CV models-- GAM
# put test predictions from all five folds into one df
gw4_df <- bind_rows(lapply(fits_aruwiwr_gam$`4`, 
                          FUN = function(x) {x$test_predictions}))

ggplot(data = gw4_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals with a vocalization") +
  ylab("mean observed 30-sec intervals with a vocalization") +
  ggtitle("model group 4")

gw18_df <- bind_rows(lapply(fits_aruwiwr_gam$`18`, 
                           FUN = function(x) {x$test_predictions}))

ggplot(data = gw18_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals with a vocalization") +
  ylab("mean observed 30-sec intervals with a vocalization")+
  ggtitle("model group 18")

gw112_df <- bind_rows(lapply(fits_aruwiwr_gam$`112`, 
                            FUN = function(x) {x$test_predictions}))

ggplot(data = gw112_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals with a vocalization") +
  ylab("mean observed 30-sec intervals with a vocalization")+
  ggtitle("model group 112")

pw94_df <- bind_rows(lapply(fits_aruwiwr_gam$`94`, 
                           FUN = function(x) {x$test_predictions}))

ggplot(data = pw94_df, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals with a vocalization") +
  ylab("mean observed 30-sec intervals with a vocalization") +
  ggtitle("model group 94")

# get predictions to independent test data from all 1000 models
wiwr_aru10c_testpreds_gam <- bind_rows(lapply(fits_aruwiwr_gam, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$test_predictions}))
}))

ggplot(data = wiwr_aru10c_testpreds_gam, aes(x = OOB_preds, y = meandet)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlab("mean predicted 30-sec intervals\n with a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization") + 
  ggtitle("Winter Wren\nARU- 10 consecutive min; 1000 GAMs")



#### END ABUNDANCE SUMMARY PLOTS-----------------------------------------------







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

