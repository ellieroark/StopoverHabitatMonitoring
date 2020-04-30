################################
## PLOTS: ARUs vs pt cts
## Point Abbaye 2019
## 
## author: Ellie Roark
## created: 27 Nov 2019
## last modified: 28 Apr 2020
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

library(wgutil)
library(tidyverse)
library(Hmisc)
library(lme4)

t_size <- 10
#setwd("/home/emer/Dropbox/Ellie Roark/R/PointAbbaye/")

### clean up environment
rm(arugcki, arugcki.day.gam, arugcki.day.gam2, aruwiwr, aruwiwr.day.gam, 
   aruwiwr.day.gam2)
##TODO add more to this list once all plots are created and I can remove 
##extraneous objects  

### SUMMARY PLOTS FOR ABUNDANCE MODELS------------------------------------------


## scatterplot of number of GCKI per day (point counts) over time, with fitted 
## average model (BRT) as a line
fits_gcki_brt <- readRDS("fits_gcki_brt.rds")

# get standardized predictions for predictions to test data from all 1000 models
gcki_ptct_preds_brt <- bind_rows(lapply(fits_gcki_brt, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$standardized_preds}))
}))

# get average prediction for each day from the 200 iterations of the 5-fold CV
warning("Add standard error to this at some point.  14 April WG.")
gcki_ptct_preds_brt <- group_by(gcki_ptct_preds_brt, day_of_yr) %>%
  summarise(mean_pred = mean(predictions))

g4p1 <- ggplot(data = gcki_ptct_preds_brt, aes(x = day_of_yr, y = mean_pred)) + 
  geom_line() + 
  geom_point(data = sum_gcki, aes(x = day_of_yr, y = meandet)) + 
  ggtitle("Golden-crowned Kinglet\nPoint counts (10 min); BRT") + 
  xlab("Julian day of year") + 
  ylab("Mean number of individuals per point count") + 
  theme_bw()

# b200_df <- bind_rows(lapply(fits_gcki_brt$`200`, 
#                             FUN = function(x) {x$standardized_preds}))
# ggplot(data = b200_df, aes(x = day_of_yr, y = predictions)) + 
#   geom_line() + 
#   geom_point(data = sum_gcki, aes(x = day_of_yr, y = meandet)) + 
#   ggtitle("Golden-crowned Kinglet\nPoint counts (10 min)\nBRT ex. model 200") + 
#   xlab("Julian day of year") + 
#   ylab("Mean number of individuals per point count") + 
#   theme_bw()
# 
# ## GCKIPointCounts over time- observed vs predicted values- one set of five-
# ## fold CV models
# # put test predictions from all five folds into one df
# p200_df <- bind_rows(lapply(fits_gcki_brt$`200`, 
#                           FUN = function(x) {x$test_predictions}))
# 
# ggplot(data = p200_df, aes(x = OOB_preds, y = meandet)) + 
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1) +
#   theme_bw() +
#   xlab("mean predicted individuals per count") +
#   ylab("mean observed individuals per count") +
#   ggtitle("model group 200")
# 
# p19_df <- bind_rows(lapply(fits_gcki_brt$`19`, 
#                           FUN = function(x) {x$test_predictions}))
# 
# ggplot(data = p19_df, aes(x = OOB_preds, y = meandet)) + 
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1) +
#   theme_bw() +
#   xlab("mean predicted individuals per count") +
#   ylab("mean observed individuals per count")+
#   ggtitle("model group 19")
# 
# p112_df <- bind_rows(lapply(fits_gcki_brt$`112`, 
#                           FUN = function(x) {x$test_predictions}))
# 
# ggplot(data = p112_df, aes(x = OOB_preds, y = meandet)) + 
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1) +
#   theme_bw() +
#   xlab("mean predicted individuals per count") +
#   ylab("mean observed individuals per count")+
#   ggtitle("model group 112")
# 
# p94_df <- bind_rows(lapply(fits_gcki_brt$`94`, 
#                           FUN = function(x) {x$test_predictions}))
# 
# ggplot(data = p94_df, aes(x = OOB_preds, y = meandet)) + 
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1) +
#   theme_bw() +
#   xlab("mean predicted individuals per count") +
#   ylab("mean observed individuals per count") +
#   ggtitle("model group 94")
# 
# # get predictions to independent test data from all 1000 models
# gcki_ptct_testpreds_brt <- bind_rows(lapply(fits_gcki_brt, FUN = function(x) {
#   bind_rows(lapply(x, FUN = function(y) {y$test_predictions}))
# }))
# 
# ggplot(data = gcki_ptct_testpreds_brt, aes(x = OOB_preds, y = meandet)) + 
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1) +
#   xlab("mean predicted individuals per count") +
#   ylab("mean observed individuals per count") + 
#   ggtitle("Golden-crowned Kinglets\nPoint counts (10 min); 1000 BRTs")
# 

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
fits_arugcki_brt <- readRDS("fits_arugcki_brt.rds")

gcki_aru10c_preds_brt <- bind_rows(lapply(fits_arugcki_brt, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$standardized_preds}))
}))

# get average prediction for each day from the 200 iterations of the 5-fold CV
warning("Add standard error to this at some point.  14 April WG.")
gcki_aru10c_preds_brt <- group_by(gcki_aru10c_preds_brt, day_of_yr) %>%
  summarise(mean_pred = mean(predictions))

g4p2 <- ggplot(data = gcki_aru10c_preds_brt, aes(x = day_of_yr, y = mean_pred)) + 
  geom_line() + 
  geom_point(data = sum_arugcki, aes(x = day_of_yr, y = meandet)) + 
  ggtitle("Golden-crowned Kinglet\nARU - consecutive 10 min BRT") + 
  xlab("Julian day of year") + 
  ylab("Mean number of 30-second intervals\nwith a vocalization") + 
  theme_bw()

# ## GCKIARU10c over time- observed vs predicted values- one set of five-
# ## fold CV models
# # put test predictions from all five folds into one df
# p140_df <- bind_rows(lapply(fits_arugcki_brt$`140`, 
#                             FUN = function(x) {x$test_predictions}))
# 
# ggplot(data = p140_df, aes(x = OOB_preds, y = meandet)) + 
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1) +
#   theme_bw() +
#   xlab("mean predicted 30-sec intervals\nwith a vocalization") +
#   ylab("mean observed 30-sec intervals\nwith a vocalization") +
#   ggtitle("model group 140")
# 
# p71_df <- bind_rows(lapply(fits_arugcki_brt$`71`, 
#                            FUN = function(x) {x$test_predictions}))
# 
# ggplot(data = p71_df, aes(x = OOB_preds, y = meandet)) + 
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1) +
#   theme_bw() +
#   xlab("mean predicted 30-sec intervals\nwith a vocalization") +
#   ylab("mean observed 30-sec intervals\nwith a vocalization")+
#   ggtitle("model group 71")
# 
# p6_df <- bind_rows(lapply(fits_arugcki_brt$`6`, 
#                             FUN = function(x) {x$test_predictions}))
# 
# ggplot(data = p6_df, aes(x = OOB_preds, y = meandet)) + 
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1) +
#   theme_bw() +
#   xlab("mean predicted 30-sec intervals\nwith a vocalization") +
#   ylab("mean observed 30-sec intervals\nwith a vocalization")+
#   ggtitle("model group 6")
# 
# p199_df <- bind_rows(lapply(fits_arugcki_brt$`199`, 
#                            FUN = function(x) {x$test_predictions}))
# 
# ggplot(data = p199_df, aes(x = OOB_preds, y = meandet)) + 
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1) +
#   theme_bw() +
#   xlab("mean predicted 30-sec intervals\nwith a vocalization") +
#   ylab("mean observed 30-sec intervals\nwith a vocalization") +
#   ggtitle("model group 199")
# 
# # get predictions to independent test data from all 1000 models
# gcki_aru10c_testpreds_brt <- bind_rows(lapply(fits_arugcki_brt, FUN = function(x) {
#   bind_rows(lapply(x, FUN = function(y) {y$test_predictions}))
# }))
# 
# ggplot(data = gcki_aru10c_testpreds_brt, aes(x = OOB_preds, y = meandet)) + 
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1) +
#   xlab("mean predicted 30-sec intervals\nwith a vocalization") +
#   ylab("mean observed 30-sec intervals\nwith a vocalization") + 
#   ggtitle("Golden-crowned Kinglets\nARU- 10 consecutive min; 1000 BRTs")

rm(fits_arugcki_brt)
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
 
## scatterplot of number of GCKI per day (ARU random 10 min) over time, 
## with fitted average model (BRT) as a line
# get standardized predictions for predictions to test data from all 1000 models
fits_arugcki10r_brt <- readRDS("fits_arugcki10r_brt.rds")

# get standardized predictions for predictions to test data from all 1000 models
gcki_aru10r_preds_brt <- bind_rows(lapply(fits_arugcki10r_brt, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$standardized_preds}))
}))

# get average prediction for each day from the 200 iterations of the 5-fold CV
warning("Add standard error to this at some point.  14 April WG.")
gcki_aru10r_preds_brt <- group_by(gcki_aru10r_preds_brt, day_of_yr) %>%
  summarise(mean_pred = mean(predictions))

g4p3 <- ggplot(data = gcki_aru10r_preds_brt, aes(x = day_of_yr, y = mean_pred)) + 
  geom_line() + 
  geom_point(data = sum_arugcki10r, aes(x = day_of_yr, y = meandet)) + 
  ggtitle("Golden-crowned Kinglet\nARU (10 random min); BRT") + 
  xlab("Julian day of year") + 
  ylab("Mean number of vocalizations\nper 30-sec interval") + 
  theme_bw()

rm(fits_arugcki10r_brt)

## scatterplot of number of GCKI per day (ARU random 22 min) over time, 
## with fitted average model (BRT) as a line
# get standardized predictions for predictions to test data from all 1000 models
fits_arugcki22r_brt <- readRDS("fits_arugcki22r_brt.rds")

# get standardized predictions for predictions to test data from all 1000 models
gcki_aru22r_preds_brt <- bind_rows(lapply(fits_arugcki22r_brt, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$standardized_preds}))
}))

# get average prediction for each day from the 200 iterations of the 5-fold CV
warning("Add standard error to this at some point.  14 April WG.")
gcki_aru22r_preds_brt <- group_by(gcki_aru22r_preds_brt, day_of_yr) %>%
  summarise(mean_pred = mean(predictions))

g4p4 <- ggplot(data = gcki_aru22r_preds_brt, aes(x = day_of_yr, y = mean_pred)) + 
  geom_line() + 
  geom_point(data = sum_arugcki22r, aes(x = day_of_yr, y = meandet)) + 
  ggtitle("Golden-crowned Kinglet\nARU (22 random min); BRT") + 
  xlab("Julian day of year") + 
  ylab("Mean number of vocalizations\nper 30-sec interval") + 
  theme_bw()

rm(fits_arugcki22r_brt)



## scatterplot of number of WIWR per day (Point Counts 10 min) over time, 
## with fitted average model (BRT) as a line
# get standardized predictions for predictions to test data from all 1000 models
fits_wiwr_brt <- readRDS("fits_wiwr_brt.rds")
wiwr_ptct_preds_brt <- bind_rows(lapply(fits_wiwr_brt, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$standardized_preds}))
}))

# get average prediction for each day from the 200 iterations of the 5-fold CV
warning("Add standard error to this at some point.  14 April WG.")
wiwr_ptct_preds_brt <- group_by(wiwr_ptct_preds_brt, day_of_yr) %>%
  summarise(mean_pred = mean(predictions))

g4p5 <- ggplot(data = wiwr_ptct_preds_brt, aes(x = day_of_yr, y = mean_pred)) + 
  geom_line() + 
  geom_point(data = sum_wiwr, aes(x = day_of_yr, y = meandet)) + 
  # ggtitle("Winter Wren\nPoint counts (10 min); BRT") + 
  ggtitle("Point Counts\n(a)") + 
  xlab("") + 
  ylab("Mean number\nof individuals\nper point count") + 
  theme_bw()

rm(fits_wiwr_brt)

## scatterplot of number of WIWR per day (ARU 10 rand min) over time, 
## with fitted average model (BRT) as a line
# get standardized predictions for predictions to test data from all 1000 models
fits_aruwiwr10r_brt <- readRDS("fits_aruwiwr10r_brt.rds")

wiwr_aru10r_preds_brt <- bind_rows(lapply(fits_aruwiwr10r_brt, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$standardized_preds}))
}))

# get average prediction for each day from the 200 iterations of the 5-fold CV
warning("Add standard error to this at some point.  14 April WG.")
wiwr_aru10r_preds_brt <- group_by(wiwr_aru10r_preds_brt, day_of_yr) %>%
  summarise(mean_pred = mean(predictions))

g4p7 <- ggplot(data = wiwr_aru10r_preds_brt, aes(x = day_of_yr, y = mean_pred)) + 
  geom_line() + 
  geom_point(data = sum_aruwiwr10r, aes(x = day_of_yr, y = meandet)) + 
  ggtitle("Winter Wren\nARU (10 random min); BRT") + 
  xlab("Julian day of year") + 
  ylab("Mean number of vocalizations\nper 30-second interval") + 
  theme_bw()

rm(fits_aruwiwr10r_brt)

## scatterplot of number of WIWR per day (ARU 22 rand min) over time, 
## with fitted average model (BRT) as a line
# get standardized predictions for predictions to test data from all 1000 models
fits_aruwiwr22r_brt <- readRDS("fits_aruwiwr22r_brt.rds")

wiwr_aru22r_preds_brt <- bind_rows(lapply(fits_aruwiwr22r_brt, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$standardized_preds}))
}))

# get average prediction for each day from the 200 iterations of the 5-fold CV
warning("Add standard error to this at some point.  14 April WG.")
wiwr_aru22r_preds_brt <- group_by(wiwr_aru22r_preds_brt, day_of_yr) %>%
  summarise(mean_pred = mean(predictions))

g4p8 <- ggplot(data = wiwr_aru22r_preds_brt, aes(x = day_of_yr, y = mean_pred)) + 
  geom_line() + 
  geom_point(data = sum_aruwiwr22r, aes(x = day_of_yr, y = meandet)) + 
  ggtitle("Winter Wren\nARU (22 random min); BRT") + 
  xlab("Julian day of year") + 
  ylab("Mean number of vocalizations\nper 30-second interval") + 
  theme_bw()

rm(fits_aruwiwr10r_brt)

# ## WIWRPointCounts over time- observed vs predicted values- one set of five-
# ## fold CV models
# # put test predictions from all five folds into one df
# p4_df <- bind_rows(lapply(fits_wiwr_brt$`4`, 
#                             FUN = function(x) {x$test_predictions}))
# 
# ggplot(data = p4_df, aes(x = OOB_preds, y = meandet)) + 
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1) +
#   theme_bw() +
#   xlab("mean predicted individuals per count") +
#   ylab("mean observed individuals per count") +
#   ggtitle("model group 4")
# 
# p18_df <- bind_rows(lapply(fits_wiwr_brt$`18`, 
#                            FUN = function(x) {x$test_predictions}))
# 
# ggplot(data = p18_df, aes(x = OOB_preds, y = meandet)) + 
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1) +
#   theme_bw() +
#   xlab("mean predicted individuals per count") +
#   ylab("mean observed individuals per count")+
#   ggtitle("model group 18")
# 
# p112_df <- bind_rows(lapply(fits_wiwr_brt$`112`, 
#                             FUN = function(x) {x$test_predictions}))
# 
# ggplot(data = p112_df, aes(x = OOB_preds, y = meandet)) + 
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1) +
#   theme_bw() +
#   xlab("mean predicted individuals per count") +
#   ylab("mean observed individuals per count")+
#   ggtitle("model group 112")
# 
# p94_df <- bind_rows(lapply(fits_wiwr_brt$`94`, 
#                            FUN = function(x) {x$test_predictions}))
# 
# ggplot(data = p94_df, aes(x = OOB_preds, y = meandet)) + 
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1) +
#   theme_bw() +
#   xlab("mean predicted individuals per count") +
#   ylab("mean observed individuals per count") +
#   ggtitle("model group 94")
# 
# # get predictions to independent test data from all 1000 models
# wiwr_ptct_testpreds_brt <- bind_rows(lapply(fits_wiwr_brt, FUN = function(x) {
#   bind_rows(lapply(x, FUN = function(y) {y$test_predictions}))
# }))
# 
# ggplot(data = wiwr_ptct_testpreds_brt, aes(x = OOB_preds, y = meandet)) + 
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1) +
#   xlab("mean predicted individuals per count") +
#   ylab("mean observed individuals per count") + 
#   ggtitle("Winter Wren\nPoint counts (10 min); 1000 BRTs")

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
fits_aruwiwr_brt <- readRDS("fits_aruwiwr_brt.rds")

wiwr_aru10c_preds_brt <- bind_rows(lapply(fits_aruwiwr_brt, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$standardized_preds}))
}))

# get average prediction for each day from the 200 iterations of the 5-fold CV
warning("Add standard error to this at some point.  14 April WG.")
wiwr_aru10c_preds_brt <- group_by(wiwr_aru10c_preds_brt, day_of_yr) %>%
  summarise(mean_pred = mean(predictions))

g4p6 <- ggplot(data = wiwr_aru10c_preds_brt, aes(x = day_of_yr, y = mean_pred)) + 
  geom_line() + 
  geom_point(data = sum_aruwiwr, aes(x = day_of_yr, y = meandet)) + 
  ggtitle("Winter Wren\nARU - consecutive 10 min; BRT") + 
  ylab("Mean number\nof 30-second intervals\nwith a vocalization") + 
  xlab("") + 
  scale_x_continuous(breaks = c(91, 105, 121, 135), 
                     labels = c("April 1", "April 15", "May 1", "May 15")) + 
  theme_bw()

# ## WIWRARU10c over time- observed vs predicted values- one set of five-
# ## fold CV models
# # put test predictions from all five folds into one df
# p136_df <- bind_rows(lapply(fits_aruwiwr_brt$`136`, 
#                             FUN = function(x) {x$test_predictions}))
# 
# ggplot(data = p136_df, aes(x = OOB_preds, y = meandet)) + 
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1) +
#   theme_bw() +
#   xlab("mean predicted 30-sec intervals\nwith a vocalization") +
#   ylab("mean observed 30-sec intervals\nwith a vocalization") +
#   ggtitle("model group 136")
# 
# p2_df <- bind_rows(lapply(fits_aruwiwr_brt$`2`, 
#                            FUN = function(x) {x$test_predictions}))
# 
# ggplot(data = p2_df, aes(x = OOB_preds, y = meandet)) + 
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1) +
#   theme_bw() +
#   xlab("mean predicted 30-sec intervals\nwith a vocalization") +
#   ylab("mean observed 30-sec intervals\nwith a vocalization")+
#   ggtitle("model group 2")
# 
# p27_df <- bind_rows(lapply(fits_aruwiwr_brt$`27`, 
#                           FUN = function(x) {x$test_predictions}))
# 
# ggplot(data = p27_df, aes(x = OOB_preds, y = meandet)) + 
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1) +
#   theme_bw() +
#   xlab("mean predicted 30-sec intervals\nwith a vocalization") +
#   ylab("mean observed 30-sec intervals\nwith a vocalization")+
#   ggtitle("model group 27")
# 
# p198_df <- bind_rows(lapply(fits_aruwiwr_brt$`198`, 
#                             FUN = function(x) {x$test_predictions}))
# 
# ggplot(data = p198_df, aes(x = OOB_preds, y = meandet)) + 
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1) +
#   theme_bw() +
#   xlab("mean predicted 30-sec intervals\nwith a vocalization") +
#   ylab("mean observed 30-sec intervals\nwith a vocalization") +
#   ggtitle("model group 198")
# 
# # get predictions to independent test data from all 1000 models
# wiwr_aru10c_testpreds_brt <- bind_rows(lapply(fits_aruwiwr_brt, FUN = function(x) {
#   bind_rows(lapply(x, FUN = function(y) {y$test_predictions}))
# }))
# 
# ggplot(data = wiwr_aru10c_testpreds_brt, aes(x = OOB_preds, y = meandet)) + 
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1) +
#   xlab("mean predicted 30-sec intervals\nwith a vocalization") +
#   ylab("mean observed 30-sec intervals\nwith a vocalization") + 
#   ggtitle("Winter Wren\nARU- 10 consecutive min; 1000 BRTs")

rm(fits_aruwiwr_brt)

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

## abundance data and BRT models for 2 species for all 4 survey methods
multiplot(g4p5, g4p1, g4p6, g4p2, g4p7, g4p3,  g4p8, g4p4, 
          layout = matrix(c(1, 2, 3, 4, 5, 6, 7, 8), nrow = 4, byrow = TRUE))

multiplot(g4p5, g4p6, g4p7, g4p8, g4p1, g4p2, g4p3, g4p4, 
          layout = matrix(c(1, 2, 3, 4, 5, 6, 7, 8), nrow = 2, byrow = TRUE))


## correlation plots for predicted and observed abundance metrics






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
## plot of coefficients for GLMM (all count types)
## example from wg:
## # make a data frame to use in ggplot
maxrand_df <- data.frame(matrix(nrow = 19, ncol = 4))
colnames(maxrand_df) <- c("variable", "coef_estimate", "l_bound", "h_bound")
maxrand_df$variable <- c("Count type (ARU- 10 min consecutive)", 
                    "Count type (ARU- 10 min random)", 
                    "Count type (ARU- 22 min random)", "Wind (2)", "Wind (3+)",
                    "Rain (Wet)", "Noise (1)", "Noise (>2)", "Day of year", 
                    "Day of year squared",
                    "Count type (ARU- 10 min consecutive)*Rain (Wet)", 
                    "Count type (ARU- 10 min random)*Rain (Wet)",
                    "Count type (ARU- 22 min random)*Rain (Wet)",
                    "Count type (ARU- 10 min consecutive)*Day of Year",
                    "Count type (ARU- 10 min random)*Day of Year",
                    "Count type (ARU- 22 min random)*Day of Year",
                    "Count type (ARU- 10 min consecutive)*Day of year squared",
                    "Count type (ARU- 10 min random)*Day of year squared",
                    "Count type (ARU- 22 min random)*Day of year squared")

# get coefficient point estimate and lower and upper CI bounds for each
# predictor variable
for(i in 1:nrow(maxrand_df)) {
  # get coefficient point estimate
  maxrand_df$coef_estimate[i] <- summary(max.rand.spdetmm)$coefficients[1 + i]
  maxrand_df$pvalue[i] <- summary(max.rand.spdetmm)$coefficients[1+i, "Pr(>|z|)"]
  maxrand_df$l_bound[i] <- CI.maxrand[2 + i, "2.5 %"]
  maxrand_df$h_bound[i] <- CI.maxrand[2 + i, "97.5 %"]
}

# make graph
print(ggplot(data = maxrand_df,
             aes(x = factor(variable,
                            levels = c("Count type (ARU- 10 min consecutive)", 
                                       "Count type (ARU- 10 min random)", 
                                       "Count type (ARU- 22 min random)", "Wind (2)", "Wind (3+)",
                                       "Rain (Wet)", "Noise (1)", "Noise (>2)", "Day of year", 
                                       "Day of year squared",
                                       "Count type (ARU- 10 min consecutive)*Rain (Wet)", 
                                       "Count type (ARU- 10 min random)*Rain (Wet)",
                                       "Count type (ARU- 22 min random)*Rain (Wet)",
                                       "Count type (ARU- 10 min consecutive)*Day of Year",
                                       "Count type (ARU- 10 min random)*Day of Year",
                                       "Count type (ARU- 22 min random)*Day of Year",
                                       "Count type (ARU- 10 min consecutive)*Day of year squared",
                                       "Count type (ARU- 10 min random)*Day of year squared",
                                       "Count type (ARU- 22 min random)*Day of year squared"),
                            labels = c("Count type (ARU\n10 min consecutive)", 
                                       "Count type (ARU\n10 min random)", 
                                       "Count type (ARU\n22 min random)", "Wind (2)", "Wind (3+)",
                                       "Rain (Wet)", "Noise (1)", "Noise (>2)", "Day of year", 
                                       "Day of year squared",
                                       "Count type (ARU\n10 min consecutive)*Rain (Wet)", 
                                       "Count type (ARU\n10 min random)*Rain (Wet)",
                                       "Count type (ARU\n22 min random)*Rain (Wet)",
                                       "Count type (ARU\n10 min consecutive)*Day of Year",
                                       "Count type (ARU\n10 min random)*Day of Year",
                                       "Count type (ARU\n22 min random)*Day of Year",
                                       "Count type (ARU\n10 min consecutive)*Day of year squared",
                                       "Count type (ARU\n10 min random)*Day of year squared",
                                       "Count type (ARU\n22 min random)*Day of year squared")),
                 y = coef_estimate)) +
        geom_point() +
        geom_linerange(aes(x = factor(variable,
                                      levels = c("Count type (ARU- 10 min consecutive)", 
                                                 "Count type (ARU- 10 min random)", 
                                                 "Count type (ARU- 22 min random)", "Wind (2)", "Wind (3+)",
                                                 "Rain (Wet)", "Noise (1)", "Noise (>2)", "Day of year", 
                                                 "Day of year squared",
                                                 "Count type (ARU- 10 min consecutive)*Rain (Wet)", 
                                                 "Count type (ARU- 10 min random)*Rain (Wet)",
                                                 "Count type (ARU- 22 min random)*Rain (Wet)",
                                                 "Count type (ARU- 10 min consecutive)*Day of Year",
                                                 "Count type (ARU- 10 min random)*Day of Year",
                                                 "Count type (ARU- 22 min random)*Day of Year",
                                                 "Count type (ARU- 10 min consecutive)*Day of year squared",
                                                 "Count type (ARU- 10 min random)*Day of year squared",
                                                 "Count type (ARU- 22 min random)*Day of year squared"),
                                      labels = c("Count type- ARU 10 min\nconsecutive", 
                                                 "Count type- ARU 10 min\nrandom", 
                                                 "Count type- ARU 22 min\nrandom", "Wind (2)", "Wind (3+)",
                                                 "Rain (Wet)", "Noise (1)", "Noise (>2)", "Day of year", 
                                                 "Day of year squared",
                                                 "Count type- ARU 10 min\nconsecutive *Rain (Wet)", 
                                                 "Count type- ARU 10 min random)*Rain (Wet)",
                                                 "Count type (ARU\n22 min random)*Rain (Wet)",
                                                 "Count type (ARU\n10 min consecutive)*Day of Year",
                                                 "Count type (ARU\n10 min random)*Day of Year",
                                                 "Count type (ARU\n22 min random)*Day of Year",
                                                 "Count type (ARU\n10 min consecutive)*Day of year squared",
                                                 "Count type (ARU\n10 min random)*Day of year squared",
                                                 "Count type (ARU\n22 min random)*Day of year squared")),
                           ymin = l_bound,
                           ymax = h_bound)) +
        xlab("Predictor Variable") +
        ylab("Coefficient estimate") +
        theme_bw() +
        ylim(-1, 1) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)))




##### ##species detected over time, stratified by count type, with plotted GLMM
## prediction line, from max.rand.spdetmm model! formula: 
## sp_detected ~ count_type + wind + rain + noise + day_of_yr_s +  
## rain:count_type + day_of_yr_s:count_type 
##  (1 | day_of_yr_s)
#first, create new df with response and predictor variable columns
mean_doy <- mean(spdet_4ct$day_of_yr)
sd_doy_c <- sd(spdet_4ct$day_of_yr_c)
aru10c.pred <- data.frame(day_of_yr = seq(min(spdet_4ct$day_of_yr), 
                                       max(spdet_4ct$day_of_yr), by = 1))
aru10c.pred$day_of_yr_c <- aru10c.pred$day_of_yr-mean_doy
aru10c.pred$day_of_yr_s <- aru10c.pred$day_of_yr_c/sd_doy_c
#ptct.pred <- data.frame(day_of_yr_s = spdet_all$day_of_yr_s)
aru10c.pred$day_sq_s <- aru10c.pred$day_of_yr_s^2
aru10c.pred$count_type <- as.factor("aru")
aru10c.pred$wind <- as.factor("0-1")
aru10c.pred$rain <- as.factor("Dry")
aru10c.pred$noise <- as.factor("0")
aru10c.pred$point_id <- as.factor("FB1")
#coerce factor variables to contain the same number of levels as the original 
aru10c.pred$count_type <- factor(aru10c.pred$count_type, 
                              levels = c("point", "aru", "aru_10r", "aru_22r"), 
                              labels = c("point", "aru", "aru_10r", "aru_22r"))
aru10c.pred$wind <- factor(aru10c.pred$wind, 
                        levels = c("0-1", "2", "3+"),
                        labels = c("0-1", "2", "3+"))
aru10c.pred$rain <- factor(aru10c.pred$rain, 
                        levels = c("Dry", "wet"),
                        labels = c("Dry", "wet"))
aru10c.pred$noise <- factor(aru10c.pred$noise, 
                         levels = c("0", "1", ">2"), 
                         labels = c("0", "1", ">2"))
aru10c.pred$point_id <- factor(aru10c.pred$point_id, 
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
aru10c.pred$preds <- predict(max.rand.spdetmm, newdata = aru10c.pred, 
                             type = "response", re.form = NA)


## now do the same thing with pt cts
# ptct.pred <- data.frame(day_of_yr_s = seq(-1.86, 1.61, by = 0.01))
ptct.pred <- data.frame(day_of_yr = seq(min(spdet_4ct$day_of_yr), 
                                        max(spdet_4ct$day_of_yr), by = 1))
ptct.pred$day_of_yr_c <- ptct.pred$day_of_yr-mean_doy
ptct.pred$day_of_yr_s <- ptct.pred$day_of_yr_c/sd_doy_c

#ptct.pred <- data.frame(day_of_yr_s = spdet_all$day_of_yr_s)
ptct.pred$day_sq_s <- ptct.pred$day_of_yr_s^2
ptct.pred$count_type <- as.factor("point")
ptct.pred$wind <- as.factor("0-1")
ptct.pred$rain <- as.factor("Dry")
ptct.pred$noise <- as.factor("0")
ptct.pred$point_id <- as.factor("FB1")
#coerce factor variables to contain the same number of levels as the original 
ptct.pred$count_type <- factor(ptct.pred$count_type, 
                               levels = c("point", "aru", "aru_10r", "aru_22r"), 
                               labels = c("point", "aru", "aru_10r", "aru_22r"))
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
ptct.pred$preds <- predict(max.rand.spdetmm, type = "response",
                           newdata = ptct.pred, re.form = NA)

## now do the same thing with aru10rand
aru10r.pred <- data.frame(day_of_yr = seq(min(spdet_4ct$day_of_yr), 
                                        max(spdet_4ct$day_of_yr), by = 1))
aru10r.pred$day_of_yr_c <- aru10r.pred$day_of_yr-mean_doy
aru10r.pred$day_of_yr_s <- aru10r.pred$day_of_yr_c/sd_doy_c
aru10r.pred$day_sq_s <- aru10r.pred$day_of_yr_s^2
aru10r.pred$count_type <- as.factor("aru_10r")
aru10r.pred$wind <- as.factor("0-1")
aru10r.pred$rain <- as.factor("Dry")
aru10r.pred$noise <- as.factor("0")
aru10r.pred$point_id <- as.factor("FB1")
#coerce factor variables to contain the same number of levels as the original 
aru10r.pred$count_type <- factor(aru10r.pred$count_type, 
                               levels = c("point", "aru", "aru_10r", "aru_20r"), 
                               labels = c("point", "aru", "aru_10r", "aru_20r"))
aru10r.pred$wind <- factor(aru10r.pred$wind, 
                         levels = c("0-1", "2", "3+"),
                         labels = c("0-1", "2", "3+"))
aru10r.pred$rain <- factor(aru10r.pred$rain, 
                         levels = c("Dry", "wet"),
                         labels = c("Dry", "wet"))
aru10r.pred$noise <- factor(aru10r.pred$noise, 
                          levels = c("0", "1", ">2"), 
                          labels = c("0", "1", ">2"))
aru10r.pred$point_id <- factor(aru10r.pred$point_id, 
                             levels = c("p1", "p2", "p3", "p4", "p5", "p6",
                                        "p8", "p9", "p10", "p11", "p12", 
                                        "p13","p14", "p15", "p17", "p18", 
                                        "FB1", "FB2"),
                             labels = c("p1", "p2", "p3", "p4", "p5", "p6",
                                        "p8", "p9", "p10", "p11", "p12", 
                                        "p13","p14", "p15", "p17", "p18", 
                                        "FB1", "FB2"))

#now make a new col with predictions
aru10r.pred$preds <- predict(max.rand.spdetmm, type = "response",
                           newdata = aru10r.pred, re.form = NA)

## now do the same thing with aru22rand
aru22r.pred <- data.frame(day_of_yr = seq(min(spdet_4ct$day_of_yr), 
                                          max(spdet_4ct$day_of_yr), by = 1))
aru22r.pred$day_of_yr_c <- aru22r.pred$day_of_yr-mean_doy
aru22r.pred$day_of_yr_s <- aru22r.pred$day_of_yr_c/sd_doy_c

aru22r.pred$day_sq_s <- aru22r.pred$day_of_yr_s^2
aru22r.pred$count_type <- as.factor("aru_22r")
aru22r.pred$wind <- as.factor("0-1")
aru22r.pred$rain <- as.factor("Dry")
aru22r.pred$noise <- as.factor("0")
aru22r.pred$point_id <- as.factor("FB1")
#coerce factor variables to contain the same number of levels as the original 
aru22r.pred$count_type <- factor(aru22r.pred$count_type, 
                                 levels = c("point", "aru", "aru_10r", "aru_22r"), 
                                 labels = c("point", "aru", "aru_10r", "aru_22r"))
aru22r.pred$wind <- factor(aru22r.pred$wind, 
                           levels = c("0-1", "2", "3+"),
                           labels = c("0-1", "2", "3+"))
aru22r.pred$rain <- factor(aru22r.pred$rain, 
                           levels = c("Dry", "wet"),
                           labels = c("Dry", "wet"))
aru22r.pred$noise <- factor(aru22r.pred$noise, 
                            levels = c("0", "1", ">2"), 
                            labels = c("0", "1", ">2"))
aru22r.pred$point_id <- factor(aru22r.pred$point_id, 
                               levels = c("p1", "p2", "p3", "p4", "p5", "p6",
                                          "p8", "p9", "p10", "p11", "p12", 
                                          "p13","p14", "p15", "p17", "p18", 
                                          "FB1", "FB2"),
                               labels = c("p1", "p2", "p3", "p4", "p5", "p6",
                                          "p8", "p9", "p10", "p11", "p12", 
                                          "p13","p14", "p15", "p17", "p18", 
                                          "FB1", "FB2"))

#now make a new col with predictions
aru22r.pred$preds <- predict(max.rand.spdetmm, type = "response",
                             newdata = aru22r.pred, re.form = NA)

#bind ptct.pred to aru10c.pred to aru10r.pred to aru22r.pred
pred.4ct <- bind_rows(ptct.pred, aru10c.pred, aru10r.pred, aru22r.pred)
spdet_time <- ggplot(pred.4ct, 
                     aes(x=day_of_yr, y=preds, 
                         color= factor(count_type, 
                                       levels = c("point", "aru", 
                                                  "aru_10r", "aru_22r"), 
                                       labels = c("Point count\n", 
                                                  "ARU - 10 min\nconsecutive\n", 
                                                  "ARU - 10 min\nrandom\n", 
                                                  "ARU - 22 min\nrandom\n")), 
                         group = factor(count_type, 
                                        levels = c("point", "aru", 
                                                   "aru_10r", "aru_22r"), 
                                        labels = c("Point count\n", 
                                                   "ARU - 10 min\nconsecutive\n", 
                                                   "ARU - 10 min\nrandom\n", 
                                                   "ARU - 22 min\nrandom\n")))) + 
  # geom_point(shape=1) +
  geom_line(size = t_size/10) + 
  geom_point(data = spdet_4ct,
             aes(x = day_of_yr, y = sp_detected, 
                 color = factor(count_type, 
                                levels = c("point", "aru", 
                                           "aru_10r", "aru_22r"), 
                                labels = c("Point count\n", 
                                           "ARU - 10 min\nconsecutive\n", 
                                           "ARU - 10 min\nrandom\n", 
                                           "ARU - 22 min\nrandom\n")))) +
  scale_colour_viridis_d(name = "Survey Method", option = "magma", 
                         begin = 0, end = 0.75) + 
  scale_y_continuous(breaks = c(0, 4, 8, 12, 16)) + 
  ylab("Number of Species") +
  xlab("Day of Year") + 
  theme_bw() + 
  theme(text = element_text(size = t_size))
  
spdet_time


#### END summary plots for species richness GLMM-------------------------------


### write out plots as jpgs ---------------------------------------------------
ggsave(spdet_time, filename = "./saved_objects/spdet_time.jpg", 
       width = 10, height = 6, 
       units = "cm", device = "jpeg")

### write out tables as .csvs---------------------------------------------------
# write out table of mixed model results for species richness model with four
# count types. 
maxrand_df[,-1] <- round(maxrand_df[,-1], digits = 4)
write_csv(maxrand_df, path = "./saved_objects/mixedmodel_results_speciesrichness.csv")



### print numbers needed for manuscript-----------------------------------------

