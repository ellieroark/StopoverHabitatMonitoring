################################
## SUPPLEMENTARY PLOTS: ARUs vs pt cts
## Point Abbaye 2019
## 
## author: Ellie Roark
## created: 1 May 2020
## last modified: 1 May 2020
## 
## inputs: *see StopoverHabitatMonitoring.R for scripts which must be run
##          before this script can be sourced
##
##         
## outputs: *a number of plots for use in publications from the Pt Abbaye 2019
##            ARU vs Pt ct project.
##            
## TODO: * 
################################
library(patchwork)


#### Species Richness plots-----------------------------------------------------
# plot of coefficients with 95% confidence intervals for max.rand.spdetmm, a
# GLMM for species detected over time depending on count type and weather 
# variables

# this result is in the main plots script as a table of coefficient values
ggplot(data = maxrand_df,
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
                                       "Count type (ARU- 22 min random)*Day of year squared")),
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
                                                 "Count type (ARU- 22 min random)*Day of year squared")),
                           ymin = l_bound,
                           ymax = h_bound)) +
        xlab("Predictor Variable") +
        ylab("Coefficient estimate") +
        theme_bw() +
        ylim(-1, 1) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))




#### End species richness plots-------------------------------------------------



#### Boosted Regression Tree summary plots--------------------------------------
## WIWR point count models------------------------------------------------------
fits_wiwr_brt <- readRDS("fits_wiwr_brt.rds")

## observed vs predicted values- one set of five-fold CV models
# put test predictions from all five folds into one df
p4_df <- bind_rows(lapply(fits_wiwr_brt$`4`,
                            FUN = function(x) {x$test_predictions}))

ggplot(data = p4_df, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count") +
  ggtitle("model group 4")

p18_df <- bind_rows(lapply(fits_wiwr_brt$`18`,
                           FUN = function(x) {x$test_predictions}))

ggplot(data = p18_df, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count")+
  ggtitle("model group 18")

p112_df <- bind_rows(lapply(fits_wiwr_brt$`112`,
                            FUN = function(x) {x$test_predictions}))

ggplot(data = p112_df, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count")+
  ggtitle("model group 112")

p94_df <- bind_rows(lapply(fits_wiwr_brt$`94`,
                           FUN = function(x) {x$test_predictions}))

ggplot(data = p94_df, aes(x = OOB_preds, y = resp)) +
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

ggplot(data = wiwr_ptct_testpreds_brt, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count") +
  ggtitle("Winter Wren\nPoint counts (10 min); 1000 BRTs")

## WIWRPointCounts over time- observed vs predicted values, standardized preds
wiwr_ptct_preds_brt <- left_join(wiwr_ptct_preds_brt, sum_wiwr, by = "day_of_yr")

ggplot(data = wiwr_ptct_preds_brt, aes(x = resp, y = mean_pred)) +
  geom_point() +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()+
  xlab("mean observed individuals per point count") +
  ylab("mean predicted individuals per point count") +
  ggtitle("WIWR point counts (10 min)\nBRT predicted vs. observed") +
  ylim(c(0, 2))

rm(fits_wiwr_brt)

## end WIWR point count models--------------------------------------------------

## WIWR ARU (10 consec minutes) models------------------------------------------
fits_aruwiwr_brt <- readRDS("fits_aruwiwr_brt.rds")

## observed vs predicted values- one set of five-fold CV models
# put test predictions from all five folds into one df
p136_df <- bind_rows(lapply(fits_aruwiwr_brt$`136`,
                            FUN = function(x) {x$test_predictions}))

ggplot(data = p136_df, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals\nwith a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization") +
  ggtitle("model group 136")

p2_df <- bind_rows(lapply(fits_aruwiwr_brt$`2`,
                           FUN = function(x) {x$test_predictions}))

ggplot(data = p2_df, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals\nwith a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization")+
  ggtitle("model group 2")

p27_df <- bind_rows(lapply(fits_aruwiwr_brt$`27`,
                          FUN = function(x) {x$test_predictions}))

ggplot(data = p27_df, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals\nwith a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization")+
  ggtitle("model group 27")

p198_df <- bind_rows(lapply(fits_aruwiwr_brt$`198`,
                            FUN = function(x) {x$test_predictions}))

ggplot(data = p198_df, aes(x = OOB_preds, y = resp)) +
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

ggplot(data = wiwr_aru10c_testpreds_brt, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlab("mean predicted 30-sec intervals\nwith a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization") +
  ggtitle("Winter Wren\nARU- 10 consecutive min; 1000 BRTs")

## WIWRARU10c BRT- observed vs predicted values- standardized preds
wiwr_aru10c_preds_brt <- left_join(wiwr_aru10c_preds_brt, sum_wiwr)

ggplot(data = wiwr_aru10c_preds_brt, aes(x = resp, y = mean_pred)) +
  geom_point() +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()+
  xlab("mean observed vocalizations\nper 30 second interval") +
  ylab("mean predicted vocalizations\nper 30 second interval") +
  ggtitle("WIWR ARU- 10 consecutive min\nBRT predicted vs. observed") +
  ylim(c(0, 2))

## end WIWR ARU (10 consec min) model plots-------------------------------------

## WIWR ARU (10 random min) model plots-----------------------------------------
fits_aruwiwr10r_brt <- readRDS("fits_aruwiwr10r_brt.rds")

rm(fits_aruwiwr10r_brt)
## end WIWR ARU (10 random min) model plots-------------------------------------

## WIWR ARU (22 rand min) model plots-------------------------------------------
fits_aruwiwr22r_brt <- readRDS("fits_aruwiwr22r_brt.rds")

rm(fits_aruwiwr22r_brt)
## end WIWR ARU (22 rand min) model plots---------------------------------------

## GCKI point count models------------------------------------------------------
## observed vs predicted values- one set of five-fold CV models
# read in predictions from model
fits_gcki_brt <- readRDS("fits_gcki_brt.rds")

# put test predictions from all five folds into one df
b200_df <- bind_rows(lapply(fits_gcki_brt$`200`,
                            FUN = function(x) {x$standardized_preds}))
ggplot(data = b200_df, aes(x = day_of_yr, y = predictions)) +
  geom_line() +
  geom_point(data = sum_gcki, aes(x = day_of_yr, y = resp)) +
  ggtitle("Golden-crowned Kinglet\nPoint counts (10 min)\nBRT ex. model 200") +
  xlab("Julian day of year") +
  ylab("Mean number of individuals per point count") +
  theme_bw()

## GCKIPointCounts over time- observed vs predicted values- one set of five-
## fold CV models
# put test predictions from all five folds into one df
p200_df <- bind_rows(lapply(fits_gcki_brt$`200`,
                          FUN = function(x) {x$test_predictions}))

ggplot(data = p200_df, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count") +
  ggtitle("model group 200")

p19_df <- bind_rows(lapply(fits_gcki_brt$`19`,
                          FUN = function(x) {x$test_predictions}))

ggplot(data = p19_df, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count")+
  ggtitle("model group 19")

p112_df <- bind_rows(lapply(fits_gcki_brt$`112`,
                          FUN = function(x) {x$test_predictions}))

ggplot(data = p112_df, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count")+
  ggtitle("model group 112")

p94_df <- bind_rows(lapply(fits_gcki_brt$`94`,
                          FUN = function(x) {x$test_predictions}))

ggplot(data = p94_df, aes(x = OOB_preds, y = resp)) +
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

ggplot(data = gcki_ptct_testpreds_brt, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count") +
  ggtitle("Golden-crowned Kinglets\nPoint counts (10 min); 1000 BRTs")


#### THIS is a plot of observed values vs STANDARDIZED predictions
## GCKIPointCounts over time- observed vs predicted values- mean of all models
gcki_ptct_preds_brt <- left_join(gcki_ptct_preds_brt, sum_gcki, by = "day_of_yr")

ggplot(data = gcki_ptct_preds_brt, aes(x = resp, y = mean_pred)) +
  geom_point() +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()+
  xlab("mean observed individuals per point count") +
  ylab("mean predicted individuals per point count") +
  ggtitle("GCKI point counts (10 min)\nBRT predicted vs. observed") +
  ylim(c(0, 2))
rm(fits_gcki_brt)

## end GCKI point count models--------------------------------------------------

## GCKI ARU (10 consec minutes) models------------------------------------------
# observed vs predicted values- one set of five-fold CV models
# put test predictions from all five folds into one df
fits_arugcki_brt <- readRDS("fits_arugcki_brt.rds")

p140_df <- bind_rows(lapply(fits_arugcki_brt$`140`,
                            FUN = function(x) {x$test_predictions}))

ggplot(data = p140_df, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals\nwith a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization") +
  ggtitle("model group 140")

p71_df <- bind_rows(lapply(fits_arugcki_brt$`71`,
                           FUN = function(x) {x$test_predictions}))

ggplot(data = p71_df, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals\nwith a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization")+
  ggtitle("model group 71")

p6_df <- bind_rows(lapply(fits_arugcki_brt$`6`,
                            FUN = function(x) {x$test_predictions}))

ggplot(data = p6_df, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals\nwith a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization")+
  ggtitle("model group 6")

p199_df <- bind_rows(lapply(fits_arugcki_brt$`199`,
                           FUN = function(x) {x$test_predictions}))

ggplot(data = p199_df, aes(x = OOB_preds, y = resp)) +
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

ggplot(data = gcki_aru10c_testpreds_brt, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlab("mean predicted 30-sec intervals\nwith a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization") +
  ggtitle("Golden-crowned Kinglets\nARU- 10 consecutive min; 1000 BRTs")

## GCKI ARU 10c BRT- observed vs predicted values
#### THIS is a plot of observed values vs STANDARDIZED predictions
gcki_aru10c_preds_brt <- left_join(gcki_aru10c_preds_brt, sum_gcki,
                                   by = "day_of_yr")

gcki_aru10c_preds_brt <- gcki_aru10c_preds_brt[complete.cases(gcki_aru10c_preds_brt), ]

ggplot(data = gcki_aru10c_preds_brt, aes(x = resp, y = mean_pred)) +
  geom_point() +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()+
  xlab("mean observed vocalizations\nper 30 second interval") +
  ylab("mean predicted vocalizations\nper 30 second interval") +
  ggtitle("GCKI ARU- 10 consecutive min\nBRT predicted vs. observed") +
  ylim(c(0, 1))

rm(fits_arugcki_brt)

## end GCKI ARU (10 consec min) model plots-------------------------------------

## GCKI ARU (10 random min) model plots-----------------------------------------
fits_arugcki10r_brt <- readRDS("fits_arugcki10r_brt.rds")

rm(fits_arugcki10r_brt)
## end GCKI ARU (10 random min) model plots-------------------------------------

## GCKI ARU (22 rand min) model plots-------------------------------------------
fits_arugcki22r_brt <- readRDS("fits_arugcki22r_brt.rds")

rm(fits_arugcki22r_brt)
## end GCKI ARU (22 rand min) model plots---------------------------------------

#### end BRT abundance summary plots----------------------------------------------


##GAM abundance summary plots --------------------------------------------------
## scatterplot of number of GCKI per day (point counts) over time, with fitted
## average model (BRT) as a line
fits_gcki_gam <- readRDS("fits_gcki_gam.rds")

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
  geom_point(data = sum_gcki, aes(x = day_of_yr, y = resp)) +
  ggtitle("Golden-crowned Kinglet\nPoint counts (10 min); GAM") +
  xlab("Julian day of year") +
  ylab("Mean number of individuals per point count") +
  theme_bw()

g77_df <- bind_rows(lapply(fits_gcki_gam$`200`,
                           FUN = function(x) {x$standardized_preds}))
ggplot(data = g77_df, aes(x = day_of_yr, y = predictions)) +
  geom_line() +
  geom_point(data = sum_gcki, aes(x = day_of_yr, y = resp)) +
  ggtitle("Golden-crowned Kinglet\nPoint counts (10 min)\nGAM ex. model 77") +
  xlab("Julian day of year") +
  ylab("Mean number of individuals per point count") +
  theme_bw()


## GCKIPointCounts over time- observed vs predicted values- one set of five-
## fold CV models-- GAM
# put test predictions from all five folds into one df
g4_df <- bind_rows(lapply(fits_gcki_gam$`4`,
                          FUN = function(x) {x$test_predictions}))

ggplot(data = g4_df, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count") +
  ggtitle("model group 4")

g18_df <- bind_rows(lapply(fits_gcki_gam$`18`,
                           FUN = function(x) {x$test_predictions}))

ggplot(data = g18_df, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count")+
  ggtitle("model group 18")

g112_df <- bind_rows(lapply(fits_gcki_gam$`112`,
                            FUN = function(x) {x$test_predictions}))

ggplot(data = g112_df, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count")+
  ggtitle("model group 112")

p94_df <- bind_rows(lapply(fits_wiwr_brt$`94`,
                           FUN = function(x) {x$test_predictions}))

ggplot(data = p94_df, aes(x = OOB_preds, y = resp)) +
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

ggplot(data = gcki_ptct_testpreds_gam, aes(x = OOB_preds, y = resp)) +
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
  geom_point(data = sum_arugcki, aes(x = day_of_yr, y = resp)) +
  ggtitle("Golden-crowned Kinglet\nARU - consecutive 10 min GAM") +
  xlab("Julian day of year") +
  ylab("Mean number of 30-second intervals\nwith a vocalization") +
  theme_bw()

## GCKIARU10c over time- observed vs predicted values- one set of five-
## fold CV models
# put test predictions from all five folds into one df
p140_df <- bind_rows(lapply(fits_arugcki_brt$`140`,
                            FUN = function(x) {x$test_predictions}))

ggplot(data = p140_df, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals\nwith a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization") +
  ggtitle("model group 140")

p71_df <- bind_rows(lapply(fits_arugcki_brt$`71`,
                           FUN = function(x) {x$test_predictions}))

ggplot(data = p71_df, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals\nwith a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization")+
  ggtitle("model group 71")

p6_df <- bind_rows(lapply(fits_arugcki_brt$`6`,
                          FUN = function(x) {x$test_predictions}))

ggplot(data = p6_df, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals\nwith a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization")+
  ggtitle("model group 6")

p199_df <- bind_rows(lapply(fits_arugcki_brt$`199`,
                            FUN = function(x) {x$test_predictions}))

ggplot(data = p199_df, aes(x = OOB_preds, y = resp)) +
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

ggplot(data = gcki_aru10c_testpreds_brt, aes(x = OOB_preds, y = resp)) +
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
  geom_point(data = sum_wiwr, aes(x = day_of_yr, y = resp)) +
  ggtitle("Winter Wren\nPoint counts (10 min); GAM") +
  xlab("Julian day of year") +
  ylab("Mean number of individuals per point count") +
  theme_bw()

## WIWRPointCounts over time- observed vs predicted values- one set of five-
## fold CV models-- GAM
# put test predictions from all five folds into one df
gw8_df <- bind_rows(lapply(fits_wiwr_gam$`8`,
                           FUN = function(x) {x$test_predictions}))

ggplot(data = gw8_df, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count") +
  ggtitle("model group 8")

gw37_df <- bind_rows(lapply(fits_wiwr_gam$`37`,
                            FUN = function(x) {x$test_predictions}))

ggplot(data = gw37_df, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count")+
  ggtitle("model group 37")

gw103_df <- bind_rows(lapply(fits_wiwr_gam$`103`,
                             FUN = function(x) {x$test_predictions}))

ggplot(data = gw103_df, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted individuals per count") +
  ylab("mean observed individuals per count")+
  ggtitle("model group 103")

gw96_df <- bind_rows(lapply(fits_wiwr_gam$`96`,
                            FUN = function(x) {x$test_predictions}))

ggplot(data = gw96_df, aes(x = OOB_preds, y = resp)) +
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

ggplot(data = wiwr_ptct_testpreds_gam, aes(x = OOB_preds, y = resp)) +
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
  geom_point(data = sum_aruwiwr, aes(x = day_of_yr, y = resp)) +
  ggtitle("Winter Wren\nARU- 10 consecutive min; GAM") +
  xlab("Julian day of year") +
  ylab("Mean number of 30-sec intervals\nwith a vocalization") +
  theme_bw()

## WIWR ARU 10 consec.- observed vs predicted values- one set of five-
## fold CV models-- GAM
# put test predictions from all five folds into one df
gw4_df <- bind_rows(lapply(fits_aruwiwr_gam$`4`,
                           FUN = function(x) {x$test_predictions}))

ggplot(data = gw4_df, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals with a vocalization") +
  ylab("mean observed 30-sec intervals with a vocalization") +
  ggtitle("model group 4")

gw18_df <- bind_rows(lapply(fits_aruwiwr_gam$`18`,
                            FUN = function(x) {x$test_predictions}))

ggplot(data = gw18_df, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals with a vocalization") +
  ylab("mean observed 30-sec intervals with a vocalization")+
  ggtitle("model group 18")

gw112_df <- bind_rows(lapply(fits_aruwiwr_gam$`112`,
                             FUN = function(x) {x$test_predictions}))

ggplot(data = gw112_df, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  xlab("mean predicted 30-sec intervals with a vocalization") +
  ylab("mean observed 30-sec intervals with a vocalization")+
  ggtitle("model group 112")

pw94_df <- bind_rows(lapply(fits_aruwiwr_gam$`94`,
                            FUN = function(x) {x$test_predictions}))

ggplot(data = pw94_df, aes(x = OOB_preds, y = resp)) +
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

ggplot(data = wiwr_aru10c_testpreds_gam, aes(x = OOB_preds, y = resp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlab("mean predicted 30-sec intervals\n with a vocalization") +
  ylab("mean observed 30-sec intervals\nwith a vocalization") +
  ggtitle("Winter Wren\nARU- 10 consecutive min; 1000 GAMs")

## end GAM abundance summary plots----------------------------------------------

## BRT plots showing out of bag test error for example model iterations --------
ex_brts_gcki <- readRDS("example_fitted_brt_gcki.rds")

ntree_err_df_gcki <- bind_rows(mapply(FUN = function(x, fold) {
  data.frame(ntrees = 1:length(x$mod$cv.error), 
             err = x$mod$cv.error, 
             fold = fold)
}, ex_brts_gcki, 1:length(ex_brts_gcki), SIMPLIFY = FALSE))

gp <- ggplot(data = ntree_err_df_gcki, aes(x = ntrees, y = err, 
                                           color = factor(fold))) + 
  geom_line() + 
  ylab("Absolute error (test data)") + 
  xlab("Number of trees") + 
  scale_color_viridis_d(name = "CV fold") + 
  theme_bw() + 
  geom_vline(xintercept = 2000) + 
  ggtitle(expression(A[p]))

rm(ex_brts_gcki)

## arugcki
ex_brts_arugcki <- readRDS("example_fitted_brt_arugcki.rds")

ntree_err_df_arugcki <- bind_rows(mapply(FUN = function(x, fold) {
  data.frame(ntrees = 1:length(x$mod$cv.error), 
             err = x$mod$cv.error, 
             fold = fold)
}, ex_brts_arugcki, 1:length(ex_brts_arugcki), SIMPLIFY = FALSE))

g30c <- ggplot(data = ntree_err_df_arugcki, aes(x = ntrees, y = err, 
                                                color = factor(fold))) + 
  geom_line() + 
  ylab("Absolute error (test data)") + 
  xlab("Number of trees") + 
  scale_color_viridis_d(name = "CV fold") + 
  theme_bw() + 
  geom_vline(xintercept = 3000)+ 
  ggtitle(expression(A[30*C]))

rm(ex_brts_arugcki)

## arugcki10r
ex_brts_arugcki10r <- readRDS("example_fitted_brt_arugcki10r.rds")

ntree_err_df_arugcki10r <- bind_rows(mapply(FUN = function(x, fold) {
  data.frame(ntrees = 1:length(x$mod$cv.error), 
             err = x$mod$cv.error, 
             fold = fold)
}, ex_brts_arugcki10r, 1:length(ex_brts_arugcki10r), SIMPLIFY = FALSE))

g30r <- ggplot(data = ntree_err_df_arugcki10r, aes(x = ntrees, y = err, 
                                           color = factor(fold))) + 
  geom_line() + 
  ylab("Absolute error (test data)") + 
  xlab("Number of trees") + 
  scale_color_viridis_d(name = "CV fold") + 
  theme_bw() + 
  geom_vline(xintercept = 3000)+ 
  ggtitle(expression(A[30*R]))

rm(ex_brts_arugcki10r)

##aru22rgcki
ex_brts_arugcki22r <- readRDS("example_fitted_brt_arugcki22r.rds")

ntree_err_df_arugcki22r <- bind_rows(mapply(FUN = function(x, fold) {
  data.frame(ntrees = 1:length(x$mod$cv.error), 
             err = x$mod$cv.error, 
             fold = fold)
}, ex_brts_arugcki22r, 1:length(ex_brts_arugcki22r), SIMPLIFY = FALSE))

g66r <- ggplot(data = ntree_err_df_arugcki22r, aes(x = ntrees, y = err,
                                                   color = factor(fold))) + 
  geom_line() + 
  ylab("Absolute error (test data)") + 
  xlab("Number of trees") + 
  scale_color_viridis_d(name = "CV fold") + 
  theme_bw() + 
  geom_vline(xintercept = 2000)+ 
  ggtitle(expression(A[66*R]))

rm(ex_brts_arugcki22r)

##wiwr
ex_brts_wiwr <- readRDS("example_fitted_brt_wiwr.rds")

ntree_err_df_wiwr <- bind_rows(mapply(FUN = function(x, fold) {
  data.frame(ntrees = 1:length(x$mod$cv.error), 
             err = x$mod$cv.error, 
             fold = fold)
}, ex_brts_wiwr, 1:length(ex_brts_wiwr), SIMPLIFY = FALSE))

wp <- ggplot(data = ntree_err_df_wiwr, aes(x = ntrees, y = err, 
                                     color = factor(fold))) + 
  geom_line() + 
  ylab("Absolute error (test data)") + 
  xlab("Number of trees") + 
  scale_color_viridis_d(name = "CV fold") + 
  theme_bw() + 
  geom_vline(xintercept = 3000)+ 
  ggtitle(expression(A[p]))

rm(ex_brts_wiwr)

##aruwiwr
ex_brts_aruwiwr <- readRDS("example_fitted_brt_aruwiwr.rds")

ntree_err_df_aruwiwr <- bind_rows(mapply(FUN = function(x, fold) {
  data.frame(ntrees = 1:length(x$mod$cv.error), 
             err = x$mod$cv.error, 
             fold = fold)
}, ex_brts_aruwiwr, 1:length(ex_brts_aruwiwr), SIMPLIFY = FALSE))

w30c <- ggplot(data = ntree_err_df_aruwiwr, aes(x = ntrees, y = err, 
                                        color = factor(fold))) + 
  geom_line() + 
  ylab("Absolute error (test data)") + 
  xlab("Number of trees") + 
  scale_color_viridis_d(name = "CV fold") + 
  theme_bw() + 
  geom_vline(xintercept = 3000)+ 
  ggtitle(expression(A[30*C]))

rm(ex_brts_aruwiwr)

##aruwiwr10r
ex_brts_aruwiwr10r <- readRDS("example_fitted_brt_aruwiwr10r.rds")

ntree_err_df_aruwiwr10r <- bind_rows(mapply(FUN = function(x, fold) {
  data.frame(ntrees = 1:length(x$mod$cv.error), 
             err = x$mod$cv.error, 
             fold = fold)
}, ex_brts_aruwiwr10r, 1:length(ex_brts_aruwiwr10r), SIMPLIFY = FALSE))

w30r <- ggplot(data = ntree_err_df_aruwiwr10r, aes(x = ntrees, y = err, 
                                           color = factor(fold))) + 
  geom_line() + 
  ylab("Absolute error (test data)") + 
  xlab("Number of trees") + 
  scale_color_viridis_d(name = "CV fold") + 
  theme_bw() + 
  geom_vline(xintercept = 3000)+ 
  ggtitle(expression(A[30*R]))

rm(ex_brts_aruwiwr10r)

##aruwiwr22r
ex_brts_aruwiwr22r <- readRDS("example_fitted_brt_aruwiwr22r.rds")

ntree_err_df_aruwiwr22r <- bind_rows(mapply(FUN = function(x, fold) {
  data.frame(ntrees = 1:length(x$mod$cv.error), 
             err = x$mod$cv.error, 
             fold = fold)
}, ex_brts_aruwiwr22r, 1:length(ex_brts_aruwiwr22r), SIMPLIFY = FALSE))

w66r <- ggplot(data = ntree_err_df_aruwiwr22r, aes(x = ntrees, y = err, 
                                                   color = factor(fold))) + 
  geom_line() + 
  ylab("Absolute error (test data)") + 
  xlab("Number of trees") + 
  scale_color_viridis_d(name = "CV fold") + 
  theme_bw() + 
  geom_vline(xintercept = 3000) + 
  ggtitle(expression(A[66*R]))

rm(ex_brts_aruwiwr22r)

brt_testerr_wiwr <- wp + w30c + w30r + w66r +
  plot_layout(ncol = 2, guides = 'collect') +
  plot_annotation(title = 'Winter Wren', 
                  tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = 10))

brt_testerr_gcki <- gp + g30c + g30r + g66r + 
  plot_layout(ncol = 2, guides = 'collect') +
  plot_annotation(title = 'Golden-crowned Kinglet', 
                  tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = 10))


## write out final plots for supp materials!------------------------------------
ggsave(brt_testerr_wiwr, filename = "./saved_objects/brt_testerr_wiwr.jpg", 
       width = 16, height = 15, 
       units = "cm", device = "jpeg")

ggsave(brt_testerr_gcki, filename = "./saved_objects/brt_testerr_gcki.jpg", 
       width = 16, height = 15, 
       units = "cm", device = "jpeg")

## end write out plots to file--------------------------------------------------
