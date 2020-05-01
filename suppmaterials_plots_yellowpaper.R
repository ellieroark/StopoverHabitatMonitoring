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

#### Boosted Regression Tree summary plots----------------------------------------
## WIWR point count models------------------------------------------------------ 
fits_wiwr_brt <- readRDS("fits_wiwr_brt.rds")

## observed vs predicted values- one set of five-fold CV models
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

## WIWRPointCounts over time- observed vs predicted values, standardized preds
wiwr_ptct_preds_brt <- left_join(wiwr_ptct_preds_brt, sum_wiwr, by = "day_of_yr")

ggplot(data = wiwr_ptct_preds_brt, aes(x = meandet, y = mean_pred)) +
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

## WIWRARU10c BRT- observed vs predicted values- standardized preds
wiwr_aru10c_preds_brt <- left_join(wiwr_aru10c_preds_brt, sum_wiwr,
                                   by = "day_of_yr")

ggplot(data = wiwr_aru10c_preds_brt, aes(x = meandet, y = mean_pred)) +
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


#### THIS is a plot of observed values vs STANDARDIZED predictions
## GCKIPointCounts over time- observed vs predicted values- mean of all models
gcki_ptct_preds_brt <- left_join(gcki_ptct_preds_brt, sum_gcki, by = "day_of_yr")

ggplot(data = gcki_ptct_preds_brt, aes(x = meandet, y = mean_pred)) +
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

## GCKI ARU 10c BRT- observed vs predicted values
#### THIS is a plot of observed values vs STANDARDIZED predictions
gcki_aru10c_preds_brt <- left_join(gcki_aru10c_preds_brt, sum_gcki,
                                   by = "day_of_yr")

gcki_aru10c_preds_brt <- gcki_aru10c_preds_brt[complete.cases(gcki_aru10c_preds_brt), ]

ggplot(data = gcki_aru10c_preds_brt, aes(x = meandet, y = mean_pred)) +
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
#fits_gcki_gam <- readRDS("fits_gcki_gam.rds")

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

## end GAM abundance summary plots----------------------------------------------








