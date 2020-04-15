################################
## Abundance estimation: Generalized Additive Model
## Point Abbaye 2019 data
## 
## author: Ellie Roark, Willson Gaul
## created: 6 Mar 2020
## last modified: 14 Apr 2020
## 
## inputs: *MUST FIRST RUN: DataPrep_PtCtAbundance.R- script reads in original
##          point count data and prepares dataframes with the number of observed 
##          GCKI and WIWR per 10 minute point count and per day
##         *AND BRT_abundance.R- script that fits GLM and BRT for GCKI and WIWR
##          data for both point counts and ARU counts
##           
## outputs: *
##            
## TODO: * 
################################


## GAM with GCKI per day (ptct)-------------------------------------------------
k <- 12 # k should be large enough that EDF is a good bit less than k-1.  

# GAM fit by wg using k (see above) knots for smoothing
# uses cubic regression spline as smoothing basis
fit_gam <- function(test_fold, sp_data) {
  train_dat <- sp_data[sp_data$fold != test_fold, ]
  f_m <- gam(count ~ 1 + s(wind, k = k) + s(day_of_yr_c, k = k, bs = "cr"), 
             data = sp_data, 
             family = "nb", select = TRUE)
  test_pred <- sp_data[sp_data$fold == test_fold, ]
  test_pred$OOB_preds <- predict(f_m, newdata = test_pred, 
                                 type = "response")
  list(mod = f_m, test_predictions = test_pred)
}

## test GAM fitting with all data and cubic regression spline smooth
# for interaction discussion, see Section 5.6.3 and p 344 of Wood
#  + ti(wind, day_of_yr_c, k = k)
gcki.day.gam <- gam(count ~ 1 + s(wind, k = k) + s(day_of_yr_c, k = k, 
                                                   bs = "cr"), 
                    data = sum_gcki, 
                    family = "nb", select = TRUE)

## test GAM fitting with all data and thin plate spline smooth
gcki.day.gam2 <- gam(count ~ 1 + s(wind, bs = "tp") + s(day_of_yr_c, , k= k, 
                                                        bs = "tp"), 
            data = sum_gcki, 
            family = "nb", select = TRUE)




# make a list to hold fitted models and predictions from models with many 
# replicate folds splits
# At the end of the following for loop, fits_gcki_gam should have results from 
# 200 different 5-fold CV splits (so 1000 different models fit, 5 for each fold
# in each of 200 different fold splits)
fits_gcki_gam <- list()

for (i in 1:200) {
  # assign days to 3-day blocks
  days <- data.frame(day = min(sum_gcki$day_of_yr):max(sum_gcki$day_of_yr), 
                     block = NA)
  n_blocks <- nrow(days)/3
  blocks <- rep(1:n_blocks, 3)
  blocks <- blocks[order(blocks)]
  start_row <- sample(1:nrow(days), size = 1)
  days$block[start_row:nrow(days)] <- blocks[1:length(start_row:nrow(days))]
  if(start_row != 1) {
    days$block[1:(start_row - 1)] <- blocks[(length(start_row:nrow(days)) + 1):
                                              nrow(days)]
  }
  
  # assign blocks to CV folds
  fold_assignments <- data.frame(
    block = unique(days$block), 
    fold = sample(rep_len(1:5, length.out = length(unique(days$block)))))
  days <- left_join(days, fold_assignments, by = "block")
  rm(blocks, n_blocks, start_row, fold_assignments)
  
  # join CV fold info onto bird data
  bird_dat <- left_join(sum_gcki, days, by = c("day_of_yr" = "day"))
  
  # fit brt to data in CV folds
  brt_test_folds <- unique(bird_dat$fold)
  names(brt_test_folds) <- as.character(brt_test_folds) 
  
  brt_test_folds <- lapply(brt_test_folds, fit_gam, sp_data = bird_dat)
  
  # put predictions for these 5 folds into the big list for all splits
  fits_gcki_gam[[i]] <- brt_test_folds
}
names(fits_gcki_gam) <- 1:length(fits_gcki_gam)






# GAM fit using thin plate splines as smoother
gam2_test_folds <- unique(sum_gcki$fold)
names(gam2_test_folds) <- as.character(gam2_test_folds)

fit_gam2 <- function(test_fold2, sp_data2) {
  train_dat2 <- sp_data2[sp_data2$fold != test_fold2, ]
  f_m2 <- gam(count ~ 1 + s(wind, bs = "tp") + s(day_of_yr_c, bs = "tp"), 
             data = sp_data2, 
             family = "nb", select = TRUE)
  test_pred2 <- sp_data2[sp_data2$fold == test_fold2, ]
  test_pred2$OOB_preds <- predict(f_m2, newdata = test_pred2, 
                                 type = "response")
  list(mod = f_m2, test_predictions2 = test_pred2)
}

gam2_test_folds <- lapply(gam2_test_folds, fit_gam2, sp_data2 = sum_gcki)


## end GAM with GCKI per day----------------------------------------------------

## evaluate GAM (GCKI per day model)--------------------------------------------
gcki.day.gam
gam.check(gcki.day.gam)
plot(gcki.day.gam, pages = 1, all.terms = T)

gcki.day.gam2
gam.check(gcki.day.gam2)
plot(gcki.day.gam2, pages = 1, all.terms = T)

# get predictions to test data-- GAM with specified k
gcki_gam_predictions <- bind_rows(lapply(gam_test_folds, 
                                         FUN = function(x) x$test_predictions))
gcki_gam_predictions$error = gcki_gam_predictions$OOB_preds - 
  gcki_gam_predictions$count

# calculate r^2 (square of Pearson correlation coefficient, see Bahn & 
# McGill 2013)
gcki_gam_r2 <- cor(gcki_gam_predictions$day_of_yr, 
                   gcki_gam_predictions$OOB_preds, 
                   method = "pearson")^2
# calculate R^2 (coefficient of determination, see Bahn & McGill 2013)
gcki_gam_R2 <- 1 - (sum(gcki_gam_predictions$error^2) / 
                      (sum((gcki_gam_predictions$count - 
                              mean(gcki_gam_predictions$count))^2)))



# get predictions to test data-- GAM with thin plate spline- cross validated
gcki_gam2_predictions <- bind_rows(lapply(gam2_test_folds, 
                                         FUN = function(x) x$test_predictions2))
gcki_gam2_predictions$error = gcki_gam2_predictions$OOB_preds - 
  gcki_gam2_predictions$count

# calculate r^2 (square of Pearson correlation coefficient, see Bahn & 
# McGill 2013)
gcki_gam2_r2 <- cor(gcki_gam2_predictions$day_of_yr, 
                   gcki_gam2_predictions$OOB_preds, 
                   method = "pearson")^2
# calculate R^2 (coefficient of determination, see Bahn & McGill 2013)
gcki_gam2_R2 <- 1 - (sum(gcki_gam2_predictions$error^2) / 
                      (sum((gcki_gam2_predictions$count - 
                              mean(gcki_gam2_predictions$count))^2)))
## end evaluate GAM (GCKI per day model-- ptct)----------------------------------------


## GAM with GCKI per day (ARU)-------------------------------------------------
k = 15

## test GAM fitting with all data
# for interaction discussion, see Section 5.6.3 and p 344 of Wood
#  + ti(wind, day_of_yr_c, k = k)
arugcki.day.gam <- gam(count ~ 1 + s(wind, k = k) + s(day_of_yr_c, k = k), 
                    data = sum_arugcki, 
                    family = "nb", select = TRUE)

## test GAM fitting with all data and thin plate spline smooth
arugcki.day.gam2 <- gam(count ~ 1 + s(wind, bs = "tp") + s(day_of_yr_c, bs = "tp"), 
                     data = sum_arugcki, 
                     family = "nb", select = TRUE)

# fit gam to data in CV folds
arugam_test_folds <- unique(sum_arugcki$fold)
names(arugam_test_folds) <- as.character(arugam_test_folds)

# GAM fit by wg using k (see above) knots for smoothing
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

arugam_test_folds <- lapply(arugam_test_folds, fit_gam, sp_data = sum_arugcki)


# GAM fit using thin plate splines as smoother
arugam2_test_folds <- unique(sum_arugcki$fold)
names(arugam2_test_folds) <- as.character(arugam2_test_folds)

fit_gam2 <- function(test_fold2, sp_data2) {
  train_dat2 <- sp_data2[sp_data2$fold != test_fold2, ]
  f_m2 <- gam(count ~ 1 + s(wind, bs = "tp") + s(day_of_yr_c, bs = "tp"), 
              data = sp_data2, 
              family = "nb", select = TRUE)
  test_pred2 <- sp_data2[sp_data2$fold == test_fold2, ]
  test_pred2$OOB_preds <- predict(f_m2, newdata = test_pred2, 
                                  type = "response")
  list(mod = f_m2, test_predictions2 = test_pred2)
}

arugam2_test_folds <- lapply(arugam2_test_folds, fit_gam2, 
                             sp_data2 = sum_arugcki)


## end GAM with GCKI per day (ARU)----------------------------------------------

## evaluate GAM (GCKI per day model)- ARU---------------------------------------
arugcki.day.gam
gam.check(arugcki.day.gam)
plot(gcki.day.gam, pages = 1, all.terms = T)

arugcki.day.gam2
gam.check(arugcki.day.gam2)
plot(arugcki.day.gam2, pages = 1, all.terms = T)
## TODO: something funky happening here....

# get predictions to test data-- GAM with specified k
arugcki_gam_predictions <- bind_rows(lapply(arugam_test_folds, 
                                         FUN = function(x) x$test_predictions))
arugcki_gam_predictions$error = arugcki_gam_predictions$OOB_preds - 
  arugcki_gam_predictions$count

# calculate r^2 (square of Pearson correlation coefficient, see Bahn & 
# McGill 2013)
arugcki_gam_r2 <- cor(arugcki_gam_predictions$day_of_yr, 
                   arugcki_gam_predictions$OOB_preds, 
                   method = "pearson")^2
# calculate R^2 (coefficient of determination, see Bahn & McGill 2013)
arugcki_gam_R2 <- 1 - (sum(arugcki_gam_predictions$error^2) / 
                      (sum((arugcki_gam_predictions$count - 
                              mean(arugcki_gam_predictions$count))^2)))



# get predictions to test data-- GAM with thin plate spline- cross validated
arugcki_gam2_predictions <- bind_rows(lapply(arugam2_test_folds, 
                                          FUN = function(x) x$test_predictions2))
arugcki_gam2_predictions$error = arugcki_gam2_predictions$OOB_preds - 
  arugcki_gam2_predictions$count

# calculate r^2 (square of Pearson correlation coefficient, see Bahn & 
# McGill 2013)
arugcki_gam2_r2 <- cor(arugcki_gam2_predictions$day_of_yr, 
                    arugcki_gam2_predictions$OOB_preds, 
                    method = "pearson")^2
# calculate R^2 (coefficient of determination, see Bahn & McGill 2013)
arugcki_gam2_R2 <- 1 - (sum(arugcki_gam2_predictions$error^2) / 
                       (sum((arugcki_gam2_predictions$count - 
                               mean(arugcki_gam2_predictions$count))^2)))
## end evaluate GAM (GCKI per day model)- ARU-----------------------------------

################################################################################
################################################################################

## GAM with WIWR per day (ptct)-------------------------------------------------
k <- 12 # k should be large enough that EDF is a good bit less than k-1.  

## test GAM fitting with all data
# for interaction discussion, see Section 5.6.3 and p 344 of Wood
#  + ti(wind, day_of_yr_c, k = k)
wiwr.day.gam <- gam(count ~ 1 + s(wind, k = k) + s(day_of_yr_c, k = k), 
                    data = sum_wiwr, 
                    family = "poisson", select = TRUE)

## test GAM fitting with all data and thin plate spline smooth
wiwr.day.gam2 <- gam(count ~ 1 + s(wind, bs = "tp") + s(day_of_yr_c, bs = "tp"), 
                     data = sum_wiwr, 
                     family = "poisson", select = TRUE)

# fit gam to data in CV folds
gam_test_folds <- unique(sum_wiwr$fold)
names(gam_test_folds) <- as.character(gam_test_folds)

# GAM fit by wg using k (see above) knots for smoothing
fit_wgam <- function(test_fold, sp_data) {
  wtrain_dat <- sp_data[sp_data$fold != test_fold, ]
  f_mw <- gam(count ~ 1 + s(wind, k = k) + s(day_of_yr_c, k = k), 
             data = sp_data, 
             family = "poisson", select = TRUE)
  wtest_pred <- sp_data[sp_data$fold == test_fold, ]
  wtest_pred$OOB_preds <- predict(f_mw, newdata = wtest_pred, 
                                 type = "response")
  list(mod = f_mw, test_predictions = wtest_pred)
}

gam_test_folds <- lapply(gam_test_folds, fit_wgam, sp_data = sum_wiwr)


# GAM fit using thin plate splines as smoother
gam2_test_folds <- unique(sum_wiwr$fold)
names(gam2_test_folds) <- as.character(gam2_test_folds)

fit_wgam2 <- function(test_fold2, sp_data2) {
  wtrain_dat2 <- sp_data2[sp_data2$fold != test_fold2, ]
  f_mw2 <- gam(count ~ 1 + s(wind, bs = "tp") + s(day_of_yr_c, bs = "tp"), 
              data = sp_data2, 
              family = "nb", select = TRUE)
  wtest_pred2 <- sp_data2[sp_data2$fold == test_fold2, ]
  wtest_pred2$OOB_preds <- predict(f_mw2, newdata = wtest_pred2, 
                                  type = "response")
  list(mod = f_mw2, test_predictions2 = wtest_pred2)
}

gam2_test_folds <- lapply(gam2_test_folds, fit_wgam2, sp_data2 = sum_wiwr)


## end GAM with WIWR per day----------------------------------------------------

## evaluate GAM (WIWR per day model)--------------------------------------------
wiwr.day.gam
gam.check(wiwr.day.gam)
plot(wiwr.day.gam, pages = 1, all.terms = T)

wiwr.day.gam2
gam.check(wiwr.day.gam2)
plot(wiwr.day.gam2, pages = 1, all.terms = T)

# get predictions to test data-- GAM with specified k
wiwr_gam_predictions <- bind_rows(lapply(gam_test_folds, 
                                         FUN = function(x) x$test_predictions))
wiwr_gam_predictions$error = wiwr_gam_predictions$OOB_preds - 
  wiwr_gam_predictions$count

# calculate r^2 (square of Pearson correlation coefficient, see Bahn & 
# McGill 2013)
wiwr_gam_r2 <- cor(wiwr_gam_predictions$day_of_yr, 
                   wiwr_gam_predictions$OOB_preds, 
                   method = "pearson")^2
# calculate R^2 (coefficient of determination, see Bahn & McGill 2013)
wiwr_gam_R2 <- 1 - (sum(wiwr_gam_predictions$error^2) / 
                      (sum((wiwr_gam_predictions$count - 
                              mean(wiwr_gam_predictions$count))^2)))



# get predictions to test data-- GAM with thin plate spline- cross validated
wiwr_gam2_predictions <- bind_rows(lapply(gam2_test_folds, 
                                          FUN = function(x) x$test_predictions2))
wiwr_gam2_predictions$error = wiwr_gam2_predictions$OOB_preds - 
  wiwr_gam2_predictions$count

# calculate r^2 (square of Pearson correlation coefficient, see Bahn & 
# McGill 2013)
wiwr_gam2_r2 <- cor(wiwr_gam2_predictions$day_of_yr, 
                    wiwr_gam2_predictions$OOB_preds, 
                    method = "pearson")^2
# calculate R^2 (coefficient of determination, see Bahn & McGill 2013)
wiwr_gam2_R2 <- 1 - (sum(wiwr_gam2_predictions$error^2) / 
                       (sum((wiwr_gam2_predictions$count - 
                               mean(wiwr_gam2_predictions$count))^2)))
## end evaluate GAM (WIWR per day model-- ptct)---------------------------------


## TODO: GAM with WIWR per day (ARU)-------------------------------------------------
k = 15

## test GAM fitting with all data
# for interaction discussion, see Section 5.6.3 and p 344 of Wood
#  + ti(wind, day_of_yr_c, k = k)
aruwiwr.day.gam <- gam(count ~ 1 + s(wind, k = k) + s(day_of_yr_c, k = k), 
                       data = sum_aruwiwr, 
                       family = "nb", select = TRUE)

## test GAM fitting with all data and thin plate spline smooth
aruwiwr.day.gam2 <- gam(count ~ 1 + s(wind, bs = "tp") + s(day_of_yr_c, bs = "tp"), 
                        data = sum_aruwiwr, 
                        family = "nb", select = TRUE)

# fit gam to data in CV folds
warugam_test_folds <- unique(sum_aruwiwr$fold)
names(warugam_test_folds) <- as.character(warugam_test_folds)

# GAM fit by wg using k (see above) knots for smoothing
fit_gamwa <- function(test_fold, sp_data) {
  watrain_dat <- sp_data[sp_data$fold != test_fold, ]
  f_mwa <- gam(count ~ 1 + s(wind, k = k) + s(day_of_yr_c, k = k), 
             data = sp_data, 
             family = "nb", select = TRUE)
  watest_pred <- sp_data[sp_data$fold == test_fold, ]
  watest_pred$OOB_preds <- predict(f_mwa, newdata = watest_pred, 
                                 type = "response")
  list(mod = f_mwa, test_predictions = watest_pred)
}

warugam_test_folds <- lapply(warugam_test_folds, fit_gamwa, 
                             sp_data = sum_aruwiwr)


# GAM fit using thin plate splines as smoother
warugam2_test_folds <- unique(sum_aruwiwr$fold)
names(warugam2_test_folds) <- as.character(warugam2_test_folds)

fit_gamwa2 <- function(test_fold2, sp_data2) {
  train_datwa2 <- sp_data2[sp_data2$fold != test_fold2, ]
  f_mwa2 <- gam(count ~ 1 + s(wind, bs = "tp") + s(day_of_yr_c, bs = "tp"), 
              data = sp_data2, 
              family = "nb", select = TRUE)
  watest_pred2 <- sp_data2[sp_data2$fold == test_fold2, ]
  watest_pred2$OOB_preds <- predict(f_mwa2, newdata = watest_pred2, 
                                  type = "response")
  list(mod = f_mwa2, watest_predictions2 = watest_pred2)
}

warugam2_test_folds <- lapply(warugam2_test_folds, fit_gamwa2, 
                             sp_data2 = sum_aruwiwr)


## end GAM with WIWR per day (ARU)----------------------------------------------

## evaluate GAM (WIWR per day model)- ARU---------------------------------------
aruwiwr.day.gam
gam.check(aruwiwr.day.gam)
plot(aruwiwr.day.gam, pages = 1, all.terms = T)

aruwiwr.day.gam2
gam.check(arugcki.day.gam2)
plot(aruwiwr.day.gam2, pages = 1, all.terms = T)
## TODO: something funky happening here....

# get predictions to test data-- GAM with specified k
aruwiwr_gam_predictions <- bind_rows(lapply(warugam_test_folds, 
                                            FUN = function(x) x$test_predictions))
aruwiwr_gam_predictions$error = aruwiwr_gam_predictions$OOB_preds - 
  aruwiwr_gam_predictions$count

# calculate r^2 (square of Pearson correlation coefficient, see Bahn & 
# McGill 2013)
aruwiwr_gam_r2 <- cor(aruwiwr_gam_predictions$day_of_yr, 
                      aruwiwr_gam_predictions$OOB_preds, 
                      method = "pearson")^2
# calculate R^2 (coefficient of determination, see Bahn & McGill 2013)
aruwiwr_gam_R2 <- 1 - (sum(aruwiwr_gam_predictions$error^2) / 
                         (sum((aruwiwr_gam_predictions$count - 
                                 mean(aruwiwr_gam_predictions$count))^2)))



# get predictions to test data-- GAM with thin plate spline- cross validated
aruwiwr_gam2_predictions <- bind_rows(lapply(warugam2_test_folds, 
                                             FUN = function(x) x$watest_predictions2))
aruwiwr_gam2_predictions$error = aruwiwr_gam2_predictions$OOB_preds - 
  aruwiwr_gam2_predictions$count

# calculate r^2 (square of Pearson correlation coefficient, see Bahn & 
# McGill 2013)
aruwiwr_gam2_r2 <- cor(aruwiwr_gam2_predictions$day_of_yr, 
                       aruwiwr_gam2_predictions$OOB_preds, 
                       method = "pearson")^2
# calculate R^2 (coefficient of determination, see Bahn & McGill 2013)
aruwiwr_gam2_R2 <- 1 - (sum(aruwiwr_gam2_predictions$error^2) / 
                          (sum((aruwiwr_gam2_predictions$count - 
                                  mean(aruwiwr_gam2_predictions$count))^2)))
## end evaluate GAM (WIWR per day model)- ARU-----------------------------------












## join brt and gam predictions and errors -------------------------------------
gcki_gam_predictions$method = "pt_gam1"
gcki_brt_predictions$method = "pt_brt"
gcki_gam2_predictions$method = "pt_gam2"
arugcki_brt_predictions$method = "aru_brt"
arugcki_gam_predictions$method = "aru_gam1"
arugcki_gam2_predictions$method = "aru_gam2"

gcki_predictions <- bind_rows(gcki_gam_predictions, gcki_brt_predictions, 
                              gcki_gam2_predictions, arugcki_brt_predictions, 
                              arugcki_gam_predictions, arugcki_gam2_predictions)
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
plot(gcki_gam2_predictions$OOB_preds ~ gcki_gam_predictions$OOB_preds)
plot(gcki_gam_predictions$error ~ gcki_brt_predictions$error)
plot(gcki_gam_predictions$error ~ gcki_gam2_predictions$error)
plot(gcki_brt_predictions$OOB_preds ~ arugcki_brt_predictions$OOB_preds)
plot(gcki_gam_predictions$OOB_preds ~ arugcki_gam_predictions$OOB_preds)
## end brt and gam predictions plot --------------------------------------------
