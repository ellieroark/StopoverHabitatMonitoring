################################
## Abundance estimation: Generalized Additive Model
## Point Abbaye 2019 data
## 
## author: Ellie Roark, Willson Gaul
## created: 6 Mar 2020
## last modified: 20 Mar 2020
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

## test GAM fitting with all data
# for interaction discussion, see Section 5.6.3 and p 344 of Wood
#  + ti(wind, day_of_yr_c, k = k)
gcki.day.gam <- gam(count ~ 1 + s(wind, k = k) + s(day_of_yr_c, k = k), 
                    data = sum_gcki, 
                    family = "nb", select = TRUE)

## test GAM fitting with all data and thin plate spline smooth
gcki.day.gam2 <- gam(count ~ 1 + s(wind, bs = "tp") + s(day_of_yr_c, bs = "tp"), 
            data = sum_gcki, 
            family = "nb", select = TRUE)

# fit gam to data in CV folds
gam_test_folds <- unique(sum_gcki$fold)
names(gam_test_folds) <- as.character(gam_test_folds)

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

gam_test_folds <- lapply(gam_test_folds, fit_gam, sp_data = sum_gcki)


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
## end evaluate GAM (GCKI per day model)----------------------------------------


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
