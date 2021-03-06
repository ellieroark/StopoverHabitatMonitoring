################################
## Abundance estimation: Generalized Additive Model
## Point Abbaye 2019 data
## 
## author: Ellie Roark, Willson Gaul
## created: 6 Mar 2020
## last modified: 28 May 2020
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

fitgam <- FALSE

if(fitgam){

## GAM with GCKI per day (ptct)-------------------------------------------------
k <- -1 # k should be large enough that EDF is a good bit less than k-1.  

# GAM fit by wg using k (see above) knots for smoothing
# uses cubic regression spline as smoothing basis
fit_gam <- function(test_fold, sp_data, newdata) {
  train_dat <- sp_data[sp_data$fold != test_fold, ]
  f_m <- gam(resp ~ 1 + s(windknots, k = k) + s(day_of_yr_c, k = k, bs = "cr"), 
             data = train_dat, 
             family = "nb", select = TRUE)
  test_pred <- sp_data[sp_data$fold == test_fold, ]
  test_pred$OOB_preds <- predict(f_m, newdata = test_pred, 
                                 type = "response")
  test_pred$error <- test_pred$OOB_preds - test_pred$resp
  test_pred$OOB_logpreds <- predict(f_m, newdata = test_pred, 
                                     type = "link")
  #test_pred$logerror <- test_pred$OOB_logpreds - log(test_pred$resp)
  
  # Get standardized predictions to new data
  stand_pred <- newdata[newdata$fold == test_fold, ]
  stand_pred$predictions <- predict(f_m, newdata = stand_pred, 
                                   type = "response")
  
  # return predictions to the observed data from the test fold, 
  # and predictions to new data (with standardized covariates)
  list(test_predictions = test_pred, standardized_preds = stand_pred)
}

# ## test GAM fitting with all data and cubic regression spline smooth
# # for interaction discussion, see Section 5.6.3 and p 344 of Wood
# #  + ti(wind, day_of_yr_c, k = k)
# gcki.day.gam <- gam(resp ~ 1 + s(wind, k = k) + s(day_of_yr_c, k = k,
#                                                    bs = "cr"),
#                     data = sum_gcki,
#                     family = "nb", select = TRUE)
# 
# ## test GAM fitting with all data and thin plate spline smooth
# gcki.day.gam2 <- gam(resp ~ 1 + s(wind, bs = "tp") + s(day_of_yr_c, k= k,
#                                                         bs = "tp"),
#             data = sum_gcki,
#             family = "nb", select = TRUE)
# 
# 
# 

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
  
  # create new data to predict with
  pgcki_day <- data.frame(day_of_yr = seq(min(sum_gcki$day_of_yr), 
                                             max(sum_gcki$day_of_yr), by = 1))
  pgcki_day$day_of_yr_c <- pgcki_day$day_of_yr-mean(pgcki_day$day_of_yr)
  pgcki_day$windknots <- mean(sum_gcki$windknots)
  pgcki_day <- left_join(pgcki_day, days, by = c("day_of_yr" = "day"))
  
  # join CV fold info onto bird data
  bird_dat <- left_join(sum_gcki, days, by = c("day_of_yr" = "day"))
  
  # fit brt to data in CV folds
  gam_test_folds <- unique(bird_dat$fold)
  names(gam_test_folds) <- as.character(gam_test_folds) 
  
  gam_test_folds <- lapply(gam_test_folds, fit_gam, sp_data = bird_dat, 
                           newdata = pgcki_day)
  
  # put predictions for these 5 folds into the big list for all splits
  fits_gcki_gam[[i]] <- gam_test_folds
}
names(fits_gcki_gam) <- 1:length(fits_gcki_gam)
rmse_gcki_gam <- sapply(fits_gcki_gam, FUN = function(x) {
  sapply(x, FUN = function(z) {
    sqrt(mean(z$test_predictions$error^2))
  })
})
saveRDS(fits_gcki_gam, "fits_gcki_gam.rds")
saveRDS(rmse_gcki_gam, "rmse_gcki_gam.rds")
rm(fits_gcki_gam, rmse_gcki_gam)

# # GAM fit using thin plate splines as smoother
# gam2_test_folds <- unique(sum_gcki$fold)
# names(gam2_test_folds) <- as.character(gam2_test_folds)
# 
# fit_gam2 <- function(test_fold2, sp_data2) {
#   train_dat2 <- sp_data2[sp_data2$fold != test_fold2, ]
#   f_m2 <- gam(count ~ 1 + s(wind, bs = "tp") + s(day_of_yr_c, bs = "tp"), 
#              data = sp_data2, 
#              family = "nb", select = TRUE)
#   test_pred2 <- sp_data2[sp_data2$fold == test_fold2, ]
#   test_pred2$OOB_preds <- predict(f_m2, newdata = test_pred2, 
#                                  type = "response")
#   test_pred2$error <- test_pred2$OOB_preds - test_pred2$resp
#   # Get standardized predictions to new data
#   stand_pred2 <- newdata[newdata$fold == test_fold2, ]
#   stand_pred2$predictions <- predict(f_m2, newdata = stand_pred2, 
#                                     type = "response")
#   
#   # return predictions to the observed data from the test fold, 
#   # and predictions to new data (with standardized covariates)
#   list(mod = f_m2, test_predictions = test_pred2, 
#        standardized_preds = stand_pred2)
# }
# 
# gam2_test_folds <- lapply(gam2_test_folds, fit_gam2, sp_data2 = sum_gcki)
# 

## end GAM with GCKI per day----------------------------------------------------

## GAM with GCKI per day (ARU)-------------------------------------------------
k = -1

fits_arugcki_gam <- list()

for (i in 1:200) {
  # assign days to 3-day blocks
  days <- data.frame(day = min(sum_arugcki$day_of_yr):max(sum_arugcki$day_of_yr), 
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
  
  # create new data to predict with
  pgcki_day <- data.frame(day_of_yr = seq(min(sum_gcki$day_of_yr), 
                                          max(sum_gcki$day_of_yr), by = 1))
  pgcki_day$day_of_yr_c <- pgcki_day$day_of_yr-mean(pgcki_day$day_of_yr)
  pgcki_day$windknots <- mean(sum_gcki$windknots)
  pgcki_day <- left_join(pgcki_day, days, by = c("day_of_yr" = "day"))
  
  # join CV fold info onto bird data
  bird_dat <- left_join(sum_arugcki, days, by = c("day_of_yr" = "day"))
  
  # fit brt to data in CV folds
  gam_test_folds <- unique(bird_dat$fold)
  names(gam_test_folds) <- as.character(gam_test_folds) 
  
  gam_test_folds <- lapply(gam_test_folds, fit_gam, sp_data = bird_dat, 
                           newdata = pgcki_day)
  
  # put predictions for these 5 folds into the big list for all splits
  fits_arugcki_gam[[i]] <- gam_test_folds
}
names(fits_arugcki_gam) <- 1:length(fits_arugcki_gam)
rmse_arugcki_gam <- sapply(fits_arugcki_gam, FUN = function(x) {
  sapply(x, FUN = function(z) {
    sqrt(mean(z$test_predictions$error^2))
  })
})
saveRDS(fits_arugcki_gam, "fits_arugcki_gam.rds")
saveRDS(rmse_arugcki_gam, "rmse_arugcki_gam.rds")
rm(fits_gcki_gam, rmse_arugcki_gam)


# ## test GAM fitting with all data
# # for interaction discussion, see Section 5.6.3 and p 344 of Wood
# #  + ti(wind, day_of_yr_c, k = k)
arugcki.day.gam <- gam(count ~ 1 + s(windknots, bs = "cr", k = k) + 
                         s(day_of_yr_c, bs = "cr", k = k),
                    data = sum_arugcki,
                    family = "nb", select = TRUE)

## test GAM fitting with all data and thin plate spline smooth
arugcki.day.gam2 <- gam(count ~ 1 + s(windknots, bs = "tp") + s(day_of_yr_c, bs = "tp"),
                     data = sum_arugcki,
                     family = "nb", select = TRUE)
# 
# # fit gam to data in CV folds
# arugam_test_folds <- unique(sum_arugcki$fold)
# names(arugam_test_folds) <- as.character(arugam_test_folds)
# 
# # GAM fit by wg using k (see above) knots for smoothing
# fit_gam <- function(test_fold, sp_data) {
#   train_dat <- sp_data[sp_data$fold != test_fold, ]
#   f_m <- gam(count ~ 1 + s(wind, k = k) + s(day_of_yr_c, k = k), 
#              data = sp_data, 
#              family = "nb", select = TRUE)
#   test_pred <- sp_data[sp_data$fold == test_fold, ]
#   test_pred$OOB_preds <- predict(f_m, newdata = test_pred, 
#                                  type = "response")
#   list(mod = f_m, test_predictions = test_pred)
# }
# 
# arugam_test_folds <- lapply(arugam_test_folds, fit_gam, sp_data = sum_arugcki)
# 
# 
# # GAM fit using thin plate splines as smoother
# arugam2_test_folds <- unique(sum_arugcki$fold)
# names(arugam2_test_folds) <- as.character(arugam2_test_folds)
# 
# fit_gam2 <- function(test_fold2, sp_data2) {
#   train_dat2 <- sp_data2[sp_data2$fold != test_fold2, ]
#   f_m2 <- gam(count ~ 1 + s(wind, bs = "tp") + s(day_of_yr_c, bs = "tp"), 
#               data = sp_data2, 
#               family = "nb", select = TRUE)
#   test_pred2 <- sp_data2[sp_data2$fold == test_fold2, ]
#   test_pred2$OOB_preds <- predict(f_m2, newdata = test_pred2, 
#                                   type = "response")
#   list(mod = f_m2, test_predictions2 = test_pred2)
# }
# 
# arugam2_test_folds <- lapply(arugam2_test_folds, fit_gam2, 
#                              sp_data2 = sum_arugcki)
# 
# 
# ## end GAM with GCKI per day (ARU)----------------------------------------------

## evaluate GAM (GCKI per day model)- ARU---------------------------------------
arugcki.day.gam
gam.check(arugcki.day.gam)
try(plot(arugcki.day.gam, pages = 1, all.terms = T))

arugcki.day.gam2
gam.check(arugcki.day.gam2)
try(plot(arugcki.day.gam2, pages = 1, all.terms = T))
## TODO: something funky happening here....

## end evaluate GAM (GCKI per day model)- ARU-----------------------------------


################################################################################
################################################################################

## GAM with WIWR per day (ptct)-------------------------------------------------
k <- -1 # k should be large enough that EDF is a good bit less than k-1.  

## test GAM fitting with all data
# for interaction discussion, see Section 5.6.3 and p 344 of Wood
#  + ti(wind, day_of_yr_c, k = k)
wiwr.day.gam <- gam(count ~ 1 + s(windknots, k = k) + s(day_of_yr_c, k = k), 
                    data = sum_wiwr, 
                    family = "poisson", select = TRUE)

## test GAM fitting with all data and cubic regression spline smooth
wiwr.day.gam2 <- gam(count ~ 1 + s(windknots, bs = "cr") + s(day_of_yr_c, bs = "cr"), 
                     data = sum_wiwr, 
                     family = "poisson", select = TRUE)


## WIWR point count GAM with 1000 iterations
fits_wiwr_gam <- list()
for (i in 1:200) {
  # assign days to 3-day blocks
  days <- data.frame(day = min(sum_wiwr$day_of_yr):max(sum_wiwr$day_of_yr), 
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
  
  # create new data to predict with
  pwiwr_day <- data.frame(day_of_yr = seq(min(sum_wiwr$day_of_yr), 
                                          max(sum_wiwr$day_of_yr), by = 1))
  pwiwr_day$day_of_yr_c <- pwiwr_day$day_of_yr-mean(pwiwr_day$day_of_yr)
  pwiwr_day$windknots <- mean(sum_wiwr$windknots)
  pwiwr_day <- left_join(pwiwr_day, days, by = c("day_of_yr" = "day"))
  
  # join CV fold info onto bird data
  bird_dat <- left_join(sum_wiwr, days, by = c("day_of_yr" = "day"))
  
  # fit brt to data in CV folds
  brt_test_folds <- unique(bird_dat$fold)
  names(brt_test_folds) <- as.character(brt_test_folds) 
  
  brt_test_folds <- lapply(brt_test_folds, fit_gam, sp_data = bird_dat, 
                           newdata = pwiwr_day)
  
  # put predictions for these 5 folds into the big list for all splits
  fits_wiwr_gam[[i]] <- brt_test_folds
}
names(fits_wiwr_gam) <- 1:length(fits_wiwr_gam)
rmse_wiwr_gam <- sapply(fits_wiwr_gam, FUN = function(x) {
  sapply(x, FUN = function(z) {
    sqrt(mean(z$test_predictions$error^2))
  })
})
saveRDS(fits_wiwr_gam, "fits_wiwr_gam.rds")
saveRDS(rmse_wiwr_gam, "rmse_wiwr_gam.rds")
rm(fits_wiwr_gam, rmse_wiwr_gam)

## end GAM with WIWR per day----------------------------------------------------

## evaluate GAM (WIWR per day model)--------------------------------------------
# wiwr.day.gam
# gam.check(wiwr.day.gam)
# plot(wiwr.day.gam, pages = 1, all.terms = T)
# 
# wiwr.day.gam2
# gam.check(wiwr.day.gam2)
# plot(wiwr.day.gam2, pages = 1, all.terms = T)

## end evaluate GAM (WIWR per day model-- ptct)---------------------------------


## TODO: GAM with WIWR per day (ARU)-------------------------------------------------

## WIWR GAM with 1000 iterations
fits_aruwiwr_gam <- list()
for (i in 1:200) {
  # assign days to 3-day blocks
  days <- data.frame(day = min(sum_aruwiwr$day_of_yr):max(sum_aruwiwr$day_of_yr), 
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
  
  # create new data to predict with
  pwiwr_day <- data.frame(day_of_yr = seq(min(sum_wiwr$day_of_yr), 
                                          max(sum_wiwr$day_of_yr), by = 1))
  pwiwr_day$day_of_yr_c <- pwiwr_day$day_of_yr-mean(pwiwr_day$day_of_yr)
  pwiwr_day$windknots <- mean(sum_wiwr$windknots)
  pwiwr_day <- left_join(pwiwr_day, days, by = c("day_of_yr" = "day"))
  
  # join CV fold info onto bird data
  bird_dat <- left_join(sum_aruwiwr, days, by = c("day_of_yr" = "day"))
  
  # fit brt to data in CV folds
  brt_test_folds <- unique(bird_dat$fold)
  names(brt_test_folds) <- as.character(brt_test_folds) 
  
  brt_test_folds <- lapply(brt_test_folds, fit_gam, sp_data = bird_dat, 
                           newdata = pwiwr_day)
  
  # put predictions for these 5 folds into the big list for all splits
  fits_aruwiwr_gam[[i]] <- brt_test_folds
}
names(fits_aruwiwr_gam) <- 1:length(fits_aruwiwr_gam)
rmse_aruwiwr_gam <- sapply(fits_aruwiwr_gam, FUN = function(x) {
  sapply(x, FUN = function(z) {
    sqrt(mean(z$test_predictions$error^2))
  })
})
saveRDS(fits_aruwiwr_gam, "fits_aruwiwr_gam.rds")
saveRDS(rmse_aruwiwr_gam, "rmse_aruwiwr_gam.rds")
rm(fits_aruwiwr_gam, rmse_aruwiwr_gam)



## test GAM fitting with all data
# for interaction discussion, see Section 5.6.3 and p 344 of Wood
#  + ti(windknots, day_of_yr_c, k = k)
aruwiwr.day.gam <- gam(count ~ 1 + s(windknots, k = k) + s(day_of_yr_c, k = k), 
                       data = sum_aruwiwr, 
                       family = "nb", select = TRUE)

## test GAM fitting with all data and thin plate spline smooth
aruwiwr.day.gam2 <- gam(count ~ 1 + s(windknots, bs = "tp") + s(day_of_yr_c, bs = "tp"), 
                        data = sum_aruwiwr, 
                        family = "nb", select = TRUE)

## end GAM with WIWR per day (ARU)----------------------------------------------

## evaluate GAM (WIWR per day model)- ARU---------------------------------------
aruwiwr.day.gam
gam.check(aruwiwr.day.gam)
try(plot(aruwiwr.day.gam, pages = 1, all.terms = T))

aruwiwr.day.gam2
gam.check(aruwiwr.day.gam2)
try(plot(aruwiwr.day.gam2, pages = 1, all.terms = T))


## end evaluate GAM (WIWR per day model)- ARU-----------------------------------












## join brt and gam predictions and errors -------------------------------------
# gcki_gam_predictions$method = "pt_gam1"
# gcki_brt_predictions$method = "pt_brt"
# gcki_gam2_predictions$method = "pt_gam2"
# arugcki_brt_predictions$method = "aru_brt"
# arugcki_gam_predictions$method = "aru_gam1"
# arugcki_gam2_predictions$method = "aru_gam2"
# 
# gcki_predictions <- bind_rows(gcki_gam_predictions, gcki_brt_predictions, 
#                               gcki_gam2_predictions, arugcki_brt_predictions, 
#                               arugcki_gam_predictions, arugcki_gam2_predictions)
# gcki_predictions <- pivot_longer(gcki_predictions, 
#                                  cols = c(error, OOB_preds), 
#                                  names_to = "metric")
# 
# ggplot(data = gcki_predictions[gcki_predictions$metric == "error", ], 
#        aes(x = day_of_yr, y = value, color = method)) + 
#   geom_point() + 
#   ggtitle("Prediction error")
# 
# ggplot(data = gcki_predictions[gcki_predictions$metric == "OOB_preds", ], 
#        aes(x = day_of_yr, y = value, color = method)) + 
#   geom_point()  + 
#   geom_smooth() + 
#   geom_point(aes(x = day_of_yr, y = count), color = "black") + 
#   ggtitle("Predictions")
# 
# plot(gcki_gam_predictions$OOB_preds ~ gcki_brt_predictions$OOB_preds)
# plot(gcki_gam2_predictions$OOB_preds ~ gcki_gam_predictions$OOB_preds)
# plot(gcki_gam_predictions$error ~ gcki_brt_predictions$error)
# plot(gcki_gam_predictions$error ~ gcki_gam2_predictions$error)
# plot(gcki_brt_predictions$OOB_preds ~ arugcki_brt_predictions$OOB_preds)
# plot(gcki_gam_predictions$OOB_preds ~ arugcki_gam_predictions$OOB_preds)
## end brt and gam predictions plot --------------------------------------------
}
