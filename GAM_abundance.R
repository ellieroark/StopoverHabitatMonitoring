################################
## Abundance estimation: Boosted Regression Trees
## Point Abbaye 2019 data
## 
## author: Ellie Roark, Willson Gaul
## created: 6 Mar 2020
## last modified: 6 Mar 2020
## 
## inputs: *MUST FIRST RUN: DataPrep_PtCtAbundance.R- script reads in original
##          point count data and prepares dataframes with the number of observed 
##          GCKI and WIWR per 10 minute point count and per day
##         *AND BRT_abundance.R- script that fits GLM and BRT for GCKI and WIWR
##          data
##           
## outputs: *
##            
## TODO: * 
################################


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
