################################
## Abundance estimation: Boosted Regression Trees
## Point Abbaye 2019 data
## 
## author: Ellie Roark, Willson Gaul
## created: 6 Mar 2020
## last modified: 15 Apr 2020
## 
## inputs: *MUST FIRST RUN: DataPrep_PtCtAbundance.R- script reads in original
##          point count data and prepares dataframes with the number of observed 
##          GCKI and WIWR per 10 minute point count and per day   
##         *AND: DataPrep_ARUabundance.R- script reads in original ARU
##          observations and prepares dfs with # of GCKI and WIWR vocalizations
##          in every 30 second period of ARU listening  
##           
## outputs: *
##            
## TODO: * 
################################

#set number of trees for BRT models
nt = 2000

plotson <- FALSE
fitbrt <- FALSE

if(plotson){
# Exploratory plots -----------------------------------------------------------
#hist(ptct$count, main = "number of individs")


# plot wren mean detections over time
plot(wiwr$day_of_yr, wiwr$count,
     main = "wiwr per pt count over time")

plot(gcki$day_of_yr, gcki$count,
     main = "gcki per pt count over time")




plot(sum_gcki$day_of_yr, sum_gcki$meandet,
     main = "avg. gcki per count per day over time")
ggplot(data = sum_gcki, aes(x = day_of_yr, y = meandet)) +
  geom_point() +
  geom_smooth()

ggplot(data = sum_arugcki, aes(x = day_of_yr, y = meandet)) +
  geom_point() +
  geom_smooth()

ggplot(data = sum_arugcki10r, aes(x = day_of_yr, y = meandet)) +
  geom_point() +
  geom_smooth()

ggplot(data = sum_arugcki22r, aes(x = day_of_yr, y = meandet)) +
  geom_point() +
  geom_smooth()



plot(sum_wiwr$day_of_yr, sum_wiwr$meandet,
     main = "avg. wiwr per count per day over time")
ggplot(data = sum_wiwr, aes(x = day_of_yr, y = meandet)) +
  geom_point() +
  geom_smooth()

ggplot(data = sum_aruwiwr, aes(x = day_of_yr, y = meandet)) +
  geom_point() +
  geom_smooth()

ggplot(data = sum_aruwiwr10r, aes(x = day_of_yr, y = meandet)) +
  geom_point() +
  geom_smooth()

ggplot(data = sum_aruwiwr22r, aes(x = day_of_yr, y = meandet)) +
  geom_point() +
  geom_smooth()

hist(sum_gcki$meandet)
hist(sum_arugcki$meandet)
hist(sum_arugcki10r$meandet)
hist(sum_arugcki22r$meandet)

hist(sum_wiwr$meandet)
hist(sum_aruwiwr$meandet)
hist(sum_aruwiwr10r$meandet)
hist(sum_aruwiwr22r$meandet)



# end Exploratory plots -------------------------------------------------------
}

# # GLM for GCKI counts over time------------------------------------------------
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
# # end GLM for GCKI counts----------------------------------------------------
# 
# # GLM for avg # of GCKI per count per day--------------------------------------
# # model of avg GCKI ** per sampling unit ** over time
# gcki.day <- glm(meandet ~ 1 + wind + day_of_yr_c + day_sq,
#                 data = sum_gcki,
#                 family = "poisson")
# 
# summary(gcki.day)
# plot(gcki.day)
# 
# ## this is not great-- residuals vs fitted vals look flared still-- indicating 
# ## that variance does not change with the mean
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
# gdaymeandet <-  table(sum_gcki$meandet)
# barplot(gdaymeandet, main = "distribution of avg # of gcki per count per day", 
#         xlab = "mean detected per count",
#         ylab = "frequency")
# 
# # check unconditional mean and variance for sp_detected (response variable)
# # (we ultimately care only about CONDITIONAL mean and variance being equal after
# # model is fit but this is a good indicator of whether it might be a problem)
# mean(sum_gcki$meandet)
# var(sum_gcki$meandet)
# # this actually looks okay, probably.
# 
# #look at deviance statistic of fit model divided by its d.f. to see if ratio
# # is over 1
# gcki.day$deviance
# gcki.day$df.residual
# #(this is the ratio we care about)
# with(gcki.day, deviance/df.residual)
# #(this gives us a p-value for that ratio)
# with(gcki.day, pchisq(deviance, df.residual, lower.tail = FALSE))
# ## this is okay-- ratio is 0.74, p-valye is .89-- not sig different from 1
# 
# # test neg binom model for GCKI per day
# gcki.day.nb <- glm.nb(meandet ~ 1 + wind + day_of_yr_c + day_sq,
#                       data = sum_gcki)
# 
# #get fitted values
# sum_gcki$nbfv <- predict(gcki.day.nb, type="response")
# 
# plot(sum_gcki$meandet ~ sum_gcki$nbfv)
# abline(0, 1)
# 
# td <- data.frame(simulate(gcki.day.nb, nsim = 10))
# td$obs <- sum_gcki$meandet
# td$doy <- sum_gcki$day_of_yr
# td <- pivot_longer(td, 1:(ncol(td)-2), names_to = "case", values_to = "n_sp")
# 
# ggplot(data = td, aes(x = n_sp, y = obs)) +
#   geom_point() +
#   geom_jitter() +
#   geom_smooth()
# 
# ggplot() +
#   geom_point(data = td, aes(x = doy, y = n_sp), alpha = 0.1) +
#   geom_point(data = td[td$case == "obs", ], aes(x = doy, y = n_sp),
#              col = "red")
# 
# #look at deviance statistic of fit model divided by its d.f. to see if ratio
# # is over 1
# gcki.day.nb$deviance
# gcki.day.nb$df.residual
# #(this is the ratio we care about)
# with(gcki.day.nb, deviance/df.residual)
# #(this gives us a p-value for that ratio)
# with(gcki.day.nb, pchisq(deviance, df.residual, lower.tail = FALSE))
# ## this is way better than poisson-- ratio is 1.09, pvalue is .3-- not sig.
# ## different from 1.
# 
# ## end GLM for GCKI per day-----------------------------------------------------

# ## Boosted Regression Tree for GCKI per COUNT-----------------------------------
# 
# gcki.brt <- gbm(count ~ 1 + wind + rain + noise + day_of_yr_c + cloud_cover +
#                   day_sq + min_past_sun, 
#                 distribution = "poisson", 
#                 data = gcki, 
#                 interaction.depth = 3, 
#                 n.trees = nt, 
#                 n.minobsinnode = 5, 
#                 shrinkage = 0.01, 
#                 bag.fraction = 0.8)
# 
# ## end BRT for GCKI per COUNT model---------------------------------------------
# 
# ## predictions with BRT **per count model***-----------------------------------
# # create new data to predict with
# pred_gcki <- data.frame(day_of_yr = seq(min(gcki$day_of_yr), 
#                                         max(gcki$day_of_yr), by = 1))
# pred_gcki$day_of_yr_c <- pred_gcki$day_of_yr-mean(pred_gcki$day_of_yr)
# pred_gcki$day_sq <- pred_gcki$day_of_yr_c^2
# pred_gcki$min_past_sun <- median(gcki$min_past_sun)
# pred_gcki$wind <- as.factor("0-1")
# pred_gcki$rain <- as.factor("Dry")
# pred_gcki$noise <- as.factor("0")
# pred_gcki$cloud_cover <- as.factor("0-33")
# #coerce factor variables to contain the same number of levels as the original 
# pred_gcki$wind <- factor(pred_gcki$wind, 
#                          levels = c("0-1", "2", "3+"),
#                          labels = c("0-1", "2", "3+"))
# pred_gcki$rain <- factor(pred_gcki$rain, 
#                          levels = c("Dry", "wet"),
#                          labels = c("Dry", "wet"))
# pred_gcki$noise <- factor(pred_gcki$noise, 
#                           levels = c("0", "1", ">2"), 
#                           labels = c("0", "1", ">2"))
# pred_gcki$cloud_cover <- factor(pred_gcki$cloud_cover, 
#                                 levels = c("0-33", "33-66", "66-100"), 
#                                 labels = c("0-33", "33-66", "66-100"))
# 
# # get predictions with new data
# pred_gcki$p1 <- predict(gcki.brt, newdata = pred_gcki, n.trees = nt, 
#                         type = "response")
# 
# #plot predictions over time 
# predgcki_time <- ggplot(pred_gcki, aes(x=day_of_yr, y=p1)) + 
#   geom_line() + 
#   geom_point(data = gcki,
#              aes(x = day_of_yr, y = count)) +
#   theme_bw() +
#   scale_colour_viridis_d() + 
#   scale_y_continuous() + 
#   ylab("Number of GCKI") +
#   xlab("Day of Year")
# predgcki_time
# ## end predictions with BRT **GCKI per count model**----------------------------

## Boosted Regression Tree for GCKI per DAY (point counts)----------------------
# define function to fit boosted regression tree
fit_brt <- function(test_fold, sp_data, newdata, nt) {
  train_dat <- sp_data[sp_data$fold != test_fold, ]
  f_m <- gbm(meandet ~ 1 + wind + day_of_yr_c, 
             distribution = "laplace", 
             data = train_dat, 
             interaction.depth = 1, 
             n.trees = nt, 
             n.minobsinnode = 1, 
             shrinkage = 0.001, 
             bag.fraction = 0.8, 
             keep.data = FALSE, verbose = F, n.cores = 1)
  test_pred <- sp_data[sp_data$fold == test_fold, ]
  test_pred$OOB_preds <- predict(f_m, newdata = test_pred, 
                                 n.trees = nt, type = "response")
  test_pred$error <- test_pred$OOB_preds - test_pred$meandet
  
  # Get standardized predictions to new data
  stand_pred <- newdata[newdata$fold == test_fold, ]
  stand_pred$predictions <- predict(f_m, newdata = stand_pred, 
                                 n.trees = nt, type = "response")
  #stand_pred$error <- stand_pred$predictions - stand_pred$meandet
  rm(f_m)
  # return fitted model, predictions to the observed data from the test fold, 
  # and predictions to new data (with standardized covariates)
  list(test_predictions = test_pred, standardized_preds = stand_pred) #mod = f_m, 
}

if(fitbrt){
# make a list to hold fitted models and predictions from models with many 
# replicate folds splits
# At the end of the following for loop, fits_gcki_brt should have results from 
# 200 different 5-fold CV splits (so 1000 different models fit, 5 for each fold
# in each of 200 different fold splits)
fits_gcki_brt <- list()
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
  pgcki_day$wind <- mean(sum_gcki$wind)
  pgcki_day <- left_join(pgcki_day, days, by = c("day_of_yr" = "day"))
  
  # join CV fold info onto bird data
  bird_dat <- left_join(sum_gcki, days, by = c("day_of_yr" = "day"))
 
  # fit brt to data in CV folds
  brt_test_folds <- unique(bird_dat$fold)
  names(brt_test_folds) <- as.character(brt_test_folds) 
  
  brt_test_folds <- lapply(brt_test_folds, fit_brt, sp_data = bird_dat, 
                           newdata = pgcki_day, nt = nt)
  
  # put predictions for these 5 folds into the big list for all splits
  fits_gcki_brt[[i]] <- brt_test_folds
  rm(brt_test_folds)
}
names(fits_gcki_brt) <- 1:length(fits_gcki_brt)
rmse_gcki_brt <- sapply(fits_gcki_brt, FUN = function(x) {
  sapply(x, FUN = function(z) {
    sqrt(mean(z$test_predictions$error^2))
  })
})
saveRDS(fits_gcki_brt, "fits_gcki_brt.rds")
saveRDS(rmse_gcki_brt, "rmse_gcki_brt.rds")
rm(fits_gcki_brt, rmse_gcki_brt)
}


## alternate BRT with interaction depth of 2 instead of 1 
# gcki.brt2 <- gbm(meandet ~ 1 + wind + day_of_yr_c, 
#            distribution = "poisson", 
#            data = sum_gcki, 
#            interaction.depth = 2, 
#            n.trees = nt, 
#            n.minobsinnode = 1, 
#            shrinkage = 0.001, 
#            bag.fraction = 0.8)

## end BRT for GCKI per DAY model---------------------------------------------


## evaluate BRT ----------------------------------------------------------------
# ## BRT with interaction depth 1
# 
# gcki_brt_predictions$error = gcki_brt_predictions$OOB_preds - gcki_brt_predictions$meandet
# 
# # calculate r^2 (square of Pearson correlation coefficient, see Bahn & 
# # McGill 2013)
# gcki_brt_r2 <- cor(gcki_brt_predictions$day_of_yr, 
#                    gcki_brt_predictions$OOB_preds, 
#                    method = "pearson")^2
# # calculate R^2 (coefficient of determination, see Bahn & McGill 2013)
# gcki_brt_R2 <- 1 - (sum(gcki_brt_predictions$error^2) / 
#                       (sum((gcki_brt_predictions$meandet - 
#                               mean(gcki_brt_predictions$meandet))^2)))
# ## end evaluate BRT ------------------------------------------------------------


################################################################################
## GLM for avg # of GCKI **per count, per day** with ARU- 10 consec data--------
# model of avg # GCKI ** per count per day ** over time with ARU data
# arugcki.day <- glm(meandet ~ 1 + wind + day_of_yr_c + day_sq,
#                 data = sum_arugcki,
#                 family = "poisson")
# 
# summary(arugcki.day)
# #plot(arugcki.day)
# 
# vif(arugcki.day)
# aresids <- rstandard(arugcki.day)
# 
# p.day.aresids <- residuals(arugcki.day, type = "deviance")
# sum_arugcki$pois_devresids <- p.day.aresids
# 
# sum_arugcki[sum_arugcki$pois_devresids > 2, ]
# 
# hist(p.day.aresids, main = "Deviance Residuals for gcki (day, aru) model", 
#      xlab = "residuals")
# boxplot(p.day.aresids, main = "Deviance residuals for gcki (day, aru) model", 
#         ylab ="residuals")
# 
# 
# arugdaymeandet <-  table(sum_arugcki$meandet)
# barplot(arugdaymeandet, main = "distribution of avg # gcki per count", 
#         xlab = "mean # detected per count",
#         ylab = "frequency")
# 
# # check unconditional mean and variance for sp_detected (response variable)
# # (we ultimately care only about CONDITIONAL mean and variance being equal after
# # model is fit but this is a good indicator of whether it might be a problem)
# mean(sum_arugcki$meandet)
# var(sum_arugcki$meandet)
# # looks real bad! suspect overdispersion.
# 
# #look at deviance statistic of fit model divided by its d.f. to see if ratio
# # is over 1
# arugcki.day$deviance
# arugcki.day$df.residual
# #(this is the ratio we care about)
# with(arugcki.day, deviance/df.residual)
# #(this gives us a p-value for that ratio)
# with(arugcki.day, pchisq(deviance, df.residual, lower.tail = FALSE))
# ## this is truly just not poisson. way, way overdispersed. 
# ## -- ratio is 6.38 (p <0.001), definitely different from 1!!!
# 
# # test neg binom model for GCKI per day (aru) to see if it helps with 
# # overdispersion
# arugcki.day.nb <- glm.nb(meandet ~ 1 + wind + day_of_yr_c + day_sq,
#                       data = sum_arugcki, control = glm.control(maxit = 1000))
# ## this does not converge
# 
# #get fitted values
# sum_arugcki$nbfv <- predict(arugcki.day.nb, type="response")
# 
# plot(sum_arugcki$meandet ~ sum_arugcki$nbfv)
# abline(0, 1)
# 
# # TODO what does this section of code do?? ask wg
# gtd <- data.frame(simulate(arugcki.day.nb, nsim = 10))
# gtd$obs <- sum_arugcki$meandet
# gtd$doy <- sum_arugcki$day_of_yr
# gtd <- pivot_longer(gtd, 1:(ncol(gtd)-2), names_to = "case", values_to = "n_sp")
# 
# ggplot(data = gtd, aes(x = n_sp, y = obs)) +
#   geom_point() +
#   geom_jitter() +
#   geom_smooth()
# 
# ggplot() +
#   geom_point(data = gtd, aes(x = doy, y = n_sp), alpha = 0.1) +
#   geom_point(data = gtd[gtd$case == "obs", ], aes(x = doy, y = n_sp),
#              col = "red")
# 
# #look at deviance statistic of fit model divided by its d.f. to see if ratio
# # is over 1
# arugcki.day.nb$deviance
# arugcki.day.nb$df.residual
# #(this is the ratio we care about)
# with(arugcki.day.nb, deviance/df.residual)
# #(this gives us a p-value for that ratio)
# with(arugcki.day.nb, pchisq(deviance, df.residual, lower.tail = FALSE))
# ## this is way better than poisson-- ratio is .78, pvalue is .79-- not sig.
# ## different from 1.

## end GLM for GCKI per day (ARU)-----------------------------------------------

if(fitbrt){
## Boosted Regression Tree for GCKI per DAY (ARU10c)----------------------------
# make a list to hold fitted models and predictions from models with many 
# replicate folds splits
# At the end of the following for loop, fits_arugcki_brt should have results from 
# 200 different 5-fold CV splits (so 1000 different models fit, 5 for each fold
# in each of 200 different fold splits)
fits_arugcki_brt <- list()

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
  parugcki_day <- data.frame(day_of_yr = seq(min(sum_arugcki$day_of_yr), 
                                             max(sum_arugcki$day_of_yr), by = 1))
  parugcki_day$day_of_yr_c <- parugcki_day$day_of_yr-mean(parugcki_day$day_of_yr)
  parugcki_day$wind <- mean(sum_arugcki$wind)
  parugcki_day <- left_join(parugcki_day, days, by = c("day_of_yr" = "day"))
  
  # join CV fold info onto bird data
  bird_dat <- left_join(sum_arugcki, days, by = c("day_of_yr" = "day"))
  
  # fit brt to data in CV folds
  brt_test_folds <- unique(bird_dat$fold)
  names(brt_test_folds) <- as.character(brt_test_folds) 
  
  brt_test_folds <- lapply(brt_test_folds, fit_brt, sp_data = bird_dat, 
                           newdata = parugcki_day, nt = nt)
  
  # put predictions for these 5 folds into the big list for all splits
  fits_arugcki_brt[[i]] <- brt_test_folds
  rm(brt_test_folds)
}
names(fits_arugcki_brt) <- 1:length(fits_arugcki_brt)
rmse_arugcki_brt <- sapply(fits_arugcki_brt, FUN = function(x) {
  sapply(x, FUN = function(z) {
    sqrt(mean(z$test_predictions$error^2))
  })
})

saveRDS(fits_arugcki_brt, "fits_arugcki_brt.rds")
saveRDS(rmse_arugcki_brt, "rmse_arugcki_brt.rds")
rm(fits_arugcki_brt, rmse_arugcki_brt)

## end BRT for GCKI per DAY model (ARU)-----------------------------------------


## BRT for GCKI per day (ARU-10 random min)-------------------------------------
fits_arugcki10r_brt <- list()
for (i in 1:200) {
  gc()
  # assign days to 3-day blocks
  days <- data.frame(day = min(sum_arugcki10r$day_of_yr):
                       max(sum_arugcki10r$day_of_yr), 
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
  pgcki_day <- data.frame(day_of_yr = seq(min(sum_arugcki10r$day_of_yr), 
                                          max(sum_arugcki10r$day_of_yr), by = 1))
  pgcki_day$day_of_yr_c <- pgcki_day$day_of_yr-mean(pgcki_day$day_of_yr)
  pgcki_day$wind <- mean(sum_arugcki10r$wind)
  pgcki_day <- left_join(pgcki_day, days, by = c("day_of_yr" = "day"))
  
  # join CV fold info onto bird data
  bird_dat <- left_join(sum_arugcki10r, days, by = c("day_of_yr" = "day"))
  
  # fit brt to data in CV folds
  brt_test_folds <- unique(bird_dat$fold)
  names(brt_test_folds) <- as.character(brt_test_folds) 
  
  brt_test_folds <- lapply(brt_test_folds, fit_brt, sp_data = bird_dat, 
                           newdata = pgcki_day, nt = nt)
  
  # put predictions for these 5 folds into the big list for all splits
  fits_arugcki10r_brt[[i]] <- brt_test_folds
  rm(brt_test_folds)
}
names(fits_arugcki10r_brt) <- 1:length(fits_arugcki10r_brt)
rmse_arugcki10r_brt <- sapply(fits_arugcki10r_brt, FUN = function(x) {
  sapply(x, FUN = function(z) {
    sqrt(mean(z$test_predictions$error^2))
  })
})
saveRDS(fits_arugcki10r_brt, "fits_arugcki10r_brt.rds")
saveRDS(rmse_arugcki10r_brt, "rmse_arugcki10r_brt.rds")
rm(fits_arugcki10r_brt, rmse_arugcki10r_brt)
## end BRT for GCKI per day- ARU 10 random min----------------------------------

## BRT for GCKI per day- ARU 22 random min--------------------------------------
fits_arugcki22r_brt <- list()
for (i in 1:200) {
  gc()
  # assign days to 3-day blocks
  days <- data.frame(day = min(sum_arugcki22r$day_of_yr):
                       max(sum_arugcki22r$day_of_yr), 
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
  pgcki_day <- data.frame(day_of_yr = seq(min(sum_arugcki22r$day_of_yr), 
                                          max(sum_arugcki22r$day_of_yr), by = 1))
  pgcki_day$day_of_yr_c <- pgcki_day$day_of_yr-mean(pgcki_day$day_of_yr)
  pgcki_day$wind <- mean(sum_arugcki22r$wind)
  pgcki_day <- left_join(pgcki_day, days, by = c("day_of_yr" = "day"))
  
  # join CV fold info onto bird data
  bird_dat <- left_join(sum_arugcki22r, days, by = c("day_of_yr" = "day"))
  
  # fit brt to data in CV folds
  brt_test_folds <- unique(bird_dat$fold)
  names(brt_test_folds) <- as.character(brt_test_folds) 
  
  brt_test_folds <- lapply(brt_test_folds, fit_brt, sp_data = bird_dat, 
                           newdata = pgcki_day, nt = nt)
  
  # put predictions for these 5 folds into the big list for all splits
  fits_arugcki22r_brt[[i]] <- brt_test_folds
  rm(brt_test_folds)
}
names(fits_arugcki22r_brt) <- 1:length(fits_arugcki22r_brt)
rmse_arugcki22r_brt <- sapply(fits_arugcki22r_brt, FUN = function(x) {
  sapply(x, FUN = function(z) {
    sqrt(mean(z$test_predictions$error^2))
  })
})
saveRDS(fits_arugcki22r_brt, "fits_arugcki22r_brt.rds")
saveRDS(rmse_arugcki22r_brt, "rmse_arugcki22r_brt.rds")
rm(fits_arugcki22r_brt, rmse_arugcki22r_brt)
## end BRT for GCKI per day- ARU 22 random min----------------------------------
}

###############################################################################
## WIWR models ################################################################
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
# barplot(wcount, main = "distribution of # of wiwr per count", xlab = "count",
#         ylab = "frequency")
# 
# # check unconditional mean and variance for sp_detected (response variable)
# # (we ultimately care only about CONDITIONAL mean and variance being equal after
# # model is fit but this is a good indicator of whether it might be a problem)
# mean(wiwr$count)
# var(wiwr$count)
# # this actually looks fine-- .59/.56. will still check for overdispersion
# 
# #look at deviance statistic of fit model divided by its d.f. to see if ratio
# # is over 1
# wiwr.ct$deviance
# wiwr.ct$df.residual
# #(this is the ratio we care about)
# with(wiwr.ct, deviance/df.residual)
# #(this gives us a p-value for that ratio)
# with(wiwr.ct, pchisq(deviance, df.residual, lower.tail = FALSE))
# ## not overdispered. ratio is .8, p-value is .9, so the ratio is NOT different 
# ## from 1.
# 
# # get fitted values for wiwr poisson model
# wiwr$fv <- predict(wiwr.ct, type="response")
# 
# #make data long format so that I can plot fit val AND original counts over time
# mwiwr <- data.frame(day_of_yr = wiwr$day_of_yr,
#                     fv = wiwr$fv,
#                     count = wiwr$count)
# mwiwr <- pivot_longer(mwiwr, cols = fv:count, names_to = 'data_type')
# 
# 
# #plot fitted values for poisson model over time, alongside actual values
# pwiwr_time <- ggplot(mwiwr, aes(x=day_of_yr, y=value, colour=data_type)) +
#         geom_point() +
#         theme_bw() +
#         scale_colour_viridis_d() +
#         scale_y_continuous() +
#         ylab("Number of WIWR") +
#         xlab("Day of Year")
# pwiwr_time
# 
# plot(wiwr$count ~ wiwr$fv)
# abline(0, 1)
# 
# td2 <- data.frame(simulate(wiwr.ct, nsim = 10))
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
#         geom_point(data = td2, aes(x = doy, y = n_sp), alpha = 0.1) +
#         geom_point(data = td2[td2$case == "obs", ], aes(x = doy, y = n_sp),
#                    col = "red")
## end GLM for WIWR counts over time--------------------------------------------

## GLM for avg WIWR **per count, per day**----------------------------------------------
# model of avg WIWR ** per count per day** over time
# wiwr.day <- glm(meandet ~ 1 + wind + day_of_yr_c + day_sq,
#                 data = sum_wiwr,
#                 family = "poisson")
# 
# summary(wiwr.day)
# #plot(gcki.day)
# 
# vif(wiwr.day)
# resids <- rstandard(wiwr.day)
# 
# p.wday.resids <- residuals(wiwr.day, type = "deviance")
# sum_wiwr$pois_devresids <- p.wday.resids
# 
# sum_wiwr[sum_wiwr$pois_devresids > 2, ]
# 
# hist(p.wday.resids, main = "Deviance Residuals for wiwr (day) model", xlab =
#              "residuals")
# boxplot(p.wday.resids, main = "Deviance residuals for wiwr (day) model", ylab =
#                 "residuals")
# 
# 
# wdaymeandet <-  table(sum_wiwr$meandet)
# barplot(wdaymeandet, main = "distribution of avg # of wiwr per count", 
#         xlab = "mean # WIWR per count",
#         ylab = "frequency")
# 
# # check unconditional mean and variance for sp_detected (response variable)
# # (we ultimately care only about CONDITIONAL mean and variance being equal after
# # model is fit but this is a good indicator of whether it might be a problem)
# mean(sum_wiwr$meandet)
# var(sum_wiwr$meandet)
# # 2.2/4.6-- doesn't look great! overdispersion may be a problem here.
# 
# #look at deviance statistic of fit model divided by its d.f. to see if ratio
# # is over 1
# wiwr.day$deviance
# wiwr.day$df.residual
# #(this is the ratio we care about)
# with(wiwr.day, deviance/df.residual)
# #(this gives us a p-value for that ratio)
# with(wiwr.day, pchisq(deviance, df.residual, lower.tail = FALSE))
# ## this actually looks fine-- ratio is 1.1; p = .31, indicating that this is not
# ## sig different from 1 and therefore not overdispersed.
# 
# ## end GLM for GCKI per day-----------------------------------------------------

if(fitbrt){
## Boosted Regression Tree for WIWR per DAY (ptct)-------------------------------
# make a list to hold fitted models and predictions from models with many 
# replicate folds splits
# At the end of the following for loop, fits_gcki_brt should have results from 
# 200 different 5-fold CV splits (so 1000 different models fit, 5 for each fold
# in each of 200 different fold splits)
fits_wiwr_brt <- list()

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
  pwiwr_day$wind <- mean(sum_wiwr$wind)
  pwiwr_day <- left_join(pwiwr_day, days, by = c("day_of_yr" = "day"))
  
  # join CV fold info onto bird data
  bird_dat <- left_join(sum_wiwr, days, by = c("day_of_yr" = "day"))
  
  # fit brt to data in CV folds
  brt_test_folds <- unique(bird_dat$fold)
  names(brt_test_folds) <- as.character(brt_test_folds) 
  
  brt_test_folds <- lapply(brt_test_folds, fit_brt, sp_data = bird_dat, 
                           newdata = pwiwr_day, nt = nt)
  
  # put predictions for these 5 folds into the big list for all splits
  fits_wiwr_brt[[i]] <- brt_test_folds
  rm(brt_test_folds)
  gc()
}
names(fits_wiwr_brt) <- 1:length(fits_wiwr_brt)

rmse_wiwr_brt <- sapply(fits_wiwr_brt, FUN = function(x) {
  sapply(x, FUN = function(z) {
    sqrt(mean(z$test_predictions$error^2))
  })
})
saveRDS(fits_wiwr_brt, "fits_wiwr_brt.rds")
saveRDS(rmse_wiwr_brt, "rmse_wiwr_brt.rds")
rm(fits_wiwr_brt, rmse_wiwr_brt)
## end BRT for WIWR per DAY model (ptct)----------------------------------------


## Boosted Regression Tree for WIWR per DAY (ARU- 10 consec min)----------------
# make a list to hold fitted models and predictions from models with many 
# replicate folds splits
# At the end of the following for loop, fits_aruwiwr_brt should have results from 
# 200 different 5-fold CV splits (so 1000 different models fit, 5 for each fold
# in each of 200 different fold splits)
fits_aruwiwr_brt <- list()

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
  paruwiwr_day <- data.frame(day_of_yr = seq(min(sum_aruwiwr$day_of_yr), 
                                             max(sum_aruwiwr$day_of_yr), by = 1))
  paruwiwr_day$day_of_yr_c <- paruwiwr_day$day_of_yr-mean(paruwiwr_day$day_of_yr)
  paruwiwr_day$wind <- mean(sum_aruwiwr$wind)
  paruwiwr_day <- left_join(paruwiwr_day, days, by = c("day_of_yr" = "day"))
  
  # join CV fold info onto bird data
  bird_dat <- left_join(sum_aruwiwr, days, by = c("day_of_yr" = "day"))
  
  # fit brt to data in CV folds
  brt_test_folds <- unique(bird_dat$fold)
  names(brt_test_folds) <- as.character(brt_test_folds) 
  
  brt_test_folds <- lapply(brt_test_folds, fit_brt, sp_data = bird_dat, 
                           newdata = paruwiwr_day, nt = nt)
  
  # put predictions for these 5 folds into the big list for all splits
  fits_aruwiwr_brt[[i]] <- brt_test_folds
  rm(brt_test_folds)
}
names(fits_aruwiwr_brt) <- 1:length(fits_aruwiwr_brt)

rmse_aruwiwr_brt <- sapply(fits_aruwiwr_brt, FUN = function(x) {
  sapply(x, FUN = function(z) {
    sqrt(mean(z$test_predictions$error^2))
  })
})

saveRDS(fits_aruwiwr_brt, "fits_aruwiwr_brt.rds")
saveRDS(rmse_aruwiwr_brt, "rmse_aruwiwr_brt.rds")
rm(fits_aruwiwr_brt, rmse_aruwiwr_brt)
## end BRT for WIWR per DAY model (ARU- 10 consec min)--------------------------

## BRT for WIWR per day- ARU 10 random min--------------------------------------
fits_aruwiwr10r_brt <- list()
for (i in 1:200) {
  # assign days to 3-day blocks
  days <- data.frame(day = min(sum_aruwiwr10r$day_of_yr):
                       max(sum_aruwiwr10r$day_of_yr), 
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
  pwiwr_day <- data.frame(day_of_yr = seq(min(sum_aruwiwr10r$day_of_yr), 
                                          max(sum_aruwiwr10r$day_of_yr), by = 1))
  pwiwr_day$day_of_yr_c <- pwiwr_day$day_of_yr-mean(pwiwr_day$day_of_yr)
  pwiwr_day$wind <- mean(sum_aruwiwr10r$wind)
  pwiwr_day <- left_join(pwiwr_day, days, by = c("day_of_yr" = "day"))
  
  # join CV fold info onto bird data
  bird_dat <- left_join(sum_aruwiwr10r, days, by = c("day_of_yr" = "day"))
  
  # fit brt to data in CV folds
  brt_test_folds <- unique(bird_dat$fold)
  names(brt_test_folds) <- as.character(brt_test_folds) 
  
  brt_test_folds <- lapply(brt_test_folds, fit_brt, sp_data = bird_dat, 
                           newdata = pwiwr_day, nt = nt)
  
  # put predictions for these 5 folds into the big list for all splits
  fits_aruwiwr10r_brt[[i]] <- brt_test_folds
}
names(fits_aruwiwr10r_brt) <- 1:length(fits_aruwiwr10r_brt)
rmse_aruwiwr10r_brt <- sapply(fits_aruwiwr10r_brt, FUN = function(x) {
  sapply(x, FUN = function(z) {
    sqrt(mean(z$test_predictions$error^2))
  })
})
saveRDS(fits_aruwiwr10r_brt, "fits_aruwiwr10r_brt.rds")
saveRDS(rmse_aruwiwr10r_brt, "rmse_aruwiwr10r_brt.rds")
rm(fits_aruwiwr10r_brt, rmse_aruwiwr10r_brt)
## end BRT for wiwr per day- ARU 10 random min----------------------------------

## BRT for WIWR per day- ARU 22 random min--------------------------------------
fits_aruwiwr22r_brt <- list()
for (i in 1:200) {
  # assign days to 3-day blocks
  days <- data.frame(day = min(sum_aruwiwr22r$day_of_yr):
                       max(sum_aruwiwr22r$day_of_yr), 
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
  pwiwr_day <- data.frame(day_of_yr = seq(min(sum_aruwiwr22r$day_of_yr), 
                                          max(sum_aruwiwr22r$day_of_yr), by = 1))
  pwiwr_day$day_of_yr_c <- pwiwr_day$day_of_yr-mean(pwiwr_day$day_of_yr)
  pwiwr_day$wind <- mean(sum_aruwiwr22r$wind)
  pwiwr_day <- left_join(pwiwr_day, days, by = c("day_of_yr" = "day"))
  
  # join CV fold info onto bird data
  bird_dat <- left_join(sum_aruwiwr22r, days, by = c("day_of_yr" = "day"))
  
  # fit brt to data in CV folds
  brt_test_folds <- unique(bird_dat$fold)
  names(brt_test_folds) <- as.character(brt_test_folds) 
  
  brt_test_folds <- lapply(brt_test_folds, fit_brt, sp_data = bird_dat, 
                           newdata = pwiwr_day, nt = nt)
  
  # put predictions for these 5 folds into the big list for all splits
  fits_aruwiwr22r_brt[[i]] <- brt_test_folds
  rm(brt_test_folds)
}
names(fits_aruwiwr22r_brt) <- 1:length(fits_aruwiwr22r_brt)
rmse_aruwiwr22r_brt <- sapply(fits_aruwiwr22r_brt, FUN = function(x) {
  sapply(x, FUN = function(z) {
    sqrt(mean(z$test_predictions$error^2))
  })
})
saveRDS(fits_aruwiwr22r_brt, "fits_aruwiwr22r_brt.rds")
saveRDS(rmse_aruwiwr22r_brt, "rmse_aruwiwr22r_brt.rds")
rm(fits_aruwiwr22r_brt, rmse_aruwiwr22r_brt)
}
## end BRT for wiwr per day- ARU 22 random min----------------------------------

