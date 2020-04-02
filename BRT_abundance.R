################################
## Abundance estimation: Boosted Regression Trees
## Point Abbaye 2019 data
## 
## author: Ellie Roark, Willson Gaul
## created: 6 Mar 2020
## last modified: 20 Mar 2020
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

set.seed(28022020) # 28 Feb 2020
#set number of trees for BRT models
nt = 2000

## Exploratory plots -----------------------------------------------------------
# #hist(ptct$count, main = "number of individs")
# 
# 
# # plot wren counts over time
# plot(wiwr$day_of_yr, wiwr$count, 
#      main = "wiwr per pt count over time")
# 
# plot(gcki$day_of_yr, gcki$count, 
#      main = "gcki per pt count over time")
# 
# 
# 
# 
# plot(sum_gcki$day_of_yr, sum_gcki$count,
#      main = "gcki per day over time")
# ggplot(data = sum_gcki, aes(x = day_of_yr, y = count)) + 
#   geom_point() + 
#   geom_smooth()
# 
# plot(sum_wiwr$day_of_yr, sum_wiwr$count,
#      main = "wiwr per day over time")
# ggplot(data = sum_wiwr, aes(x = day_of_yr, y = count)) + 
#   geom_point() + 
#   geom_smooth()


## end Exploratory plots -------------------------------------------------------

## GLM for GCKI counts over time------------------------------------------------
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

## end GLM for GCKI counts------------------------------------------------------

## GLM for GCKI **per day** counts----------------------------------------------
## model of GCKI ** per day ** over time
# gcki.day <- glm(count ~ 1 + wind + day_of_yr_c + day_sq,
#                 data = sum_gcki, 
#                 family = "poisson")
# 
# summary(gcki.day)
# #plot(gcki.day)
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
# gdaycount <-  table(sum_gcki$count)
# barplot(gdaycount, main = "distribution of # of gcki per day", xlab = "count", 
#         ylab = "frequency")
# 
# # check unconditional mean and variance for sp_detected (response variable)
# # (we ultimately care only about CONDITIONAL mean and variance being equal after
# # model is fit but this is a good indicator of whether it might be a problem)
# mean(sum_gcki$count)
# var(sum_gcki$count)
# # doesn't look great! overdispersion may be a problem here.
# 
# #look at deviance statistic of fit model divided by its d.f. to see if ratio
# # is over 1
# gcki.day$deviance
# gcki.day$df.residual
# #(this is the ratio we care about)
# with(gcki.day, deviance/df.residual)
# #(this gives us a p-value for that ratio)
# with(gcki.day, pchisq(deviance, df.residual, lower.tail = FALSE))
# ## this is TERRIBLE-- ratio is 2.4, definitely different from 1!!! 
# ## Poisson assumptions pretty definitively not met. 
# 

# test neg binom model for GCKI per day to see if it helps with overdispersion
gcki.day.nb <- glm.nb(count ~ 1 + wind + day_of_yr_c + day_sq,
                      data = sum_gcki)

#get fitted values
sum_gcki$nbfv <- predict(gcki.day.nb, type="response")

plot(sum_gcki$count ~ sum_gcki$nbfv)
abline(0, 1)

td <- data.frame(simulate(gcki.day.nb, nsim = 10))
td$obs <- sum_gcki$count
td$doy <- sum_gcki$day_of_yr
td <- pivot_longer(td, 1:(ncol(td)-2), names_to = "case", values_to = "n_sp")

ggplot(data = td, aes(x = n_sp, y = obs)) +
  geom_point() +
  geom_jitter() +
  geom_smooth()

ggplot() +
  geom_point(data = td, aes(x = doy, y = n_sp), alpha = 0.1) +
  geom_point(data = td[td$case == "obs", ], aes(x = doy, y = n_sp),
             col = "red")

#look at deviance statistic of fit model divided by its d.f. to see if ratio
# is over 1
gcki.day.nb$deviance
gcki.day.nb$df.residual
#(this is the ratio we care about)
with(gcki.day.nb, deviance/df.residual)
#(this gives us a p-value for that ratio)
with(gcki.day.nb, pchisq(deviance, df.residual, lower.tail = FALSE))
## this is way better than poisson-- ratio is 1.09, pvalue is .3-- not sig.
## different from 1.

## end GLM for GCKI per day-----------------------------------------------------

## Boosted Regression Tree for GCKI per COUNT-----------------------------------

gcki.brt <- gbm(count ~ 1 + wind + rain + noise + day_of_yr_c + cloud_cover +
                  day_sq + min_past_sun, 
                distribution = "poisson", 
                data = gcki, 
                interaction.depth = 3, 
                n.trees = nt, 
                n.minobsinnode = 5, 
                shrinkage = 0.01, 
                bag.fraction = 0.8)

## end BRT for GCKI per COUNT model---------------------------------------------

## predictions with BRT **per count model***-----------------------------------
# create new data to predict with
pred_gcki <- data.frame(day_of_yr = seq(min(gcki$day_of_yr), 
                                        max(gcki$day_of_yr), by = 1))
pred_gcki$day_of_yr_c <- pred_gcki$day_of_yr-mean(pred_gcki$day_of_yr)
pred_gcki$day_sq <- pred_gcki$day_of_yr_c^2
pred_gcki$min_past_sun <- median(gcki$min_past_sun)
pred_gcki$wind <- as.factor("0-1")
pred_gcki$rain <- as.factor("Dry")
pred_gcki$noise <- as.factor("0")
pred_gcki$cloud_cover <- as.factor("0-33")
#coerce factor variables to contain the same number of levels as the original 
pred_gcki$wind <- factor(pred_gcki$wind, 
                         levels = c("0-1", "2", "3+"),
                         labels = c("0-1", "2", "3+"))
pred_gcki$rain <- factor(pred_gcki$rain, 
                         levels = c("Dry", "wet"),
                         labels = c("Dry", "wet"))
pred_gcki$noise <- factor(pred_gcki$noise, 
                          levels = c("0", "1", ">2"), 
                          labels = c("0", "1", ">2"))
pred_gcki$cloud_cover <- factor(pred_gcki$cloud_cover, 
                                levels = c("0-33", "33-66", "66-100"), 
                                labels = c("0-33", "33-66", "66-100"))

# get predictions with new data
pred_gcki$p1 <- predict(gcki.brt, newdata = pred_gcki, n.trees = nt, 
                        type = "response")

#plot predictions over time 
predgcki_time <- ggplot(pred_gcki, aes(x=day_of_yr, y=p1)) + 
  geom_line() + 
  geom_point(data = gcki,
             aes(x = day_of_yr, y = count)) +
  theme_bw() +
  scale_colour_viridis_d() + 
  scale_y_continuous() + 
  ylab("Number of GCKI") +
  xlab("Day of Year")
predgcki_time
## end predictions with BRT **GCKI per count model**----------------------------

## Boosted Regression Tree for GCKI per DAY-----------------------------------

# assign days to 3-day blocks
days <- data.frame(day = min(sum_gcki$day_of_yr):max(sum_gcki$day_of_yr), 
                   block = NA)
n_blocks <- nrow(days)/3
blocks <- rep(1:n_blocks, 3)
blocks <- blocks[order(blocks)]
start_row <- sample(1:nrow(days), size = 1)
days$block[start_row:nrow(days)] <- blocks[1:length(start_row:nrow(days))]
days$block[1:(start_row - 1)] <- blocks[(length(start_row:nrow(days)) + 1):
                                          nrow(days)]

# assign blocks to CV folds
fold_assignments <- data.frame(
  block = unique(days$block), 
  fold = sample(rep_len(1:5, length.out = length(unique(days$block)))))
days <- left_join(days, fold_assignments, by = "block")
rm(blocks, n_blocks, start_row, fold_assignments)

# join CV fold info onto bird data
sum_gcki <- left_join(sum_gcki, days, by = c("day_of_yr" = "day"))

# fit brt to data in CV folds
brt_test_folds <- unique(sum_gcki$fold)
names(brt_test_folds) <- as.character(brt_test_folds)

fit_brt <- function(test_fold, sp_data) {
  train_dat <- sp_data[sp_data$fold != test_fold, ]
  f_m <- gbm(count ~ 1 + wind + day_of_yr_c, 
             distribution = "poisson", 
             data = train_dat, 
             interaction.depth = 1, 
             n.trees = nt, 
             n.minobsinnode = 1, 
             shrinkage = 0.001, 
             bag.fraction = 0.8)
  test_pred <- sp_data[sp_data$fold == test_fold, ]
  test_pred$OOB_preds <- predict(f_m, newdata = test_pred, 
                                 n.trees = nt, type = "response")
  list(mod = f_m, test_predictions = test_pred)
}

brt_test_folds <- lapply(brt_test_folds, fit_brt, sp_data = sum_gcki)

## alternate BRT with interaction depth of 2 instead of 1 
gcki.brt2 <- gbm(count ~ 1 + wind + day_of_yr_c, 
           distribution = "poisson", 
           data = sum_gcki, 
           interaction.depth = 2, 
           n.trees = nt, 
           n.minobsinnode = 1, 
           shrinkage = 0.001, 
           bag.fraction = 0.8)

## end BRT for GCKI per DAY model---------------------------------------------


## evaluate BRT ----------------------------------------------------------------
## BRT with interaction depth 1
# get predictions to test data
gcki_brt_predictions <- bind_rows(lapply(brt_test_folds, 
                                         FUN = function(x) x$test_predictions))
gcki_brt_predictions$error = gcki_brt_predictions$OOB_preds - gcki_brt_predictions$count

# calculate r^2 (square of Pearson correlation coefficient, see Bahn & 
# McGill 2013)
gcki_brt_r2 <- cor(gcki_brt_predictions$day_of_yr, gcki_brt_predictions$OOB_preds, 
                   method = "pearson")^2
# calculate R^2 (coefficient of determination, see Bahn & McGill 2013)
gcki_brt_R2 <- 1 - (sum(gcki_brt_predictions$error^2) / 
                      (sum((gcki_brt_predictions$count - 
                              mean(gcki_brt_predictions$count))^2)))

## BRT with interaction depth 2
# get predictions to test data
gcki_brt2_predictions <- sum_gcki
gcki_brt2_predictions$fv <- predict(gcki.brt2, n.trees = nt, type = "response")

gcki_brt2_predictions$error = gcki_brt2_predictions$fv -
  gcki_brt_predictions$count

# calculate r^2 (square of Pearson correlation coefficient, see Bahn & 
# McGill 2013)
gcki_brt2_r2 <- cor(gcki_brt2_predictions$day_of_yr, gcki_brt2_predictions$fv, 
                   method = "pearson")^2
# calculate R^2 (coefficient of determination, see Bahn & McGill 2013)
gcki_brt2_R2 <- 1 - (sum(gcki_brt2_predictions$error^2) / 
                      (sum((gcki_brt2_predictions$count - 
                              mean(gcki_brt2_predictions$count))^2)))

## end evaluate BRT ------------------------------------------------------------

## predictions with BRT **per DAY model***-----------------------------------
# create new data to predict with
pgcki_day <- data.frame(day_of_yr = seq(min(sum_gcki$day_of_yr), 
                                        max(sum_gcki$day_of_yr), by = 1))
pgcki_day$day_of_yr_c <- pgcki_day$day_of_yr-mean(pgcki_day$day_of_yr)
pgcki_day$wind <- mean(sum_gcki$wind)
pgcki_day <- left_join(pgcki_day, days, by = c("day_of_yr" = "day"))

# get predictions with new data.  Predict to days using the model for which 
# those days were not in the training set
gcki_preds_newdata <- bind_rows(
  lapply(1:length(brt_test_folds), 
         FUN = function(x, newdata, brts) {
           test_data <- newdata[newdata$fold == x, ]
           test_data$predictions <- predict(brts[[x]]$mod, 
                                            newdata = test_data, 
                                            n.trees = nt, 
                                            type = "response")
           test_data}, 
         newdata = pgcki_day, brts = brt_test_folds))

#plot predictions over time 
pgcki_day_time <- ggplot(gcki_preds_newdata, aes(x=day_of_yr, y=predictions)) + 
  geom_line() + 
  geom_point(data = sum_gcki,
             aes(x = day_of_yr, y = count)) +
  theme_bw() +
  scale_colour_viridis_d() + 
  scale_y_continuous() + 
  ylab("Number of GCKI per day") +
  xlab("Day of Year")
pgcki_day_time

# plot predicted vs. observed values
ggplot(data = gcki_brt_predictions, aes(x = count, y = OOB_preds)) + 
  geom_point() + 
  geom_smooth() + 
  geom_abline(intercept = 0, slope = 1) + 
  ggtitle("BRT predicted vs. observed") + 
  ylim(c(0, 9))
## end predictions with BRT **GCKI per DAY model**----------------------------

################################################################################
## GLM for GCKI **per day** counts with ARUs------------------------------------
# model of GCKI ** per day ** over time with ARU data
arugcki.day <- glm(count ~ 1 + wind + day_of_yr_c + day_sq,
                data = sum_arugcki,
                family = "poisson")

summary(arugcki.day)
#plot(arugcki.day)

vif(arugcki.day)
aresids <- rstandard(arugcki.day)

p.day.aresids <- residuals(arugcki.day, type = "deviance")
sum_arugcki$pois_devresids <- p.day.aresids

sum_arugcki[sum_arugcki$pois_devresids > 2, ]

hist(p.day.aresids, main = "Deviance Residuals for gcki (day, aru) model", 
     xlab = "residuals")
boxplot(p.day.aresids, main = "Deviance residuals for gcki (day, aru) model", 
        ylab ="residuals")


arugdaycount <-  table(sum_arugcki$count)
barplot(arugdaycount, main = "distribution of # of gcki per day", xlab = "count",
        ylab = "frequency")

# check unconditional mean and variance for sp_detected (response variable)
# (we ultimately care only about CONDITIONAL mean and variance being equal after
# model is fit but this is a good indicator of whether it might be a problem)
mean(sum_arugcki$count)
var(sum_arugcki$count)
# looks real bad! suspect overdispersion.

#look at deviance statistic of fit model divided by its d.f. to see if ratio
# is over 1
arugcki.day$deviance
arugcki.day$df.residual
#(this is the ratio we care about)
with(arugcki.day, deviance/df.residual)
#(this gives us a p-value for that ratio)
with(arugcki.day, pchisq(deviance, df.residual, lower.tail = FALSE))
## this is truly just not poisson. way, way overdispersed. 
## -- ratio is 6.38 (p <0.001), definitely different from 1!!!

# test neg binom model for GCKI per day (aru) to see if it helps with 
# overdispersion
arugcki.day.nb <- glm.nb(count ~ 1 + wind + day_of_yr_c + day_sq,
                      data = sum_arugcki, control = glm.control(maxit = 1000))
## this does not converge

#get fitted values
sum_arugcki$nbfv <- predict(arugcki.day.nb, type="response")

plot(sum_arugcki$count ~ sum_arugcki$nbfv)
abline(0, 1)

# TODO what does this section of code do?? ask wg
gtd <- data.frame(simulate(arugcki.day.nb, nsim = 10))
gtd$obs <- sum_arugcki$count
gtd$doy <- sum_arugcki$day_of_yr
gtd <- pivot_longer(gtd, 1:(ncol(gtd)-2), names_to = "case", values_to = "n_sp")

ggplot(data = gtd, aes(x = n_sp, y = obs)) +
  geom_point() +
  geom_jitter() +
  geom_smooth()

ggplot() +
  geom_point(data = gtd, aes(x = doy, y = n_sp), alpha = 0.1) +
  geom_point(data = gtd[gtd$case == "obs", ], aes(x = doy, y = n_sp),
             col = "red")

#look at deviance statistic of fit model divided by its d.f. to see if ratio
# is over 1
arugcki.day.nb$deviance
arugcki.day.nb$df.residual
#(this is the ratio we care about)
with(arugcki.day.nb, deviance/df.residual)
#(this gives us a p-value for that ratio)
with(arugcki.day.nb, pchisq(deviance, df.residual, lower.tail = FALSE))
## this is way better than poisson-- ratio is .78, pvalue is .79-- not sig.
## different from 1.

## end GLM for GCKI per day (ARU)-----------------------------------------------

## Boosted Regression Tree for GCKI per DAY (ARU)-------------------------------

# assign days to 3-day blocks
# assign blocks to CV folds --> will use "days" dataframe from previous model 

# join CV fold info onto bird data
sum_arugcki <- left_join(sum_arugcki, days, by = c("day_of_yr" = "day"))

# fit brt to data in CV folds
brt_test_folds <- unique(sum_arugcki$fold)
names(brt_test_folds) <- as.character(brt_test_folds)

fit_arubrt <- function(test_fold, sp_data) {
  atrain_dat <- sp_data[sp_data$fold != test_fold, ]
  f_ma <- gbm(count ~ 1 + wind + day_of_yr_c, 
             distribution = "poisson", 
             data = atrain_dat, 
             interaction.depth = 1, 
             n.trees = nt, 
             n.minobsinnode = 1, 
             shrinkage = 0.001, 
             bag.fraction = 0.8)
  atest_pred <- sp_data[sp_data$fold == test_fold, ]
  atest_pred$OOB_preds <- predict(f_ma, newdata = atest_pred, 
                                 n.trees = nt, type = "response")
  list(mod = f_ma, test_predictions = atest_pred)
}

brt_test_folds <- lapply(brt_test_folds, fit_arubrt, sp_data = sum_arugcki)

## end BRT for GCKI per DAY model (ARU)-----------------------------------------


## evaluate BRT (ARU)-----------------------------------------------------------
## BRT with interaction depth 1
# get predictions to test data
arugcki_brt_predictions <- bind_rows(lapply(brt_test_folds, 
                                         FUN = function(x) x$test_predictions))
arugcki_brt_predictions$error = arugcki_brt_predictions$OOB_preds - 
  arugcki_brt_predictions$count

# calculate r^2 (square of Pearson correlation coefficient, see Bahn & 
# McGill 2013)
arugcki_brt_r2 <- cor(arugcki_brt_predictions$day_of_yr, 
                      arugcki_brt_predictions$OOB_preds, 
                   method = "pearson")^2
# calculate R^2 (coefficient of determination, see Bahn & McGill 2013)
arugcki_brt_R2 <- 1 - (sum(arugcki_brt_predictions$error^2) / 
                      (sum((arugcki_brt_predictions$count - 
                              mean(arugcki_brt_predictions$count))^2)))

## end evaluate BRT (ARU)-------------------------------------------------------

## predictions with BRT **per DAY model*** (ARU)--------------------------------
# create new data to predict with
parugcki_day <- data.frame(day_of_yr = seq(min(sum_arugcki$day_of_yr), 
                                        max(sum_arugcki$day_of_yr), by = 1))
parugcki_day$day_of_yr_c <- parugcki_day$day_of_yr-mean(parugcki_day$day_of_yr)
parugcki_day$wind <- mean(sum_arugcki$wind)
parugcki_day <- left_join(parugcki_day, days, by = c("day_of_yr" = "day"))

# get predictions with new data.  Predict to days using the model for which 
# those days were not in the training set
arugcki_preds_newdata <- bind_rows(
  lapply(1:length(brt_test_folds), 
         FUN = function(x, newdata, brts) {
           test_data <- newdata[newdata$fold == x, ]
           test_data$predictions <- predict(brts[[x]]$mod, 
                                            newdata = test_data, 
                                            n.trees = nt, 
                                            type = "response")
           test_data}, 
         newdata = parugcki_day, brts = brt_test_folds))

#plot predictions over time 
parugcki_day_time <- ggplot(arugcki_preds_newdata, aes(x=day_of_yr, 
                                                       y=predictions)) + 
  geom_line() + 
  geom_point(data = sum_arugcki,
             aes(x = day_of_yr, y = count)) +
  theme_bw() +
  scale_colour_viridis_d() + 
  scale_y_continuous() + 
  ylab("Number of GCKI per day (ARU)") +
  xlab("Day of Year")
parugcki_day_time

  # plot predicted vs. observed values
ggplot(data = arugcki_brt_predictions, aes(x = count, y = OOB_preds)) + 
  geom_point() + 
  geom_smooth() + 
  geom_abline(intercept = 0, slope = 1) + 
  ggtitle("BRT predicted vs. observed (ARU)") + 
  ylim(c(0, 9))
## end predictions with BRT **GCKI per DAY model** (ARU)------------------------


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

## GLM for WIWR **per day** counts----------------------------------------------
# model of WIWR ** per day ** over time
wiwr.day <- glm(count ~ 1 + wind + day_of_yr_c + day_sq,
                data = sum_wiwr,
                family = "poisson")

summary(wiwr.day)
#plot(gcki.day)

vif(wiwr.day)
resids <- rstandard(wiwr.day)

p.wday.resids <- residuals(wiwr.day, type = "deviance")
sum_wiwr$pois_devresids <- p.wday.resids

sum_wiwr[sum_wiwr$pois_devresids > 2, ]

hist(p.wday.resids, main = "Deviance Residuals for wiwr (day) model", xlab =
             "residuals")
boxplot(p.wday.resids, main = "Deviance residuals for wiwr (day) model", ylab =
                "residuals")


wdaycount <-  table(sum_wiwr$count)
barplot(wdaycount, main = "distribution of # of wiwr per day", xlab = "count",
        ylab = "frequency")

# check unconditional mean and variance for sp_detected (response variable)
# (we ultimately care only about CONDITIONAL mean and variance being equal after
# model is fit but this is a good indicator of whether it might be a problem)
mean(sum_wiwr$count)
var(sum_wiwr$count)
# 2.2/4.6-- doesn't look great! overdispersion may be a problem here.

#look at deviance statistic of fit model divided by its d.f. to see if ratio
# is over 1
wiwr.day$deviance
wiwr.day$df.residual
#(this is the ratio we care about)
with(wiwr.day, deviance/df.residual)
#(this gives us a p-value for that ratio)
with(wiwr.day, pchisq(deviance, df.residual, lower.tail = FALSE))
## this actually looks fine-- ratio is 1.1; p = .31, indicating that this is not
## sig different from 1 and therefore not overdispersed.

## end GLM for GCKI per day-----------------------------------------------------


## Boosted Regression Tree for WIWR per DAY (ptct)-------------------------------

# assign days to 3-day blocks
# assign blocks to CV folds --> will use "days" dataframe from previous model 

# join CV fold info onto bird data
sum_wiwr <- left_join(sum_wiwr, days, by = c("day_of_yr" = "day"))

# fit brt to data in CV folds
brt_test_folds <- unique(sum_wiwr$fold)
names(brt_test_folds) <- as.character(brt_test_folds)

fit_wbrt <- function(test_fold, sp_data) {
  wtrain_dat <- sp_data[sp_data$fold != test_fold, ]
  f_mw <- gbm(count ~ 1 + wind + day_of_yr_c, 
              distribution = "poisson", 
              data = wtrain_dat, 
              interaction.depth = 1, 
              n.trees = nt, 
              n.minobsinnode = 1, 
              shrinkage = 0.001, 
              bag.fraction = 0.8)
  wtest_pred <- sp_data[sp_data$fold == test_fold, ]
  wtest_pred$OOB_preds <- predict(f_mw, newdata = wtest_pred, 
                                  n.trees = nt, type = "response")
  list(mod = f_mw, test_predictions = wtest_pred)
}

brt_test_folds <- lapply(brt_test_folds, fit_wbrt, sp_data = sum_wiwr)

## end BRT for WIWR per DAY model (ptct)----------------------------------------


## evaluate WIWR BRT (ptct) ---------------------------------------------------------
## BRT with interaction depth 1
# get predictions to test data
wiwr_brt_predictions <- bind_rows(lapply(brt_test_folds, 
                                            FUN = function(x) x$test_predictions))
wiwr_brt_predictions$error = wiwr_brt_predictions$OOB_preds - 
  wiwr_brt_predictions$count

# calculate r^2 (square of Pearson correlation coefficient, see Bahn & 
# McGill 2013)
wiwr_brt_r2 <- cor(wiwr_brt_predictions$day_of_yr, 
                      wiwr_brt_predictions$OOB_preds, 
                      method = "pearson")^2
# calculate R^2 (coefficient of determination, see Bahn & McGill 2013)
wiwr_brt_R2 <- 1 - (sum(wiwr_brt_predictions$error^2) / 
                         (sum((wiwr_brt_predictions$count - 
                                 mean(wiwr_brt_predictions$count))^2)))

## end evaluate WIWR BRT (ptct) -----------------------------------------------------

## predictions with WIWR BRT **per DAY model*** (ptct)-------------------------------
# create new data to predict with
pwiwr_day <- data.frame(day_of_yr = seq(min(sum_wiwr$day_of_yr), 
                                           max(sum_wiwr$day_of_yr), by = 1))
pwiwr_day$day_of_yr_c <- pwiwr_day$day_of_yr-mean(pwiwr_day$day_of_yr)
pwiwr_day$wind <- mean(sum_wiwr$wind)
pwiwr_day <- left_join(pwiwr_day, days, by = c("day_of_yr" = "day"))

# get predictions with new data.  Predict to days using the model for which 
# those days were not in the training set
wiwr_preds_newdata <- bind_rows(
  lapply(1:length(brt_test_folds), 
         FUN = function(x, newdata, brts) {
           test_data <- newdata[newdata$fold == x, ]
           test_data$predictions <- predict(brts[[x]]$mod, 
                                            newdata = test_data, 
                                            n.trees = nt, 
                                            type = "response")
           test_data}, 
         newdata = pwiwr_day, brts = brt_test_folds))

#plot predictions over time 
pwiwr_day_time <- ggplot(wiwr_preds_newdata, aes(x=day_of_yr, 
                                                       y=predictions)) + 
  geom_line() + 
  geom_point(data = sum_wiwr,
             aes(x = day_of_yr, y = count)) +
  theme_bw() +
  scale_colour_viridis_d() + 
  scale_y_continuous() + 
  ylab("Number of WIWR per day") +
  xlab("Day of Year")
pwiwr_day_time

# plot predicted vs. observed values
ggplot(data = wiwr_brt_predictions, aes(x = count, y = OOB_preds)) + 
  geom_point() + 
  geom_smooth() + 
  geom_abline(intercept = 0, slope = 1) + 
  ggtitle("WIWR- BRT predicted vs. observed (ptct)") + 
  ylim(c(0, 9))
## end predictions with WIWR BRT **WIWR per DAY model** (ptct)-----------------------

## Boosted Regression Tree for WIWR per DAY (ARU)-------------------------------

# assign days to 3-day blocks
# assign blocks to CV folds --> will use "days" dataframe from previous model 

# join CV fold info onto bird data
sum_aruwiwr <- left_join(sum_aruwiwr, days, by = c("day_of_yr" = "day"))

# fit brt to data in CV folds
brt_test_folds <- unique(sum_aruwiwr$fold)
names(brt_test_folds) <- as.character(brt_test_folds)

fit_awbrt <- function(test_fold, sp_data) {
  awtrain_dat <- sp_data[sp_data$fold != test_fold, ]
  f_maw <- gbm(count ~ 1 + wind + day_of_yr_c, 
              distribution = "poisson", 
              data = awtrain_dat, 
              interaction.depth = 1, 
              n.trees = nt, 
              n.minobsinnode = 1, 
              shrinkage = 0.001, 
              bag.fraction = 0.8)
  awtest_pred <- sp_data[sp_data$fold == test_fold, ]
  awtest_pred$OOB_preds <- predict(f_maw, newdata = awtest_pred, 
                                  n.trees = nt, type = "response")
  list(mod = f_maw, test_predictions = awtest_pred)
}

brt_test_folds <- lapply(brt_test_folds, fit_awbrt, sp_data = sum_aruwiwr)

## end BRT for WIWR per DAY model (ARU)----------------------------------------


## evaluate WIWR BRT (ARU) ---------------------------------------------------------
## BRT with interaction depth 1
# get predictions to test data
aruwiwr_brt_predictions <- bind_rows(lapply(brt_test_folds, 
                                         FUN = function(x) x$test_predictions))
aruwiwr_brt_predictions$error = aruwiwr_brt_predictions$OOB_preds - 
  aruwiwr_brt_predictions$count

# calculate r^2 (square of Pearson correlation coefficient, see Bahn & 
# McGill 2013)
aruwiwr_brt_r2 <- cor(aruwiwr_brt_predictions$day_of_yr, 
                   aruwiwr_brt_predictions$OOB_preds, 
                   method = "pearson")^2
# calculate R^2 (coefficient of determination, see Bahn & McGill 2013)
aruwiwr_brt_R2 <- 1 - (sum(aruwiwr_brt_predictions$error^2) / 
                      (sum((aruwiwr_brt_predictions$count - 
                              mean(aruwiwr_brt_predictions$count))^2)))

## end evaluate WIWR BRT (ARU) -----------------------------------------------------

## predictions with WIWR BRT **per DAY model*** (ARU)-------------------------------
# create new data to predict with
paruwiwr_day <- data.frame(day_of_yr = seq(min(sum_aruwiwr$day_of_yr), 
                                        max(sum_aruwiwr$day_of_yr), by = 1))
paruwiwr_day$day_of_yr_c <- paruwiwr_day$day_of_yr-mean(paruwiwr_day$day_of_yr)
paruwiwr_day$wind <- mean(sum_aruwiwr$wind)
paruwiwr_day <- left_join(paruwiwr_day, days, by = c("day_of_yr" = "day"))

# get predictions with new data.  Predict to days using the model for which 
# those days were not in the training set
aruwiwr_preds_newdata <- bind_rows(
  lapply(1:length(brt_test_folds), 
         FUN = function(x, newdata, brts) {
           test_data <- newdata[newdata$fold == x, ]
           test_data$predictions <- predict(brts[[x]]$mod, 
                                            newdata = test_data, 
                                            n.trees = nt, 
                                            type = "response")
           test_data}, 
         newdata = paruwiwr_day, brts = brt_test_folds))

#plot predictions over time 
paruwiwr_day_time <- ggplot(aruwiwr_preds_newdata, aes(x=day_of_yr, 
                                                 y=predictions)) + 
  geom_line() + 
  geom_point(data = sum_aruwiwr,
             aes(x = day_of_yr, y = count)) +
  theme_bw() +
  scale_colour_viridis_d() + 
  scale_y_continuous() + 
  ylab("Number of WIWR per day (ARU)") +
  xlab("Day of Year")
paruwiwr_day_time

# plot predicted vs. observed values
ggplot(data = aruwiwr_brt_predictions, aes(x = count, y = OOB_preds)) + 
  geom_point() + 
  geom_smooth() + 
  geom_abline(intercept = 0, slope = 1) + 
  ggtitle("WIWR- BRT predicted vs. observed (ARU)") 

## end predictions with WIWR BRT **WIWR per DAY model** (ptct)-----------------------
