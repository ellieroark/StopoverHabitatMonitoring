################################
## Abundance estimation: Boosted Regression Trees
## Point Abbaye 2019 data
## 
## author: Ellie Roark, Willson Gaul
## created: 6 Mar 2020
## last modified: 6 Feb 2021 (wg)
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

plotson <- FALSE
fitbrt <- FALSE # fit models for GCKI and WIWR for all 4 count types
fitbrt_allSp_ap <- FALSE # fit models for all species using point count data
fitbrt_allSp_a30c <- T # fit models for all species using A30c data
fitbrt_allSp_a30r <- F # fit models for all species using A30R data
fitbrt_allSp_a66r <- FALSE # fit models for all species using A66R data
n_cores <- 4

# make a df to hold the shrinkage rate and number of trees to use for each
# species for both point count and ARU66r models.  The shrinkage rate and
# n trees were chosen manually by looking at graphs of error rate based on the
# preliminary models fitted around line 820 of this script, and the graphs from 
# about line 943 in "suppmaterials_plots_yellowpaper.R"
# list species detected on both point counts and ARUs
sp_detected_on_both <- table(
  c(names(sum_species_dfs),
    names(sum_aru_dfs)))[order(table(c(names(sum_species_dfs), 
                                       names(sum_aru_dfs))))]
sp_detected_on_both <- names(sp_detected_on_both[sp_detected_on_both == 2])
sp_detected_on_both <- sp_detected_on_both[sp_detected_on_both != "CANG"]

set.seed(16011449) # set this to something new before running each small species batch
brt_params <- list(
  AMCR = list(pt_ct = c(sr = 0.0001, nt = 3000), aru66r = c(sr = 0.0001, nt = 3000),
              aru30r = c(sr = 0.0001, nt = 3000), aru30c = c(sr = 0.0001, nt = 3000)),
  AMGO = list(pt_ct = c(sr = 0.0001, nt = 3000), aru66r = c(sr = 0.0001, nt = 3000),
              aru30r = c(sr = 0.0001, nt = 3000), aru30c = c(sr = 0.0001, nt = 3000)),
  AMRE = list(pt_ct = c(sr = 0.0005, nt = 2000), aru66r = c(sr = 0.0002, nt = 3000),
              aru30r = c(sr = 0.0001, nt = 2500), aru30c = c(sr = 0.0001, nt = 3000)),
  AMRO = list(pt_ct = c(sr = 0.0005, nt = 2000), aru66r = c(sr = 0.0001, nt = 3000),
              aru30r = c(sr = 0.0006, nt = 4000), aru30c = c(sr = 0.0001, nt = 3000)),
  BAEA = list(pt_ct = c(sr = 0.0001, nt = 3000), aru66r = c(sr = 0.0001, nt = 3000),
              aru30r = c(sr = 0.0001, nt = 3000), aru30c = c(sr = 0.0001, nt = 3000)),
  BAWW = list(pt_ct = c(sr = 0.0001, nt = 2000), aru66r = c(sr = 0.0001, nt = 3000),
              aru30r = c(sr = 0.0001, nt = 3000), aru30c = c(sr = 0.0001, nt = 3000)),
  BCCH = list(pt_ct = c(sr = 0.002, nt = 2000), aru66r = c(sr = 0.001, nt = 3000),
              aru30r = c(sr = 0.0005, nt = 3000), aru30c = c(sr = 0.0001, nt = 3000)),
  BHVI = list(pt_ct = c(sr = 0.0001, nt = 3000), aru66r = c(sr = 0.0001, nt = 3000),
              aru30r = c(sr = 0.0001, nt = 3000), aru30c = c(sr = 0.0001, nt = 3000)),
  BLJA = list(pt_ct = c(sr = 0.0005, nt = 2500), aru66r = c(sr = 0.0002, nt = 2500),
              aru30r = c(sr = 0.0001, nt = 3000), aru30c = c(sr = 0.0001, nt = 2000)),
  BRCR = list(pt_ct = c(sr = 0.0005, nt = 2500), aru66r = c(sr = 0.0002, nt = 3000),
              aru30r = c(sr = 0.0005, nt = 2000), aru30c = c(sr = 0.0001, nt = 3000)),
  BTNW = list(pt_ct = c(sr = 0.0005, nt = 2500), aru66r = c(sr = 0.0005, nt = 2500),
              aru30r = c(sr = 0.0005, nt = 3000), aru30c = c(sr = 0.0001, nt = 3000)),
  COGR = list(pt_ct = c(sr = 0.0001, nt = 3000), aru66r = c(sr = 0.0005, nt = 3000),
              aru30r = c(sr = 0.0001, nt = 3000), aru30c = c(sr = 0.0001, nt = 3000)),
  CORA = list(pt_ct = c(sr = 0.0001, nt = 3000), aru66r = c(sr = 0.0001, nt = 3000),
              aru30r = c(sr = 0.0005, nt = 3000), aru30c = c(sr = 0.0001, nt = 3000)),
  DEJU = list(pt_ct = c(sr = 0.0001, nt = 3000), aru66r = c(sr = 0.0001, nt = 3000),
              aru30r = c(sr = 0.0001, nt = 3000), aru30c = c(sr = 0.0001, nt = 3000)),
  DOWO = list(pt_ct = c(sr = 0.0001, nt = 3000), aru66r = c(sr = 0.0001, nt = 3000),
              aru30r = c(sr = 0.0001, nt = 3000), aru30c = c(sr = 0.0001, nt = 3000)),
  GCKI = list(pt_ct = c(sr = 0.0005, nt = 2000), aru66r = c(sr = 0.0005, nt = 2000),
              aru30r = c(sr = 0.0001, nt = 3000), aru30c = c(sr = 0.0001, nt = 3000)),
  HAWO = list(pt_ct = c(sr = 0.0001, nt = 6000), aru66r = c(sr = 0.0001, nt = 3000),
              aru30r = c(sr = 0.0001, nt = 3000), aru30c = c(sr = 0.0001, nt = 3000)),
  HETH = list(pt_ct = c(sr = 0.0005, nt = 2000), aru66r = c(sr = 0.0005, nt = 2000),
              aru30r = c(sr = 0.0001, nt = 3000), aru30c = c(sr = 0.00003, nt = 2000)),
  LEFL = list(pt_ct = c(sr = 0.0001, nt = 3000), aru66r = c(sr = 0.0001, nt = 3000),
              aru30r = c(sr = 0.0001, nt = 3000), aru30c = c(sr = 0.0001, nt = 3000)),
  MERL = list(pt_ct = c(sr = 0.0001, nt = 3000), aru66r = c(sr = 0.0005, nt = 3000),
              aru30r = c(sr = 0.0001, nt = 3000), aru30c = c(sr = 0.0001, nt = 3000)),
  NAWA = list(pt_ct = c(sr = 0.0001, nt = 2000), aru66r = c(sr = 0.0001, nt = 3000),
              aru30r = c(sr = 0.0001, nt = 3000), aru30c = c(sr = 0.0001, nt = 3000)),
  NOFL = list(pt_ct = c(sr = 0.0001, nt = 8000), aru66r = c(sr = 0.0001, nt = 3000),
              aru30r = c(sr = 0.0001, nt = 3000), aru30c = c(sr = 0.0001, nt = 3000)),
  NOPA = list(pt_ct = c(sr = 0.0005, nt = 2000), aru66r = c(sr = 0.0001, nt = 1500),
              aru30r = c(sr = 0.0001, nt = 2500), aru30c = c(sr = 0.0001, nt = 3000)),
  NOWA = list(pt_ct = c(sr = 0.0001, nt = 2000), aru66r = c(sr = 0.0005, nt = 3000),
              aru30r = c(sr = 0.0001, nt = 3000), aru30c = c(sr = 0.0001, nt = 3000)),
  OVEN = list(pt_ct = c(sr = 0.0001, nt = 3000), aru66r = c(sr = 0.00003, nt = 1500),
              aru30r = c(sr = 0.00003, nt = 4000), aru30c = c(sr = 0.0001, nt = 3000)),
  PIWA = list(pt_ct = c(sr = 0.0001, nt = 2000), aru66r = c(sr = 0.0005, nt = 3000),
              aru30r = c(sr = 0.0001, nt = 3000), aru30c = c(sr = 0.0001, nt = 3000)),
  PIWO = list(pt_ct = c(sr = 0.0001, nt = 3000), aru66r = c(sr = 0.0001, nt = 3000),
              aru30r = c(sr = 0.0001, nt = 3000), aru30c = c(sr = 0.0001, nt = 3000)),
  PUFI = list(pt_ct = c(sr = 0.0001, nt = 3000), aru66r = c(sr = 0.0001, nt = 3000),
              aru30r = c(sr = 0.0001, nt = 3000), aru30c = c(sr = 0.0001, nt = 3000)),
  RBNU = list(pt_ct = c(sr = 0.0005, nt = 2000), aru66r = c(sr = 0.0001, nt = 3000),
              aru30r = c(sr = 0.0001, nt = 3000), aru30c = c(sr = 0.0001, nt = 3000)),
  RCKI = list(pt_ct = c(sr = 0.0001, nt = 5000), aru66r = c(sr = 0.0001, nt = 3000),
              aru30r = c(sr = 0.0001, nt = 3000), aru30c = c(sr = 0.0001, nt = 3000)),
  SWTH = list(pt_ct = c(sr = 0.0001, nt = 3000), aru66r = c(sr = 0.0005, nt = 3000),
              aru30r = c(sr = 0.0001, nt = 3000), aru30c = c(sr = 0.0001, nt = 3000)),
  WBNU = list(pt_ct = c(sr = 0.0001, nt = 3000), aru66r = c(sr = 0.0005, nt = 3000),
              aru30r = c(sr = 0.0001, nt = 3000), aru30c = c(sr = 0.0001, nt = 3000)),
  WIWR = list(pt_ct = c(sr = 0.0005, nt = 3000), aru66r = c(sr = 0.0005, nt = 3000),
              aru30r = c(sr = 0.0005, nt = 3000), aru30c = c(sr = 0.0001, nt = 3000)),
  WTSP = list(pt_ct = c(sr = 0.0005, nt = 2000), aru66r = c(sr = 0.0001, nt = 6000),
              aru30r = c(sr = 0.0005, nt = 2000), aru30c = c(sr = 0.0001, nt = 3000)),
  YBSA = list(pt_ct = c(sr = 0.0005, nt = 5000), aru66r = c(sr = 0.0005, nt = 3000),
              aru30r = c(sr = 0.0001, nt = 3000), aru30c = c(sr = 0.0001, nt = 3000)),
  YRWA = list(pt_ct = c(sr = 0.0005, nt = 5000), aru66r = c(sr = 0.0005, nt = 2000),
              aru30r = c(sr = 0.0005, nt = 3000), aru30c = c(sr = 0.0005, nt = 2000))
)

if(plotson){
# Exploratory plots -----------------------------------------------------------
#hist(ptct$count, main = "number of individs")


# plot wren mean detections over time
plot(wiwr$day_of_yr, wiwr$count,
     main = "wiwr per pt count over time")

plot(gcki$day_of_yr, gcki$count,
     main = "gcki per pt count over time")




plot(sum_gcki$day_of_yr, sum_gcki$resp,
     main = "avg. gcki per count per day over time")
ggplot(data = sum_gcki, aes(x = day_of_yr, y = resp)) +
  geom_point() +
  geom_smooth()

ggplot(data = sum_arugcki, aes(x = day_of_yr, y = resp)) +
  geom_point() +
  geom_smooth()

ggplot(data = sum_arugcki10r, aes(x = day_of_yr, y = resp)) +
  geom_point() +
  geom_smooth()

ggplot(data = sum_arugcki22r, aes(x = day_of_yr, y = resp)) +
  geom_point() +
  geom_smooth()

plot(sum_wiwr$day_of_yr, sum_wiwr$resp,
     main = "avg. wiwr per count per day over time")
ggplot(data = sum_wiwr, aes(x = day_of_yr, y = resp)) +
  geom_point() +
  geom_smooth()

ggplot(data = sum_aruwiwr, aes(x = day_of_yr, y = resp)) +
  geom_point() +
  geom_smooth()

ggplot(data = sum_aruwiwr10r, aes(x = day_of_yr, y = resp)) +
  geom_point() +
  geom_smooth()

ggplot(data = sum_aruwiwr22r, aes(x = day_of_yr, y = resp)) +
  geom_point() +
  geom_smooth()

hist(sum_gcki$resp)
hist(sum_arugcki$resp)
hist(sum_arugcki10r$resp)
hist(sum_arugcki22r$resp)

hist(sum_wiwr$resp)
hist(sum_aruwiwr$resp)
hist(sum_aruwiwr10r$resp)
hist(sum_aruwiwr22r$resp)

aruplot <- list()
for(i in 1:length(aru_sp_codes)) {
  this_sp <- aru_sp_codes[i]
  tplot <- ggplot(data = sum_aru_dfs[[i]], aes(x = day_of_yr, y = resp)) +
    geom_point() +
    geom_smooth() +
    ggtitle(this_sp)
  aruplot[[i]] <- tplot
}


# end Exploratory plots -------------------------------------------------------
}

## Boosted Regression Tree for GCKI per DAY (point counts)----------------------
# define function to fit boosted regression tree
fit_brt <- function(test_fold, sp_data, newdata, nt, sr, 
                    resp_name, return_model, n_cores = 1) {
  # ARGS: resp_name - character string giving the name of the column to be used
  #           as the response variable
  # get only data not in test fold
  train_dat <- data.frame(sp_data[sp_data$fold != test_fold, ])
  # rename response column to standard name for this function
  colnames(train_dat)[colnames(train_dat) == resp_name] <- "resp" 
  
  f_m <- gbm(resp ~ 1 + wind + day_of_yr_c, 
             distribution = "laplace", 
             data = train_dat,
             interaction.depth = 1, 
             n.trees = nt, 
             n.minobsinnode = 1, 
             shrinkage = sr, 
             bag.fraction = 0.8, cv.folds = 10,
             keep.data = FALSE, verbose = F, n.cores = n_cores)
  # browser()
  #  gbm.perf(f_m)
  
  test_pred <- data.frame(sp_data[sp_data$fold == test_fold, ])
  test_pred$OOB_preds <- predict(f_m, newdata = test_pred, 
                                 n.trees = nt, type = "response")
  test_pred$error <- test_pred$OOB_preds - test_pred[, colnames(test_pred) == 
                                                       resp_name]
  
  # Get standardized predictions to new data
  stand_pred <- newdata[newdata$fold == test_fold, ]
  stand_pred$predictions <- predict(f_m, newdata = stand_pred, 
                                 n.trees = nt, type = "response")
  #stand_pred$error <- stand_pred$predictions - stand_pred$resp
  if(!return_model) rm(f_m)
  # return fitted model, predictions to the observed data from the test fold, 
  # and predictions to new data (with standardized covariates)
  if(return_model) {
    return(list(test_predictions = test_pred, 
                standardized_preds = stand_pred, mod = f_m))
  } else
  return(list(test_predictions = test_pred, 
              standardized_preds = stand_pred))
}

if(fitbrt){
# make a list to hold fitted models and predictions from models with many 
# replicate folds splits
# At the end of the following for loop, fits_gcki1_brt should have results from 
# 200 different 5-fold CV splits (so 1000 different models fit, 5 for each fold
# in each of 200 different fold splits)
fits_gcki1_brt <- list()
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
  pgcki_day$wind <- 1
  pgcki_day <- left_join(pgcki_day, days, by = c("day_of_yr" = "day"))
  
  # join CV fold info onto bird data
  bird_dat <- left_join(sum_gcki, days, by = c("day_of_yr" = "day"))
 
  # fit brt to data in CV folds
  brt_test_folds <- unique(bird_dat$fold)
  names(brt_test_folds) <- as.character(brt_test_folds) 
  
  # the first time through the loop, save an example set of 5-CV models so that
  # we can graph the error as a function of number of trees
  if(i < 3) {
    if(i == 1) mods <- list()
    mods[[i]] <- lapply(unique(bird_dat$fold), FUN = fit_brt, sp_data = bird_dat, 
                   newdata = pgcki_day, nt = 10000, sr = 0.0005, 
                   resp_name = "resp", return_model = TRUE)
  } else if(i ==3) {
    mods <- unlist(mods, recursive = F)
    saveRDS(mods, file = "example_fitted_brt_gcki1.rds")
    rm(mods)
  }
  
  brt_test_folds <- lapply(brt_test_folds, fit_brt, sp_data = bird_dat, 
                           newdata = pgcki_day, nt = 2000, sr = 0.0005, 
                           resp_name = "resp", return_model = FALSE)
  
  # put predictions for these 5 folds into the big list for all splits
  fits_gcki1_brt[[i]] <- brt_test_folds
  rm(brt_test_folds)
}
names(fits_gcki1_brt) <- 1:length(fits_gcki1_brt)
rmse_gcki_brt <- sapply(fits_gcki1_brt, FUN = function(x) {
  sapply(x, FUN = function(z) {
    sqrt(mean(z$test_predictions$error^2))
  })
})
saveRDS(fits_gcki1_brt, "fits_gcki1_brt.rds")
saveRDS(rmse_gcki_brt, "rmse_gcki_brt.rds")
rm(fits_gcki1_brt, rmse_gcki_brt)
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
  parugcki_day$wind <- 1
  parugcki_day <- left_join(parugcki_day, days, by = c("day_of_yr" = "day"))
  
  # join CV fold info onto bird data
  bird_dat <- left_join(sum_arugcki, days, by = c("day_of_yr" = "day"))
  
  # fit brt to data in CV folds
  brt_test_folds <- unique(bird_dat$fold)
  names(brt_test_folds) <- as.character(brt_test_folds) 
  
  # the first time through the loop, save an example set of 5-CV models so that
  # we can graph the error as a function of number of trees

  if(i < 3) {
    if(i == 1) mods <- list()
    mods[[i]] <- lapply(unique(bird_dat$fold), FUN = fit_brt, sp_data = bird_dat, 
                        newdata = parugcki_day, nt = 10000, sr = 0.0001, 
                        resp_name = "resp", return_model = TRUE)
  } else if(i ==3) {
    mods <- unlist(mods, recursive = F)
    saveRDS(mods, file = "example_fitted_brt_arugcki.rds")
    rm(mods)
  }
  
  brt_test_folds <- lapply(brt_test_folds, fit_brt, sp_data = bird_dat, 
                           newdata = parugcki_day, nt = 3000, sr = 0.0001,
                           resp_name = "resp", 
                           return_model = FALSE)
  
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
  pgcki_day$wind <- 1
  pgcki_day <- left_join(pgcki_day, days, by = c("day_of_yr" = "day"))
  
  # join CV fold info onto bird data
  bird_dat <- left_join(sum_arugcki10r, days, by = c("day_of_yr" = "day"))
  
  # fit brt to data in CV folds
  brt_test_folds <- unique(bird_dat$fold)
  names(brt_test_folds) <- as.character(brt_test_folds) 
  
  
  # the first time through the loop, save an example set of 5-CV models so that
  # we can graph the error as a function of number of trees
  if(i < 3) {
    if(i == 1) mods <- list()
    mods[[i]] <- lapply(unique(bird_dat$fold), FUN = fit_brt, sp_data = bird_dat, 
                        newdata = pgcki_day, nt = 10000, sr = 0.0001, 
                        resp_name = "resp", return_model = TRUE)
  } else if(i ==3) {
    mods <- unlist(mods, recursive = F)
    saveRDS(mods, file = "example_fitted_brt_arugcki10r.rds")
    rm(mods)
  }
  
  brt_test_folds <- lapply(brt_test_folds, fit_brt, sp_data = bird_dat, 
                           newdata = pgcki_day, nt = 3000, sr = 0.0001, 
                           resp_name = "resp", 
                           return_model = FALSE)

  
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
  pgcki_day$wind <- 1
  pgcki_day <- left_join(pgcki_day, days, by = c("day_of_yr" = "day"))
  
  # join CV fold info onto bird data
  bird_dat <- left_join(sum_arugcki22r, days, by = c("day_of_yr" = "day"))
  
  # fit brt to data in CV folds
  brt_test_folds <- unique(bird_dat$fold)
  names(brt_test_folds) <- as.character(brt_test_folds) 
  
  # the first time through the loop, save an example set of 5-CV models so that
  # we can graph the error as a function of number of trees
  if(i < 3) {
    if(i == 1) mods <- list()
    mods[[i]] <- lapply(unique(bird_dat$fold), FUN = fit_brt, sp_data = bird_dat, 
                        newdata = pgcki_day, nt = 10000, sr = 0.0005, 
                        resp_name = "resp", return_model = TRUE)
  } else if(i ==3) {
    mods <- unlist(mods, recursive = F)
    saveRDS(mods, file = "example_fitted_brt_arugcki22r.rds")
    rm(mods)
  }
  
  brt_test_folds <- lapply(brt_test_folds, fit_brt, sp_data = bird_dat, 
                           newdata = pgcki_day, nt = 2000, sr = 0.0005, 
                           resp_name = "resp", 
                           return_model = FALSE)
  
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


if(fitbrt){
  ## Boosted Regression Tree for WIWR per DAY (ptct)---------------------------
  # make a list to hold fitted models and predictions from models with many 
  # replicate folds splits
  # At the end of the following for loop, fits_wiwr1_brt should have results from 
  # 200 different 5-fold CV splits (so 1000 different models fit, 5 for each fold
  # in each of 200 different fold splits)
  fits_wiwr1_brt <- list()
  
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
    pwiwr_day$wind <- 1
    pwiwr_day <- left_join(pwiwr_day, days, by = c("day_of_yr" = "day"))
    
    # join CV fold info onto bird data
    bird_dat <- left_join(sum_wiwr, days, by = c("day_of_yr" = "day"))
    
    # fit brt to data in CV folds
    brt_test_folds <- unique(bird_dat$fold)
    names(brt_test_folds) <- as.character(brt_test_folds) 
    
    # the first time through the loop, save an example set of 5-CV models so that
    # we can graph the error as a function of number of trees
    if(i < 3) {
      if(i == 1) mods <- list()
      mods[[i]] <- lapply(unique(bird_dat$fold), FUN = fit_brt, sp_data = bird_dat, 
                          newdata = pwiwr_day, nt = 10000, sr = 0.0005, 
                          resp_name = "resp", return_model = TRUE)
    } else if(i ==3) {
      mods <- unlist(mods, recursive = F)
      saveRDS(mods, file = "example_fitted_brt_wiwr1.rds")
      rm(mods)
    }
    
    brt_test_folds <- lapply(brt_test_folds, fit_brt, sp_data = bird_dat, 
                             newdata = pwiwr_day, nt = 3000, sr = 0.0005, 
                             resp_name = "resp", 
                             return_model = FALSE)
    
    
    # put predictions for these 5 folds into the big list for all splits
    fits_wiwr1_brt[[i]] <- brt_test_folds
    rm(brt_test_folds)
    gc()
  }
  names(fits_wiwr1_brt) <- 1:length(fits_wiwr1_brt)
  
  rmse_wiwr_brt <- sapply(fits_wiwr1_brt, FUN = function(x) {
    sapply(x, FUN = function(z) {
      sqrt(mean(z$test_predictions$error^2))
    })
  })
  saveRDS(fits_wiwr1_brt, "fits_wiwr1_brt.rds")
  saveRDS(rmse_wiwr_brt, "rmse_wiwr_brt.rds")
  rm(fits_wiwr1_brt, rmse_wiwr_brt)
  ## end BRT for WIWR per DAY model (ptct)-------------------------------------
  
  
  ## Boosted Regression Tree for WIWR per DAY (ARU- 10 consec min)-------------
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
    paruwiwr_day$wind <- 1
    paruwiwr_day <- left_join(paruwiwr_day, days, by = c("day_of_yr" = "day"))
    
    # join CV fold info onto bird data
    bird_dat <- left_join(sum_aruwiwr, days, by = c("day_of_yr" = "day"))
    
    # fit brt to data in CV folds
    brt_test_folds <- unique(bird_dat$fold)
    names(brt_test_folds) <- as.character(brt_test_folds) 
    
    # the first time through the loop, save an example set of 5-CV models so that
    # we can graph the error as a function of number of trees
    if(i < 3) {
      if(i == 1) mods <- list()
      mods[[i]] <- lapply(unique(bird_dat$fold), FUN = fit_brt, sp_data = bird_dat, 
                          newdata = paruwiwr_day, nt = 10000, sr = 0.0001, 
                          resp_name = "resp", return_model = TRUE)
    } else if(i ==3) {
      mods <- unlist(mods, recursive = F)
      saveRDS(mods, file = "example_fitted_brt_aruwiwr.rds")
      rm(mods)
    }
    
    brt_test_folds <- lapply(brt_test_folds, fit_brt, sp_data = bird_dat, 
                             newdata = paruwiwr_day, nt = 3000, sr = 0.0001, 
                             resp_name = "resp", 
                             return_model = FALSE)
    
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
  ## end BRT for WIWR per DAY model (ARU- 10 consec min)-----------------------
  
  ## BRT for WIWR per day- ARU 10 random min-----------------------------------
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
    pwiwr_day$wind <- 1
    pwiwr_day <- left_join(pwiwr_day, days, by = c("day_of_yr" = "day"))
    
    # join CV fold info onto bird data
    bird_dat <- left_join(sum_aruwiwr10r, days, by = c("day_of_yr" = "day"))
    
    # fit brt to data in CV folds
    brt_test_folds <- unique(bird_dat$fold)
    names(brt_test_folds) <- as.character(brt_test_folds) 
    
    # the first time through the loop, save an example set of 5-CV models so that
    # we can graph the error as a function of number of trees
    if(i < 3) {
      if(i == 1) mods <- list()
      mods[[i]] <- lapply(unique(bird_dat$fold), FUN = fit_brt, sp_data = bird_dat, 
                          newdata = pwiwr_day, nt = 10000, sr = 0.0005,
                          resp_name = "resp", return_model = TRUE)
    } else if(i ==3) {
      mods <- unlist(mods, recursive = F)
      saveRDS(mods, file = "example_fitted_brt_aruwiwr10r.rds")
      rm(mods)
    }
    
    brt_test_folds <- lapply(brt_test_folds, fit_brt, sp_data = bird_dat, 
                             newdata = pwiwr_day, nt = 3000, sr = 0.0005, 
                             resp_name = "resp", 
                             return_model = FALSE)
    
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
    pwiwr_day$wind <- 1
    pwiwr_day <- left_join(pwiwr_day, days, by = c("day_of_yr" = "day"))
    
    # join CV fold info onto bird data
    bird_dat <- left_join(sum_aruwiwr22r, days, by = c("day_of_yr" = "day"))
    
    # fit brt to data in CV folds
    brt_test_folds <- unique(bird_dat$fold)
    names(brt_test_folds) <- as.character(brt_test_folds) 
    
    # the first time through the loop, save an example set of 5-CV models so that
    # we can graph the error as a function of number of trees
    if(i < 3) {
      if(i == 1) mods <- list()
      mods[[i]] <- lapply(unique(bird_dat$fold), FUN = fit_brt, sp_data = bird_dat, 
                          newdata = pwiwr_day, nt = 10000, sr = 0.0005, 
                          resp_name = "resp", return_model = TRUE)
    } else if(i ==3) {
      mods <- unlist(mods, recursive = F)
      saveRDS(mods, file = "example_fitted_brt_aruwiwr22r.rds")
      rm(mods)
    }
    
    brt_test_folds <- lapply(brt_test_folds, fit_brt, sp_data = bird_dat, 
                             newdata = pwiwr_day, nt = 3000, sr = 0.0005, 
                             resp_name = "resp", 
                             return_model = FALSE)
    
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

## BRT for all species per day- point counts------------------------------------
if(fitbrt_allSp_ap) {
  # loop through species detected on both point counts and ARUs
  for (sp_n in 1:length(brt_params)) {
    this_sp <- names(brt_params)[sp_n]
    this_df <- sum_species_dfs[[this_sp]] # get point count data for this sp.
    # only model sp detected > 1 day
    if(length(which(this_df$resp > 0)) > 1) {
      ## Boosted Regression Tree for this species per DAY (ptct) -------------
      # make a list to hold fitted models and predictions from models with many 
      # replicate folds splits
      # At the end of the following for loop, fits_sp_brt should have results 
      # from 200 different 5-fold CV splits (so 1000 different models fit, 5 
      # for each fold in each of 200 different fold splits)
      fits_sp_brt <- list()
      
      for (i in 1:200) { # do this 1:200
        # assign days to 3-day blocks
        days <- data.frame(day = min(this_df$day_of_yr):max(this_df$day_of_yr), 
                           block = NA)
        n_blocks <- nrow(days)/3
        blocks <- rep(1:n_blocks, 3)
        blocks <- blocks[order(blocks)]
        start_row <- sample(1:nrow(days), size = 1)
        days$block[start_row:nrow(days)] <- blocks[1:length(
          start_row:nrow(days))]
        if(start_row != 1) {
          days$block[1:(start_row - 1)] <- blocks[(length(
            start_row:nrow(days)) + 1):nrow(days)]
        }
        
        # assign blocks to CV folds
        fold_assignments <- data.frame(
          block = unique(days$block), 
          fold = sample(rep_len(1:5, length.out = length(unique(days$block)))))
        days <- left_join(days, fold_assignments, by = "block")
        rm(blocks, n_blocks, start_row, fold_assignments)
        
        # create new data to predict with
        p_day <- data.frame(day_of_yr = seq(min(this_df$day_of_yr), 
                                            max(this_df$day_of_yr), by = 1))
        p_day$day_of_yr_c <- p_day$day_of_yr-mean(p_day$day_of_yr)
        p_day$wind <- 1
        p_day <- left_join(p_day, days, by = c("day_of_yr" = "day"))
        
        # join CV fold info onto bird data
        bird_dat <- left_join(this_df, days, by = c("day_of_yr" = "day"))
        
        # fit brt to data in CV folds
        brt_test_folds <- unique(bird_dat$fold)
        names(brt_test_folds) <- as.character(brt_test_folds) 
        
        # the first time through the loop, save an example set of a few 5-CV 
        # models so that we can graph the error as a function of number of trees
        if(i < 3) {
          if(i == 1) mods <- list()
          mods[[i]] <- lapply(unique(bird_dat$fold), FUN = fit_brt, 
                              sp_data = bird_dat, newdata = p_day, 
                              nt = 10000, 
                              sr = brt_params[[this_sp]]$pt_ct[["sr"]], 
                              n_cores = n_cores,
                              resp_name = "resp", return_model = TRUE)
        } else if(i ==3) {
          mods <- unlist(mods, recursive = F)
          saveRDS(mods, file = paste0("example_fitted_brt_", this_sp, ".rds"))
          rm(mods)
        }
        
        # Here, we specify the nt and sr for each individual species, based
        # on looking at the graphs of error by ntrees made using the .rds file
        # written out in the above loop, and the graphing code in
        # suppmaterials_plots_yellowpaper.R
        brt_test_folds <- lapply(brt_test_folds, FUN = fit_brt, 
                                 sp_data = bird_dat,
                                 newdata = p_day, 
                                 nt = brt_params[[this_sp]]$pt_ct[["nt"]], 
                                 sr = brt_params[[this_sp]]$pt_ct[["sr"]],
                                 resp_name = "resp", n_cores = n_cores,
                                 return_model = FALSE)
        
        # put predictions for these 5 folds into the big list for all splits
        fits_sp_brt[[i]] <- brt_test_folds
        rm(brt_test_folds)
        gc()
      }
      names(fits_sp_brt) <- 1:length(fits_sp_brt)

      rmse_thisSp_brt <- sapply(fits_sp_brt, FUN = function(x) {
        sapply(x, FUN = function(z) {
          sqrt(mean(z$test_predictions$error^2))
        })
      })
      saveRDS(fits_sp_brt, paste0("fits_", this_sp, "_brt.rds"))
      saveRDS(rmse_thisSp_brt, paste0("rmse_", this_sp, "_brt.rds"))
      try(rm(fits_sp_brt, rmse_thisSp_brt))
      ## end BRT for this species per DAY model (ptct)-------------------------
    }
  }
}
## end BRT for all species per day- point counts--------------------------------

## BRT for all species per day- ARU 10 consec min-------------------------------
if(fitbrt_allSp_a30c) {
  # loop through species detected on both point counts and ARUs
  for (sp_n in 1:length(brt_params)) {
    this_sp <- names(brt_params)[sp_n]
    this_df <- sum_aru_dfs_10c[[this_sp]] # get ARU data for this sp.
    # only model sp detected > 1 day
    if(length(which(this_df$resp > 0)) > 1) {
      ## Boosted Regression Tree for this species per DAY (aru) -------------
      # make a list to hold fitted models and predictions from models with many 
      # replicate folds splits
      # At the end of the following for loop, fits_sp_brt should have results 
      # from 200 different 5-fold CV splits (so 1000 different models fit, 5 
      # for each fold in each of 200 different fold splits)
      fits_sp_brt <- list()
      
      for (i in 1:3) { # do this 1:200
        # assign days to 3-day blocks
        days <- data.frame(day = min(this_df$day_of_yr):max(this_df$day_of_yr), 
                           block = NA)
        n_blocks <- nrow(days)/3
        blocks <- rep(1:n_blocks, 3)
        blocks <- blocks[order(blocks)]
        start_row <- sample(1:nrow(days), size = 1)
        days$block[start_row:nrow(days)] <- blocks[1:length(
          start_row:nrow(days))]
        if(start_row != 1) {
          days$block[1:(start_row - 1)] <- blocks[(length(
            start_row:nrow(days)) + 1):nrow(days)]
        }
        
        # assign blocks to CV folds
        fold_assignments <- data.frame(
          block = unique(days$block), 
          fold = sample(rep_len(1:5, length.out = length(unique(days$block)))))
        days <- left_join(days, fold_assignments, by = "block")
        rm(blocks, n_blocks, start_row, fold_assignments)
        
        # create new data to predict with
        p_day <- data.frame(day_of_yr = seq(min(this_df$day_of_yr), 
                                            max(this_df$day_of_yr), by = 1))
        p_day$day_of_yr_c <- p_day$day_of_yr-mean(p_day$day_of_yr)
        p_day$wind <- 1
        p_day <- left_join(p_day, days, by = c("day_of_yr" = "day"))
        
        # join CV fold info onto bird data
        bird_dat <- left_join(this_df, days, by = c("day_of_yr" = "day"))
        
        # fit brt to data in CV folds
        brt_test_folds <- unique(bird_dat$fold)
        names(brt_test_folds) <- as.character(brt_test_folds) 
        
        # the first time through the loop, save an example set of a few 5-CV 
        # models so that we can graph the error as a function of number of trees
        if(i < 3) {
          if(i == 1) mods <- list()
          mods[[i]] <- lapply(unique(bird_dat$fold), FUN = fit_brt, 
                              sp_data = bird_dat, newdata = p_day, 
                              nt = 10000, 
                              sr = brt_params[[this_sp]]$aru30c[["sr"]], 
                              n_cores = n_cores,
                              resp_name = "resp", return_model = TRUE)
        } else if(i ==3) {
          mods <- unlist(mods, recursive = F)
          saveRDS(mods, file = paste0("example_fitted_brt_aru30c_", this_sp, 
                                      ".rds"))
          rm(mods)
        }
        
        # Here, we specify the nt and sr for each individual species, based
        # on looking at the graphs of error by ntrees made using the .rds file
        # written out in the above loop, and the graphing code in
        # suppmaterials_plots_yellowpaper.R
        brt_test_folds <- lapply(brt_test_folds, FUN = fit_brt,
                                 sp_data = bird_dat,
                                 newdata = p_day,
                                 nt = brt_params[[this_sp]]$aru30c[["nt"]],
                                 sr = brt_params[[this_sp]]$aru30c[["sr"]],
                                 resp_name = "resp", n_cores = n_cores,
                                 return_model = FALSE)
        
        # put predictions for these 5 folds into the big list for all splits
        fits_sp_brt[[i]] <- brt_test_folds
        rm(brt_test_folds)
        gc()
      }
      names(fits_sp_brt) <- 1:length(fits_sp_brt)
      
      rmse_thisSp_brt <- sapply(fits_sp_brt, FUN = function(x) {
        sapply(x, FUN = function(z) {
          sqrt(mean(z$test_predictions$error^2))
        })
      })
      saveRDS(fits_sp_brt, paste0("fits_", this_sp, "_aru30c_brt.rds"))
      saveRDS(rmse_thisSp_brt, paste0("rmse_", this_sp, "_aru30c_brt.rds"))
      try(rm(fits_sp_brt, rmse_thisSp_brt))
      ## end BRT for this species per DAY model (aru)-------------------------
    }
  }
}
## end BRT for all species- ARU 10 consec min-----------------------------------

##BRT for all species- ARU 10 random min-----------------------------------------
if(fitbrt_allSp_a30r) {
  # loop through species detected on both point counts and ARUs
  for (sp_n in 1:length(brt_params)) {
    this_sp <- names(brt_params)[sp_n]
    this_df <- sum_aru_dfs_10r[[this_sp]] # get ARU data for this sp.
    # only model sp detected > 1 day
    if(length(which(this_df$resp > 0)) > 1) {
      ## Boosted Regression Tree for this species per DAY (aru) -------------
      # make a list to hold fitted models and predictions from models with many 
      # replicate folds splits
      # At the end of the following for loop, fits_sp_brt should have results 
      # from 200 different 5-fold CV splits (so 1000 different models fit, 5 
      # for each fold in each of 200 different fold splits)
      fits_sp_brt <- list()
      
      for (i in 1:200) { # do this 1:200
        # assign days to 3-day blocks
        days <- data.frame(day = min(this_df$day_of_yr):max(this_df$day_of_yr), 
                           block = NA)
        n_blocks <- nrow(days)/3
        blocks <- rep(1:n_blocks, 3)
        blocks <- blocks[order(blocks)]
        start_row <- sample(1:nrow(days), size = 1)
        days$block[start_row:nrow(days)] <- blocks[1:length(
          start_row:nrow(days))]
        if(start_row != 1) {
          days$block[1:(start_row - 1)] <- blocks[(length(
            start_row:nrow(days)) + 1):nrow(days)]
        }
        
        # assign blocks to CV folds
        fold_assignments <- data.frame(
          block = unique(days$block), 
          fold = sample(rep_len(1:5, length.out = length(unique(days$block)))))
        days <- left_join(days, fold_assignments, by = "block")
        rm(blocks, n_blocks, start_row, fold_assignments)
        
        # create new data to predict with
        p_day <- data.frame(day_of_yr = seq(min(this_df$day_of_yr), 
                                            max(this_df$day_of_yr), by = 1))
        p_day$day_of_yr_c <- p_day$day_of_yr-mean(p_day$day_of_yr)
        p_day$wind <- 1
        p_day <- left_join(p_day, days, by = c("day_of_yr" = "day"))
        
        # join CV fold info onto bird data
        bird_dat <- left_join(this_df, days, by = c("day_of_yr" = "day"))
        
        # fit brt to data in CV folds
        brt_test_folds <- unique(bird_dat$fold)
        names(brt_test_folds) <- as.character(brt_test_folds) 
        
        # the first time through the loop, save an example set of a few 5-CV 
        # models so that we can graph the error as a function of number of trees
        if(i < 3) {
          if(i == 1) mods <- list()
          mods[[i]] <- lapply(unique(bird_dat$fold), FUN = fit_brt, 
                              sp_data = bird_dat, newdata = p_day, 
                              nt = 10000, 
                              sr = brt_params[[this_sp]]$aru30r[["sr"]], 
                              n_cores = n_cores,
                              resp_name = "resp", return_model = TRUE)
        } else if(i ==3) {
          mods <- unlist(mods, recursive = F)
          saveRDS(mods, file = paste0("example_fitted_brt_aru30r_", this_sp, 
                                      ".rds"))
          rm(mods)
        }
        
        # Here, we specify the nt and sr for each individual species, based
        # on looking at the graphs of error by ntrees made using the .rds file
        # written out in the above loop, and the graphing code in
        # suppmaterials_plots_yellowpaper.R
        brt_test_folds <- lapply(brt_test_folds, FUN = fit_brt,
                                 sp_data = bird_dat,
                                 newdata = p_day,
                                 nt = brt_params[[this_sp]]$aru30r[["nt"]],
                                 sr = brt_params[[this_sp]]$aru30r[["sr"]],
                                 resp_name = "resp", n_cores = n_cores,
                                 return_model = FALSE)
        
        # put predictions for these 5 folds into the big list for all splits
        fits_sp_brt[[i]] <- brt_test_folds
        rm(brt_test_folds)
        gc()
      }
      names(fits_sp_brt) <- 1:length(fits_sp_brt)
      
      rmse_thisSp_brt <- sapply(fits_sp_brt, FUN = function(x) {
        sapply(x, FUN = function(z) {
          sqrt(mean(z$test_predictions$error^2))
        })
      })
      saveRDS(fits_sp_brt, paste0("fits_", this_sp, "_aru30r_brt.rds"))
      saveRDS(rmse_thisSp_brt, paste0("rmse_", this_sp, "_aru30r_brt.rds"))
      try(rm(fits_sp_brt, rmse_thisSp_brt))
      ## end BRT for this species per DAY model (aru)-------------------------
    }
  }
}

## end BRT for all species- ARU 10 random min-----------------------------------

## BRT for all species per day- ARU 22 random min-------------------------------
if(fitbrt_allSp_a66r) {
  # loop through species detected on both point counts and ARUs
  for (sp_n in 1:length(brt_params)) {
    this_sp <- names(brt_params)[sp_n]
    this_df <- sum_aru_dfs[[this_sp]] # get ARU data for this sp.
    # only model sp detected > 1 day
    if(length(which(this_df$resp > 0)) > 1) {
      ## Boosted Regression Tree for this species per DAY (aru) -------------
      # make a list to hold fitted models and predictions from models with many 
      # replicate folds splits
      # At the end of the following for loop, fits_sp_brt should have results 
      # from 200 different 5-fold CV splits (so 1000 different models fit, 5 
      # for each fold in each of 200 different fold splits)
      fits_sp_brt <- list()
      
      for (i in 1:200) { # do this 1:200
        # assign days to 3-day blocks
        days <- data.frame(day = min(this_df$day_of_yr):max(this_df$day_of_yr), 
                           block = NA)
        n_blocks <- nrow(days)/3
        blocks <- rep(1:n_blocks, 3)
        blocks <- blocks[order(blocks)]
        start_row <- sample(1:nrow(days), size = 1)
        days$block[start_row:nrow(days)] <- blocks[1:length(
          start_row:nrow(days))]
        if(start_row != 1) {
          days$block[1:(start_row - 1)] <- blocks[(length(
            start_row:nrow(days)) + 1):nrow(days)]
        }
        
        # assign blocks to CV folds
        fold_assignments <- data.frame(
          block = unique(days$block), 
          fold = sample(rep_len(1:5, length.out = length(unique(days$block)))))
        days <- left_join(days, fold_assignments, by = "block")
        rm(blocks, n_blocks, start_row, fold_assignments)
        
        # create new data to predict with
        p_day <- data.frame(day_of_yr = seq(min(this_df$day_of_yr), 
                                            max(this_df$day_of_yr), by = 1))
        p_day$day_of_yr_c <- p_day$day_of_yr-mean(p_day$day_of_yr)
        p_day$wind <- 1
        p_day <- left_join(p_day, days, by = c("day_of_yr" = "day"))
        
        # join CV fold info onto bird data
        bird_dat <- left_join(this_df, days, by = c("day_of_yr" = "day"))
        
        # fit brt to data in CV folds
        brt_test_folds <- unique(bird_dat$fold)
        names(brt_test_folds) <- as.character(brt_test_folds) 
        
        # the first time through the loop, save an example set of a few 5-CV 
        # models so that we can graph the error as a function of number of trees
        if(i < 3) {
          if(i == 1) mods <- list()
          mods[[i]] <- lapply(unique(bird_dat$fold), FUN = fit_brt, 
                              sp_data = bird_dat, newdata = p_day, 
                              nt = 10000, 
                              sr = brt_params[[this_sp]]$aru66r[["sr"]], 
                              n_cores = n_cores,
                              resp_name = "resp", return_model = TRUE)
        } else if(i ==3) {
          mods <- unlist(mods, recursive = F)
          saveRDS(mods, file = paste0("example_fitted_brt_aru66r_", this_sp, 
                                      ".rds"))
          rm(mods)
        }
        
        # Here, we specify the nt and sr for each individual species, based
        # on looking at the graphs of error by ntrees made using the .rds file
        # written out in the above loop, and the graphing code in
        # suppmaterials_plots_yellowpaper.R
        brt_test_folds <- lapply(brt_test_folds, FUN = fit_brt,
                                 sp_data = bird_dat,
                                 newdata = p_day,
                                 nt = brt_params[[this_sp]]$aru66r[["nt"]],
                                 sr = brt_params[[this_sp]]$aru66r[["sr"]],
                                 resp_name = "resp", n_cores = n_cores,
                                 return_model = FALSE)

        # put predictions for these 5 folds into the big list for all splits
        fits_sp_brt[[i]] <- brt_test_folds
        rm(brt_test_folds)
        gc()
      }
      names(fits_sp_brt) <- 1:length(fits_sp_brt)

      rmse_thisSp_brt <- sapply(fits_sp_brt, FUN = function(x) {
        sapply(x, FUN = function(z) {
          sqrt(mean(z$test_predictions$error^2))
        })
      })
      saveRDS(fits_sp_brt, paste0("fits_", this_sp, "_aru66r_brt.rds"))
      saveRDS(rmse_thisSp_brt, paste0("rmse_", this_sp, "_aru66r_brt.rds"))
      try(rm(fits_sp_brt, rmse_thisSp_brt))
      ## end BRT for this species per DAY model (aru)-------------------------
    }
  }
}
## end BRT for all species per day- ARU 22 random min---------------------------
