################################
## Linear Regression- Predicting species detection with 10 min counts
## Point Abbaye 2019
## 
## author: Ellie Roark
## created: 29 August 2019
## last modified: 19 December 2019
## 
## inputs: *DataCleaningPtAbbaye2019.R- script that loads the aru and point ct 
##          data from 2019 and returns the following:
##            - allaru: df of every 10 min paired ct aru observation
##            - ptct: df of every point count observation
##            - spdet_paired: df with # of species detected per count by count
##              type
##
##         
## outputs: *linear regression model predicting the number of species detected 
##            based on count type, timing and weather variables
##            
## TODO: * 
################################

#install.packages("psych")
#install.packages("GGally")
#install.packages("sandwich")
#install.packages("MASS")
library(Hmisc)
library(MASS)
library(tidyverse)
library(lubridate)
library(car)
library(psych)
library(GGally)
library(sandwich)
library(nlme)
library(ICC)
library(lme4)
library(AICcmodavg)
library(afex)


setwd("/home/emer/Dropbox/Ellie Roark/R/PointAbbaye/")
source("./SRModels/SRModels/DataPrep_SR~counttype_10mincts_2019.R")

set.seed(3271989)

### clean up environment
rm(filenames, longaru, widearu, nobird_aruct, nobird_ptct,
   spdet_aru, spdet_ptct, sunrise_times, var_aru, var_pt, weathervar, allaru,
   ptct, allaru_nb, ptct_nb, spdet_aruF)

### control whether I make exploratory plots and run diagnostics:
plotson <- FALSE
regdiag <- FALSE
poisdiag <- FALSE

# create vector of counts with no aru recording
drop_counts <- c("2019-04-16_p4",
                 "2019-04-17_p1", 
                 "2019-04-20_p5", 
                 "2019-05-06_p8", 
                 "2019-05-07_p17", 
                 "2019-05-08_p11", 
                 "2019-05-11_p6", 
                 "2019-05-12_FB2", 
                 "2019-05-14_p6",
                 "2019-05-15_p1", 
                 "2019-05-17_p9", 
                 "2019-05-21_p17", 
                 "2019-05-21_p5", 
                 "2019-05-22_p15")



## coerce data to desired format and regroup variables--------------------------

#change wind from dbl to factor with 3 levels (0-1, 2, 3+)
spdet_paired[which(spdet_paired$wind <= "1"), "wind"] <- "0-1"
spdet_paired[which(spdet_paired$wind >= "3"), "wind"] <- "3+"
spdet_paired$wind <- factor(as.character(spdet_paired$wind), 
                            levels = c("0-1", "2", "3+"))

#change rain into factor with two groups, wet and dry
spdet_paired[which(spdet_paired$rain == "Rain/Snow"), "rain"] <- "wet"
spdet_paired[which(spdet_paired$rain == "Drizzle"), "rain"] <- "wet"
spdet_paired[which(spdet_paired$rain == "Fog"), "rain"] <- "wet"
spdet_paired$rain <- factor(as.character(spdet_paired$rain), 
                            levels = c("Dry", "wet"))

#change noise into factor variable with three groups: 0, 1, >2
spdet_paired[which(spdet_paired$noise >= 2), "noise"] <- ">2"
spdet_paired$noise <- factor(as.character(spdet_paired$noise), 
                             levels = c("0", "1", ">2"))

#make aru_id into factor variable
spdet_paired$aru_id <- factor(as.character(spdet_paired$aru_id), 
                             levels = c("swift01", "swift02", "swift03", "AM",
                                        "none"))

# make a centered day of year and a squared day of year
spdet_paired$day_of_yr_c <- spdet_paired$day_of_yr - mean(spdet_paired$day_of_yr)
spdet_paired$day_sq <- spdet_paired$day_of_yr_c^2

# center min_past_sun
spdet_paired$min_past_sun_c <- spdet_paired$min_past_sun - 
  mean(spdet_paired$min_past_sun)

# make scaled versions of all continuous variables
spdet_paired$day_of_yr_s <- scale(spdet_paired$day_of_yr_c, 
                                  center = F, scale = T)[, 1] # funny subsetting
                                                              # because scale 
                                                              # returns a matrix
spdet_paired$day_sq_s <- spdet_paired$day_of_yr_s^2
spdet_paired$min_past_sun_s <- scale(spdet_paired$min_past_sun_c, center = F, 
                                     scale = T)[, 1]

# make count type a factor variable
spdet_paired$count_type <- factor(as.character(spdet_paired$count_type), 
                                  levels = c("point", "aru"))

# make point_id a factor variable
spdet_paired$point_id <- factor(as.character(spdet_paired$point_id), 
                                levels = c("p1", "p2", "p3", "p4", "p5", "p6",
                                           "p8", "p9", "p10", "p11", "p12", 
                                           "p13","p14", "p15", "p17", "p18", 
                                           "FB1", "FB2"))

# make spdet_all, which will have ALL counts in it, and subset spdet_paired to 
# only counts where there's at least one aru recording
spdet_all <- spdet_paired
spdet_paired <- spdet_paired[spdet_paired$ptct_id %nin% drop_counts, ]

##end coerce data to desired format and regroup variables-----------------------

if(plotson){
## EXPLORATORY PLOTS -----------------------------------------------------------
#hist of frequency of # of sp detected
obsfreq_hist <- hist(spdet_paired$sp_detected, main = "Species Detected")

#boxplot for sp detected by each count type
count_type_box <- ggplot(spdet_paired, aes(x=count_type, y=sp_detected)) + 
  geom_boxplot() + scale_y_continuous(breaks = c(1, 5, 10)) +
  labs(x = "Count type", y = "Number of species detected", 
       title = "Number of Species Detected per Point Count by Count Type")
count_type_box

aprcount_type_box <- ggplot(spdet_paired[which(
  spdet_paired$date <= "2019-05-01"), ], aes(x=count_type, y=sp_detected)) + 
  geom_boxplot() + labs(title = "Number of Species Detected by Count Type")

#boxplot for sp detected by aru_id
aru_id_box <- ggplot(spdet_paired, aes(x=aru_id, y=sp_detected)) + 
  geom_boxplot() + labs(x = "ARU ID", y = "Species detected", 
  title = "Number of Species Detected per Point Count by ARU ID")
aru_id_box

#boxplot for species detected per count, total
allobs_box <- ggplot(spdet_paired, aes(y=sp_detected)) + geom_boxplot()
allobs_box

#boxplot for species detected at each point location
point_id_box <- ggplot(spdet_paired, aes(x=point_id, y=sp_detected)) + 
  geom_boxplot() + labs(title = "Number of Species Detected at each Point loc")
point_id_box

#qqplot for species detected
qqnorm(spdet_paired$sp_detected, main = "QQ Plot for all species detected")
qqline(spdet_paired$sp_detected)

#plot species detected per rain level
rain_box <- ggplot(spdet_paired, aes(x=rain, y=sp_detected)) + geom_boxplot() +
  labs(title = "Species detected over precipitation type")
rain_box

#plot species detected per wind level
wind_box <- ggplot(spdet_paired, aes(x=wind, y=sp_detected)) + geom_boxplot() +
  labs(title = "Species detected over wind level")
wind_box

#plot species detected per noise level
noise_box <- ggplot(spdet_paired, aes(x=noise, y=sp_detected)) + geom_boxplot() +
  labs(title = "Species detected over noise level")
noise_box

# plot of sp detected over time
doy_spdet <- plot(spdet_paired$day_of_yr, spdet_paired$sp_detected, 
                  main = "Species per count over time")
abline(lm(sp_detected~day_of_yr, data = spdet_paired),
       col = "chartreuse4")
with(spdet_paired, lines(lowess(day_of_yr, sp_detected)
                         , col = "darkblue"))

#sp detected over time, stratified by count type
spdet_time_ctype <- ggplot(spdet_paired, aes(x=day_of_yr, y=sp_detected, 
                                             color=count_type)) +  
  geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method="glm",method.args=list(family=poisson),  
              se=TRUE)     # Add confidence interval shading
spdet_time_ctype

spdet_time_ctype_loess <- ggplot(spdet_paired, aes(x=day_of_yr, y=sp_detected, 
                                             color=count_type)) +  
  theme_bw() +
  geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method="loess",  
              se=TRUE)     # Add confidence interval shading
spdet_time_ctype_loess

tod_spdet <- plot(spdet_paired$min_past_sun, spdet_paired$sp_detected,
                  main = "species per count by minutes past sunrise")

#look at response variable
describe(spdet_paired$sp_detected)

#look at histograms for each count type
ggplot(spdet_paired, aes(sp_detected, fill = count_type)) + 
  geom_histogram(binwidth = 1) + 
  facet_grid(count_type ~ ., margins = TRUE, scales = "free")

#look at pairs plot for all data
spdet_paired <- spdet_paired %>% ungroup()
spdet_paired_covs <- select(spdet_paired, c("wind", "rain", "noise", "day_sq",
                                            "day_of_yr_c", "min_past_sun"))

pairplot <- ggpairs(spdet_paired_covs, columns = 1:5)
}

## END EXPLORATORY PLOTS -------------------------------------------------------

## LINEAR REGRESSION MODEL -----------------------------------------------------
## species detected ~ a + beta*count_type + beta2*wind + beta3*noise + beta4*rain
##  + beta5*dayofyear + beta6*dayofyear^2 + beta7*count_type*rain
##  + beta8*minpastsunrise + beta9*aru_id

spdetmod <- glm(sp_detected ~ 1 + count_type + wind + rain + noise +
                            day_of_yr_s + day_sq_s + rain:count_type +
                            day_of_yr_s:count_type + day_sq_s:count_type + 
                            min_past_sun_s, 
                data = spdet_all, 
                 family = "gaussian")

summary(spdetmod)
plot(spdetmod)

## this gaussian regression does NOT meet constant variance assumption because 
## residuals show a distinctive shape. so instead we try:  
## 
## poisson regression model!!
pois_spdetmod <- glm(sp_detected ~ 1 + count_type + wind + rain + noise +
                       day_of_yr_s + day_sq_s + rain:count_type +
                       day_of_yr_s:count_type + day_sq_s:count_type + 
                       min_past_sun_s,
                     data = spdet_all, family = "poisson")

summary(pois_spdetmod)
plot(pois_spdetmod)

## test significance of whole model agains null model (that count type doesn't
## matter)
nullpois_spdetmod <- glm(sp_detected ~ 1 + wind + rain + noise +
                               day_of_yr_s + day_sq_s + min_past_sun_s,
                             data = spdet_all, family = "poisson")
anova(nullpois_spdetmod, pois_spdetmod, test= "Chisq")

## test significance of each categorical variable
# wind
nowind_spdetmod <- glm(sp_detected ~ 1 + count_type + rain + noise +
                day_of_yr_s + day_sq_s + rain:count_type +
                day_of_yr_s:count_type + day_sq_s:count_type + 
                min_past_sun_s,
              data = spdet_all, family = "poisson")
anova(nowind_spdetmod, pois_spdetmod, test = "Chisq")

# rain
norain_spdetmod <- glm(sp_detected ~ 1 + count_type + wind + noise +
                         day_of_yr_s + day_sq_s +
                         day_of_yr_s:count_type + day_sq_s:count_type + 
                         min_past_sun_s,
                       data = spdet_all, family = "poisson")

anova(norain_spdetmod, pois_spdetmod, test = "Chisq")

# noise
nonoise_spdetmod <- glm(sp_detected ~ 1 + count_type + wind + rain +
                          day_of_yr_s + day_sq_s + rain:count_type +
                          day_of_yr_s:count_type + day_sq_s:count_type + 
                          min_past_sun_s,
                        data = spdet_all, family = "poisson")

anova(nonoise_spdetmod, pois_spdetmod, test = "Chisq")

## END LINEAR REGRESSION MODEL---------------------------------------------------

if (regdiag){
## regression diagnostics-----------------------------------------------
# examining whether day and day^2 are collinear
plot(spdet_paired$day_of_yr_c, spdet_paired$day_sq)

#check for collinearity for all predictor variables
vif(spdetmod)
vif(pois_spdetmod)

#check residuals and model fit
resids <- rstandard(spdetmod)
hist(resids, main = "Standardized Residuals for sp_det model", xlab = 
       "standardized residuals")
boxplot(resids, main = "standardized residuals for sp_det model", ylab = 
         "standardize residuals")

presids <- rstandard(pois_spdetmod)
hist(presids, main = "Standardized Residuals for sp_det model", xlab = 
       "standardized residuals")
boxplot(presids, main = "standardized residuals for sp_det model", ylab = 
          "standardize residuals")


## test whole model for significance ------------------------------------------


## build null model to test whether whole pt_ct_richness_mod is significant
null_spdetmod <- glm(sp_detected ~ 1, data = spdet_paired,
                               family = "gaussian")

anova(null_spdetmod, spdetmod, test = "Chisq")

# test individual variables
# is polynomical term sigificant, or is a straight line just as good?
spdetmod_noPoly <- glm(sp_detected ~ 1 + count_type + wind + rain + noise +
                         day_of_yr_c + rain:count_type + min_past_sun, 
                        data = spdet_paired, family = "gaussian")
summary(spdetmod_noPoly)
anova(spdetmod_noPoly, spdetmod, test = "Chisq")

pois_spdetmod_nopoly <- glm(sp_detected ~ 1 + count_type + wind + rain + noise +
                              day_of_yr_c + rain:count_type + min_past_sun, 
                            data = spdet_paired, family = "poisson")
anova(pois_spdetmod_nopoly, pois_spdetmod, test = "Chisq")

## result of noPoly model suggests that polynomial term is not important for the 
## model! (with gaussian error distribution only) re-check assumptions for new 
## model with no polynomial term
#check for collinearity for all predictor variables
vif(spdetmod_noPoly)

#check residuals and model fit
resids <- rstandard(spdetmod_noPoly)
hist(resids, main = "Standardized Residuals for sp_det model without poly term",
     xlab = "standardized residuals")
boxplot(resids, main = "standardized residuals for sp_det model with no poly 
        term", ylab = "standardize residuals")
plot(spdetmod_noPoly)


## end test model significance -------------------------------------------------

##end Gaussian diagnostics------------------------------------------------------ 
}

if(poisdiag){
## POISSON model diagnostics----------------------------------------------------
# bar plot of frequency of # of sp detected
spcount <-  table(spdet_paired$sp_detected)
barplot(spcount, main = "distribution of species detected", xlab = "sp detected", 
        ylab = "count/frequency")

# check unconditional mean and variance for sp_detected (response variable)
# (we ultimately care only about CONDITIONAL mean and variance being equal after
# model is fit but this is a good indicator of whether it might be a problem)
mean(spdet_paired$sp_detected)
var(spdet_paired$sp_detected)
# doesn't look great! overdispersion may be a problem here.
# also check partially conditioned mean and variance by count type
aggregate(sp_detected~count_type, FUN=mean, data=spdet_paired)
aggregate(sp_detected~count_type, FUN=var, data=spdet_paired)

#look at deviance statistic of fit model divided by its d.f. to see if ratio
# is over 1
pois_spdetmod$deviance
pois_spdetmod$df.residual
#(this is the ratio we care about)
with(pois_spdetmod, deviance/df.residual)
#(this gives us a p-value for that ratio)
with(pois_spdetmod, pchisq(deviance, df.residual, lower.tail = FALSE))
## this shows us that we might have overdispersion; ratio is 1.4 instead of 1. 


# ## IF overdispersion is slight: fit robust standard errors using Huber's
# ## sandwich estimator of SEs
# 
# # calculate robust SEs for each term in model
# pois_cov <- vcovHC(pois_spdetmod, type= "HC")
# robust_SE <- sqrt(diag(pois_cov))
# robust_SE
# # get associated robust p-value
# 2*pnorm(abs(pois_spdetmod$coefficients/robust_SE), lower.tail = FALSE)
# # get robust 95% confidence intervals
# # lower bounds
# pois_spdetmod$coefficients - 1.96*robust_SE
# # upper bounds
# pois_spdetmod$coefficients + 1.96*robust_SE
# 
# #????? not sure how to evaluate if robust SEs are sufficient. 
# # IF overdispersion is too great for robust SEs, we refit model by SCALING SEs 
# # to a constant value by specifying "quasipoisson"
# qpois_spdetmod <- glm(sp_detected ~ 1 + count_type + wind + rain + noise +
#                        day_of_yr_c + day_sq + rain:count_type +
#                        min_past_sun, data = spdet_paired, 
#                      family = "quasipoisson")
# 
# summary(qpois_spdetmod)
# plot(qpois_spdetmod)
# 
# ## another option: fit a negative binomial model 
# nb_spdetmod <- glm.nb(sp_detected ~ 1 + count_type + wind + rain + noise +
#                         day_of_yr_c + day_sq + rain:count_type +
#                         min_past_sun, data = spdet_paired)
# summary(nb_spdetmod)
# plot(nb_spdetmod)
# 
# ##check to see if the nb model is a better fit than the poisson model
# pchisq(2 * (logLik(nb_spdetmod) - logLik(pois_spdetmod)), df = 1, 
#        lower.tail = FALSE)

### ULTIMATELY, all corrections for overdispersion give us results that look  
### roughly the same as original poisson model, so we will use original poisson, 
### since that is the simplest model

## Do we need mixed model or is the GLM w/Poisson distribution ok?
# Look for structure in residuals
#check residuals and model fit
resids_GLM_pois <- rstandard(pois_spdetmod)
hist(resids_GLM_pois, 
     main = "Standardized Residuals for pois_spdetmod model",
     xlab = "standardized residuals")
boxplot(resids_GLM_pois, main = "standardized residuals for pois_spdetmod model with no poly 
        term", ylab = "standardize residuals")

# look at residuals vs. fitted values
spdet_paired$resids <- resids_GLM_pois
spdet_paired$predicted <- predict(pois_spdetmod, newdata = spdet_paired)

# predicted vs. observed
ggplot(data = spdet_paired, aes(x = exp(predicted), y = sp_detected)) + 
  geom_point() + 
  ggtitle("Observed and expected number of species\nPoisson GLM")

# resids vs. fitted values
pts <- unique(spdet_paired$point_id)
ggplot(data = spdet_paired[spdet_paired$point_id %in% pts[11:18], ], 
       aes(x = predicted, y = resids, color = point_id)) + 
  geom_point(size = 3) + 
  ggtitle("residual vs. fitted values\nPoisson GLM")

ggplot(data = spdet_paired, aes(x = point_id, y = resids)) + 
  geom_boxplot() + 
  ggtitle("Residuals by point location\nPoisson GLM")

ggplot(data = spdet_paired, aes(x = day_of_yr_c, y = resids)) + 
  geom_point() + 
  geom_smooth() +
  ggtitle("Residuals by day of year\nPoisson GLM")

acf(spdet_paired$resids[order(spdet_paired$day_of_yr_c)])
## END Poisson model diagnostics------------------------------------------------
}
## ALL-ARU MODEL -----------------------------------------------------
## model to test whether aru_id is important
## species detected ~ a + beta*count_type + beta2*wind + beta3*noise + beta4*rain
##  + beta5*dayofyear + beta6*dayofyear^2 + beta7*count_type*rain
##  + beta8*minpastsunrise + beta9*aru_id

spdet_aru <- spdet_paired[which(spdet_paired$count_type == "aru"), ]

aruspdetmod <- glm(sp_detected ~ 1 + wind + rain + noise + aru_id +
                  day_of_yr_s + day_sq_s + min_past_sun_s, 
                  data = spdet_aru, 
                family = "poisson")

summary(aruspdetmod)
plot(aruspdetmod)

## none of the aru values appear significant, but now test significance of 
## overall term:
aruspdetmod2 <- update(aruspdetmod, . ~ . - aru_id)
anova(aruspdetmod, aruspdetmod2, test = "Chisq")
#models not significantly different from each other

##make sure both models meet poisson assumptions and are not overdispersed
aruspdetmod$deviance
aruspdetmod$df.residual
with(aruspdetmod, deviance/df.residual)
with(aruspdetmod, pchisq(deviance, df.residual, lower.tail = FALSE))

aruspdetmod2$deviance
aruspdetmod2$df.residual
with(aruspdetmod2, deviance/df.residual)
with(aruspdetmod2, pchisq(deviance, df.residual, lower.tail = FALSE))
# looks fine, I think. ratio is 1.5, which is greater than 1 but not 
# dramatically different from 1.4 (pois_spdetmod) above, and 1.4 was effectively
# no overdispersion.

#ultimately, we decide to exclude aru_id from the overall model

#BOXPLOT GOES HERE FOR SUPPLEMENTARY MATERIALS
#boxplot for sp detected by aru_id
aru_id_box <- ggplot(spdet_aru, aes(x=aru_id, y=sp_detected)) + 
  geom_boxplot() + 
  labs(x = "ARU ID", y = "Species detected", 
       title = "Number of Species Detected per Point Count by ARU ID")
aru_id_box

## END ALL ARU MODEL---------------------------------------------------

## GET CONFIDENCE INTERVALS FOR POIS_SPDETMOD----------------------------------

confint(pois_spdetmod)


## END GET CONFIDENCE INTERVALS FOR POIS_SPDETMOD------------------------------

#PLOT: species detected per count over time, by count type
spdet_time_ctype <- ggplot(spdet_all, aes(x=day_of_yr_s, y=sp_detected, 
                                             color=count_type)) +  
  geom_point(shape=1) +
  theme_bw() +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  scale_y_continuous(breaks = c(0, 4, 8, 12)) +
  geom_smooth(method="glm",method.args=list(family=poisson),  
              se=TRUE)     
spdet_time_ctype

#PLOT: look at species detected per count by count type for each point location
pts_spdet <- ggplot(data = spdet_all, aes(x = day_of_yr_s, y = sp_detected, 
                                             color = factor(count_type))) + 
  geom_smooth(method = "glm", se = T) + 
  geom_point() +
  scale_y_continuous(breaks = c(0, 5, 10, 15)) +
  facet_wrap(~ factor(point_id))
pts_spdet



