################################
## GLMM- Predicting species detection with 20 random minute counts
## Point Abbaye 2019
## 
## author: Ellie Roark
## created: 7 Feb 2020
## last modified: 28 Apr 2020
## 
## inputs: *
##
##         
## outputs: *max.rand.spdetmm - mixed model predicting the number of species 
##          detected based on count type, timing and weather variables
##            
## TODO: * 
################################

library(tidyverse)
library(lme4)
library(car)

plotson <- FALSE


#change wind from dbl to factor with 3 levels (0-1, 2, 3+)
spdet_4ct[which(spdet_4ct$wind <= "1"), "wind"] <- "0-1"
spdet_4ct[which(spdet_4ct$wind >= "3"), "wind"] <- "3+"
spdet_4ct$wind <- factor(as.character(spdet_4ct$wind), 
                            levels = c("0-1", "2", "3+"))

#change rain into factor with two groups, wet and dry
spdet_4ct[which(spdet_4ct$rain == "Rain/Snow"), "rain"] <- "wet"
spdet_4ct[which(spdet_4ct$rain == "Drizzle"), "rain"] <- "wet"
spdet_4ct[which(spdet_4ct$rain == "Fog"), "rain"] <- "wet"
spdet_4ct$rain <- factor(as.character(spdet_4ct$rain), 
                            levels = c("Dry", "wet"))

#change noise into factor variable with three groups: 0, 1, >2
spdet_4ct[which(spdet_4ct$noise >= 2), "noise"] <- ">2"
spdet_4ct$noise <- factor(as.character(spdet_4ct$noise), 
                             levels = c("0", "1", ">2"))

#make aru_id into factor variable
spdet_4ct$aru_id <- factor(as.character(spdet_4ct$aru_id), 
                              levels = c("swift01", "swift02", "swift03", "AM",
                                         "none"))

# make a centered day of year and a squared day of year
spdet_4ct$day_of_yr_c <- spdet_4ct$day_of_yr - mean(spdet_4ct$day_of_yr)
spdet_4ct$day_sq <- spdet_4ct$day_of_yr_c^2

# center min_past_sun
spdet_4ct$min_past_sun_c <- spdet_4ct$min_past_sun - 
  mean(spdet_4ct$min_past_sun)

# make scaled versions of all continuous variables
spdet_4ct$day_of_yr_s <- scale(spdet_4ct$day_of_yr_c, 
                                  center = F, scale = T)[, 1] # funny subsetting
# because scale 
# returns a matrix
spdet_4ct$day_sq_s <- spdet_4ct$day_of_yr_s^2
spdet_4ct$min_past_sun_s <- scale(spdet_4ct$min_past_sun_c, center = F, 
                                     scale = T)[, 1]

# make count type a factor variable
spdet_4ct$count_type <- factor(as.character(spdet_4ct$count_type), 
                                  levels = c("point", "aru", "aru_10r", 
                                             "aru_22r"))

# make point_id a factor variable
spdet_4ct$point_id <- factor(as.character(spdet_4ct$point_id), 
                                levels = c("p1", "p2", "p3", "p4", "p5", "p6",
                                           "p8", "p9", "p10", "p11", "p12", 
                                           "p13","p14", "p15", "p17", "p18", 
                                           "FB1", "FB2"))

##end coerce data to desired format and regroup variables-----------------------

if(plotson){
  ## EXPLORATORY PLOTS -----------------------------------------------------------
  #hist of frequency of # of sp detected
  obsfreq_hist <- hist(spdet_4ct$sp_detected, main = "Species Detected")
  
  #boxplot for sp detected by each count type
  count_type_box <- ggplot(spdet_4ct, aes(x=count_type, y=sp_detected)) + 
    geom_boxplot() + scale_y_continuous(breaks = c(1, 5, 10)) +
    labs(x = "Count type", y = "Number of species detected", 
         title = "Number of Species Detected per Point Count by Count Type")
  count_type_box
  
  aprcount_type_box <- ggplot(spdet_4ct[which(
    spdet_4ct$date <= "2019-05-01"), ], aes(x=count_type, y=sp_detected)) + 
    geom_boxplot() + labs(title = "Number of Species Detected by Count Type")
  
  #boxplot for sp detected by aru_id
  aru_id_box <- ggplot(spdet_4ct, aes(x=aru_id, y=sp_detected)) + 
    geom_boxplot() + labs(x = "ARU ID", y = "Species detected", 
                          title = 
                          "Number of Species Detected per Point Count by ARU ID")
  aru_id_box
  
  #boxplot for species detected per count, total
  allobs_box <- ggplot(spdet_4ct, aes(y=sp_detected)) + geom_boxplot()
  allobs_box
  
  #boxplot for species detected at each point location
  point_id_box <- ggplot(spdet_4ct, aes(x=point_id, y=sp_detected)) + 
    geom_boxplot() + labs(title = "Number of Species Detected at each Point loc")
  point_id_box
  
  #qqplot for species detected
  qqnorm(spdet_4ct$sp_detected, main = "QQ Plot for all species detected")
  qqline(spdet_4ct$sp_detected)
  
  #plot species detected per rain level
  rain_box <- ggplot(spdet_4ct, aes(x=rain, y=sp_detected)) + geom_boxplot() +
    labs(title = "Species detected over precipitation type")
  rain_box
  
  #plot species detected per wind level
  wind_box <- ggplot(spdet_4ct, aes(x=wind, y=sp_detected)) + geom_boxplot() +
    labs(title = "Species detected over wind level")
  wind_box
  
  #plot species detected per noise level
  noise_box <- ggplot(spdet_4ct, aes(x=noise, y=sp_detected)) + geom_boxplot() +
    labs(title = "Species detected over noise level")
  noise_box
  
  # plot of sp detected over time
  doy_spdet <- plot(spdet_4ct$day_of_yr, spdet_4ct$sp_detected, 
                    main = "Species per count over time")
  abline(lm(sp_detected~day_of_yr, data = spdet_4ct),
         col = "chartreuse4")
  with(spdet_4ct, lines(lowess(day_of_yr, sp_detected)
                           , col = "darkblue"))
  
  #sp detected over time, stratified by count type
  spdet_time_ctype <- ggplot(spdet_4ct, aes(x=day_of_yr, y=sp_detected, 
                                               color=count_type)) +  
    geom_point(shape=1) +
    scale_colour_hue(l=50) + # Use a slightly darker palette than normal
    geom_smooth(method="glm",method.args=list(family=poisson),  
                se=TRUE)     # Add confidence interval shading
  spdet_time_ctype
  
  spdet_time_ctype_loess <- ggplot(spdet_4ct, aes(x=day_of_yr, y=sp_detected, 
                                                     color=count_type)) +  
    theme_bw() +
    geom_point(shape=1) +
    scale_colour_hue(l=50) + # Use a slightly darker palette than normal
    geom_smooth(method="loess",  
                se=TRUE)     # Add confidence interval shading
  spdet_time_ctype_loess
  
  tod_spdet <- plot(spdet_4ct$min_past_sun, spdet_4ct$sp_detected,
                    main = "species per count by minutes past sunrise")
  
  #look at response variable
  describe(spdet_4ct$sp_detected)
  
  #look at histograms for each count type
  ggplot(spdet_4ct, aes(sp_detected, fill = count_type)) + 
    geom_histogram(binwidth = 1) + 
    facet_grid(count_type ~ ., margins = TRUE, scales = "free")
  
  #look at pairs plot for all data
  spdet_4ct <- spdet_4ct %>% ungroup()
  spdet_all_covs <- dplyr::select(spdet_4ct, c("wind", "rain", "noise", "day_sq",
                                        "day_of_yr_c", "min_past_sun"))
  
  pairplot <- ggpairs(spdet_all_covs, columns = 1:5)
}

## END EXPLORATORY PLOTS -------------------------------------------------------

## check assumptions and test distributions of GLM to decide which error 
## distribution to use for GLMM
norm.4ct <- glm(sp_detected ~ 1 + count_type + wind + rain + noise +
                  day_of_yr_s + day_sq_s + rain:count_type +
                  day_of_yr_s:count_type + day_sq_s:count_type, 
                data = spdet_4ct, 
                family = "gaussian")

summary(norm.4ct)
#plot(spdetmod, ask = FALSE)
## normal distribution looks not great (as expected). Residuals show distinctive
## shape. SO we try Poisson
pois.4ct <- glm(sp_detected ~ 1 + count_type + wind + rain + noise +
                       day_of_yr_s + day_sq_s + rain:count_type +
                       day_of_yr_s:count_type + day_sq_s:count_type,
                     data = spdet_4ct, family = "poisson")

summary(pois.4ct)
#plot(pois.4ct, ask = FALSE)

## check poisson assumptions
# check unconditional mean and variance for sp_detected (response variable)
# (we ultimately care only about CONDITIONAL mean and variance being equal after
# model is fit but this is a good indicator of whether it might be a problem)
mean(spdet_4ct$sp_detected)
var(spdet_4ct$sp_detected)
# doesn't look great! overdispersion may be a problem here.
# also check partially conditioned mean and variance by count type
aggregate(sp_detected~count_type, FUN=mean, data=spdet_4ct)
aggregate(sp_detected~count_type, FUN=var, data=spdet_4ct)

#look at deviance statistic of fit model divided by its d.f. to see if ratio
# is over 1
pois.4ct$deviance
pois.4ct$df.residual
#(this is the ratio we care about)
with(pois.4ct, deviance/df.residual)
#(this gives us a p-value for that ratio)
with(pois.4ct, pchisq(deviance, df.residual, lower.tail = FALSE))
## this shows us that we might have overdispersion; ratio is 1.2 instead of 1, 
## and p-value is 0.00002

## IF overdispersion is slight: fit robust standard errors using Huber's
## sandwich estimator of SEs

# calculate robust SEs for each term in model
pois_cov <- vcovHC(pois.4ct, type= "HC")
robust_SE <- sqrt(diag(pois_cov))
robust_SE
# get associated robust p-value
2*pnorm(abs(pois.4ct$coefficients/robust_SE), lower.tail = FALSE)
# get robust 95% confidence intervals
# lower bounds
pois.4ct$coefficients - 1.96*robust_SE
# upper bounds
pois.4ct$coefficients + 1.96*robust_SE

#????? not sure how to evaluate if robust SEs are sufficient.
# IF overdispersion is too great for robust SEs, we refit model by SCALING SEs
# to a constant value by specifying "quasipoisson"
qpois.4ct <- glm(sp_detected ~ 1 + count_type + wind + rain + noise +
                  day_of_yr_s + day_sq_s + rain:count_type + 
                  day_of_yr_s:count_type + day_sq_s:count_type, 
                 data = spdet_4ct,
                     family = "quasipoisson")

summary(qpois.4ct)
#plot(qpois.4ct)



##GLMM with 4 count types-------------------------------------------------------
rand.spdetmm <- glmer(sp_detected ~ count_type + wind + rain + noise +
                      day_of_yr_s + day_sq_s + rain:count_type +
                      day_of_yr_s:count_type + day_sq_s:count_type +
                     (1|day_of_yr_s), 
                    data = spdet_4ct,
                    family = poisson)

#this model does not converge: troubleshooting: 
##check for singularity
tt <- getME(rand.spdetmm,"theta")
ll <- getME(rand.spdetmm,"lower")
min(tt[ll==0])
# if this number is VERY small (approaching 0) we have a singularity problem
# because it is not, we likely do not

##next test: double check gradient calculations
derivs1 <- rand.spdetmm@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))

max(pmin(abs(sc_grad1),abs(derivs1$gradient)))
# apparently the typical tolerance for this value is max 0.001, and my value is
# below that, so that's good.(?) 

## next test: restart from previous fit, bumping up max number of configurations
ss <- getME(rand.spdetmm,c("theta","fixef"))
max.rand.spdetmm <- update(rand.spdetmm,start=ss,control=glmerControl(
  optCtrl=list(maxfun=2e4)))
# model now converges. 

summary(max.rand.spdetmm)

## get 95% confidence intervals for coefficient estimates for for final model 
CI.maxrand <- confint.merMod(max.rand.spdetmm, level = 0.95, method = "Wald")


## end GLMM with 4 ct types-----------------------------------------------------

#plot SR over time, stratified by count type
spdet_time_4ct <- ggplot(spdet_4ct, aes(x=day_of_yr, y=sp_detected, 
                                             color=count_type)) +  
  geom_point() +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method="loess", 
              se=TRUE)     # Add confidence interval shading
spdet_time_4ct


## clean up workspace 
rm(allaru, allobs_box, alpha_codes, aprcount_type_box, aru_id_box, aru_names, 
   arurand, aruspdetmod, aruspdetmod2, CI.r1, count_type_box, derivs1, noise_box,
   non_species, nonoise_spdetmod, nonoise.spdetmm, norain_spdetmod, 
   norain.spdetmm, norm.4ct, nowind_spdetmod, nowind.spdetmm, nullpois_spdetmod,
   obsfreq_hist, onlyaru, onlypt, pairplot, point_id_box, pois_cov, pois_spdetmod, 
   pois.4ct, ptct, pts_spdet, qpois.4ct, r1.spdetmm, rain_box, rand.spdetmm, sdf, 
   sp_detection_method, spdet_all, spdet_all_covs, spdet_aru, spdet_paired, 
   spdet_r, spdet_time_4ct, spdet_time_ctype, spdet_time_ctype_loess, spdetmod,
   ss, wind_box, arudet, arudiag, doy_spdet, drop, drop_counts, keep_sp, ll, 
   lm_on, mmdiag, plotson, poisdiag, ptdet, regdiag, robust_SE, sc_grad1, 
   tod_spdet, tt)
