################################
## GLMM- Predicting species detection with 20 random minute counts
## Point Abbaye 2019
## 
## author: Ellie Roark
## created: 7 Feb 2020
## last modified: 7 Feb 2020
## 
## inputs: *
##
##         
## outputs: *r1.spdetmm - mixed model predicting the number of species detected
##            based on count type, timing and weather variables
##            
## TODO: * 
################################

library(tidyverse)
library(lme4)
library(car)

set.seed(3271989)

#change wind from dbl to factor with 3 levels (0-1, 2, 3+)
spdet_3ct[which(spdet_3ct$wind <= "1"), "wind"] <- "0-1"
spdet_3ct[which(spdet_3ct$wind >= "3"), "wind"] <- "3+"
spdet_3ct$wind <- factor(as.character(spdet_3ct$wind), 
                            levels = c("0-1", "2", "3+"))

#change rain into factor with two groups, wet and dry
spdet_3ct[which(spdet_3ct$rain == "Rain/Snow"), "rain"] <- "wet"
spdet_3ct[which(spdet_3ct$rain == "Drizzle"), "rain"] <- "wet"
spdet_3ct[which(spdet_3ct$rain == "Fog"), "rain"] <- "wet"
spdet_3ct$rain <- factor(as.character(spdet_3ct$rain), 
                            levels = c("Dry", "wet"))

#change noise into factor variable with three groups: 0, 1, >2
spdet_3ct[which(spdet_3ct$noise >= 2), "noise"] <- ">2"
spdet_3ct$noise <- factor(as.character(spdet_3ct$noise), 
                             levels = c("0", "1", ">2"))

#make aru_id into factor variable
spdet_3ct$aru_id <- factor(as.character(spdet_3ct$aru_id), 
                              levels = c("swift01", "swift02", "swift03", "AM",
                                         "none"))

# make a centered day of year and a squared day of year
spdet_3ct$day_of_yr_c <- spdet_3ct$day_of_yr - mean(spdet_3ct$day_of_yr)
spdet_3ct$day_sq <- spdet_3ct$day_of_yr_c^2

# center min_past_sun
spdet_3ct$min_past_sun_c <- spdet_3ct$min_past_sun - 
  mean(spdet_3ct$min_past_sun)

# make scaled versions of all continuous variables
spdet_3ct$day_of_yr_s <- scale(spdet_3ct$day_of_yr_c, 
                                  center = F, scale = T)[, 1] # funny subsetting
# because scale 
# returns a matrix
spdet_3ct$day_sq_s <- spdet_3ct$day_of_yr_s^2
spdet_3ct$min_past_sun_s <- scale(spdet_3ct$min_past_sun_c, center = F, 
                                     scale = T)[, 1]

# make count type a factor variable
spdet_3ct$count_type <- factor(as.character(spdet_3ct$count_type), 
                                  levels = c("point", "aru", "arurand"))

# make point_id a factor variable
spdet_3ct$point_id <- factor(as.character(spdet_3ct$point_id), 
                                levels = c("p1", "p2", "p3", "p4", "p5", "p6",
                                           "p8", "p9", "p10", "p11", "p12", 
                                           "p13","p14", "p15", "p17", "p18", 
                                           "FB1", "FB2"))

##end coerce data to desired format and regroup variables-----------------------

##GLMM with 3 count types-------------------------------------------------------
r20.spdetmm <- glmer(sp_detected ~ count_type + wind + rain + noise +
                      day_of_yr_s + day_sq_s + rain:count_type +
                      day_of_yr_s:count_type + day_sq_s:count_type +
                     (1|day_of_yr_s), 
                    data = spdet_3ct,
                    family = poisson, 
                    start = r1.spdetmm@theta )
summary(r20.spdetmm)


## end GLMM with 3 ct types-----------------------------------------------------

#plot SR over time, stratified by count type
spdet_time_3ct <- ggplot(spdet_3ct, aes(x=day_of_yr, y=sp_detected, 
                                             color=count_type)) +  
  geom_point() +
  scale_colour_hue(l=50)  # Use a slightly darker palette than normal
  #geom_smooth(method="loess", 
  #            se=TRUE)     # Add confidence interval shading
spdet_time_3ct
