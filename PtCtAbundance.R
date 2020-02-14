################################
## Abundance estimation for Point Counts
## Point Abbaye 2019 data
## 
## author: Ellie Roark
## created: 10 Feb 2020
## last modified: 11 Feb 2020
## 
## inputs: *MUST FIRST RUN DataPrep_SR~counttype_10mincts_2019.R
##          AND ARUDuplicateReview2019.R
##          
##
##         
## outputs: *
##            
## TODO: * 
################################


ptct$count <- as.numeric(ptct$count)
obsfreq_hist <- hist(ptct$count, main = "number of individs")

#boxplot for sp detected by each count type
count_type_box <- ggplot(spdet_paired, aes(x=count_type, y=sp_detected)) + 
  geom_boxplot() + scale_y_continuous(breaks = c(1, 5, 10)) +
  labs(x = "Count type", y = "Number of species detected", 
       title = "Number of Species Detected per Point Count by Count Type")
count_type_box


# make day of yr col for ptct obs
ptct$day_of_yr <- yday(ptct$date)

#subset to only sp observations of WIWR
wiwr <- ptct[which(ptct$species_code == "WIWR"), ]

#subset to ybsa
ybsa <- ptct[which(ptct$species_code == "YBSA"), ]

#subset to Bcch
bcch <- ptct[which(ptct$species_code == "BCCH"), ]

#subset to heth
heth <- ptct[which(ptct$species_code == "HETH"), ]

#subset to gcki
gcki <- ptct[which(ptct$species_code == "GCKI"), ]

# plot wren counts over time
plot(wiwr$day_of_yr, wiwr$count, 
                  main = "wiwr per pt count over time")

plot(ybsa$day_of_yr, ybsa$count, 
                  main = "ybsa per pt count over time")

plot(bcch$day_of_yr, bcch$count, 
     main = "bcch per pt count over time")

plot(heth$day_of_yr, heth$count, 
     main = "heth per pt count over time")

plot(gcki$day_of_yr, gcki$count, 
     main = "gcki per pt count over time")

# add up number of individs detected per day for each 
sum_wiwr <- wiwr %>% 
  group_by(day_of_yr) %>% 
  summarise(count = n())

sum_ybsa <- ybsa %>% 
  group_by(day_of_yr) %>% 
  summarise(count = n())

sum_bcch <- bcch %>% 
  group_by(day_of_yr) %>% 
  summarise(count = n())

sum_heth <- heth %>% 
  group_by(day_of_yr) %>% 
  summarise(count = n())

sum_gcki <- gcki %>% 
  group_by(day_of_yr) %>% 
  summarise(count = n())

plot(sum_gcki$day_of_yr, sum_gcki$count, 
     main = "gcki per day over time")
plot(sum_ybsa$day_of_yr, sum_ybsa$count, 
     main = "ybsa per day over time")
plot(sum_wiwr$day_of_yr, sum_wiwr$count, 
     main = "wiwr per day over time")
plot(sum_bcch$day_of_yr, sum_bcch$count, 
     main = "bcch per day over time")
plot(sum_heth$day_of_yr, sum_heth$count, 
     main = "heth per day over time")
