################################
## 20 random minutes ARU data- prep for Mixed models, Point Abbaye 2019 data
## 
## author: Ellie Roark
## created: 5 Feb 2020
## last modified: 5 Feb 2020
## 
## inputs: *ARU20randmin.csv- file with bird species identifications from 20 
##          random minutes of aru data for each recorder for each survey day
##         *filename_key_20randmin.csv- key containing date, original file name
##          and other essential information to de-anonymize the file names in 
##          ARU20randmin.csv
##         
## outputs: *
##            
## TODO: * check through de-anonymized data and ensure there are no NAs
################################

### read in random minute aru observations and file name key

arurand <- read_csv(file = "./data/ARU20randmin_final.csv")
filenames <- read_csv(file = "./data/filename_key_20randmin.csv", 
                      col_types = cols(.default = "?", folder = "c"))

##cut columns down to just anon_filename and species_code
arurand <- subset(arurand, select = c(filename, species_code))

##change filename to anon_name in arurand
arurand <- rename(arurand, anon_name = "filename")

### un-anonymize the aru file names---------------------------------------------

#get rid of columns we don't need in filenames
filenames <- subset(filenames, select = c(selec, sound.files, anon_name, date))

#join filenames to allaru to add a column that has the un-anonymized file names
arurand <- left_join(arurand, filenames, by = "anon_name")

### end un-anonymize------------------------------------------------------------

### add point id, count type, aru id, weather, aru_sample columns---------------

#create dataframe with key for the anonymized recorder names
rec <- c("rec1", "rec2", "rec3", "rec4")
aru_id <- c("swift02", "swift01", "AM", "swift03")
recs <- data.frame(aru_id, rec)

#create an aru_id col for allaru
arurand$rec <- gsub("day.{2,3}_?", "", arurand$anon_name)
arurand$rec <- gsub("_.*_.*", "", arurand$rec)

#de-anonymize aru_id
arurand <- left_join(arurand, recs, by = "rec")

#remove "rec" column-- aru_id is all that's needed
arurand$rec <- NULL

# fetch the point id for a particular date and aru from the allaru df
locs <- data.frame(allaru$point_id, allaru$date, allaru$aru_id)
locs <- rename(locs, point_id = "allaru.point_id", date = "allaru.date", 
               aru_id = "allaru.aru_id")

#create an aruday column that says both the date and the aru
locs$aruday <- paste(as.character(locs$date), as.character(locs$aru_id), 
                     sep = "_")
arurand$aruday <- paste(as.character(arurand$date), as.character(arurand$aru_id), 
                       sep = "_")

#get rid of unnecessary columns and rows in locs
locs$date <- NULL
locs$aru_id <- NULL
locs <- unique(locs)

#add point id to arurand by joining locs and arurand by aruday
arurand <- left_join(arurand, locs, by = "aruday")

#add locations to arudays 2019-04-16_swift02 and 2019-05-05_swift01 
# these days had a five hour ARU rec but no paired in person/aru point count
# so their locations are not in the allaru or ptct dataframes
arurand[which(arurand$aruday == "2019-05-05_swift01"), "point_id"] <- "p3"
arurand[which(arurand$aruday == "2019-04-16_swift02"), "point_id"] <- "p13"
##NB: because these units on these days did not have in person surveys, there is
## no wind, rain or noise rating. they will recieve the daily average wind
## rating for inclusion in the abundance models but will be dropped from the 
## species richness models since they do not have the required weather variables
## create drop object to exclude these days from species richness models
drop <- c("2019-05-05_swift01", "2019-04-16_swift02")

#add a "ptct_id" column that pastes day and point_id (location) together
arurand$ptct_id <- paste(as.character(arurand$date), arurand$point_id, sep = "_")

# create a new df with just weather variables, aru_id and date from ptct data
weathervar <- allaru[ , c("aru_id", "date", "wind", "rain", "noise")]
# create aruday for weathervar
weathervar$aruday <- paste(as.character(weathervar$date), weathervar$aru_id, 
                           sep = "_")

#use only unique rows of weather data
weathervar <- distinct(weathervar)

#remove aru_id and date, since they are no longer needed in weathervar
weathervar$aru_id <- NULL
weathervar$date <- NULL

# join to arurand df to add weather and start time variables
arurand <- left_join(arurand, weathervar, by = "aruday")

# make aru_sample col
arurand$aru_sample <- TRUE

### end add pt id, count type, aru_id, etc. cols--------------------------------

## get unique values of selec for each aruday
min <- arurand %>% group_by(aruday) %>% distinct(selec)
#table(min$aruday)

##randomly sample 10 rand min from each aruday and 22 rand min from each aruday
# get 10 random min for each aruday from "min" df 
s10r <- sample_n(min, 10, replace = FALSE)

# get 22 random min for each aruday from "min" df
s22r <- sample_n(min, 22, replace = FALSE)

# get dataframe with all species observations from the 22 randomly selected mins
# for each aruday
aru22r <- semi_join(arurand, s22r, by = c("selec", "aruday"))

# get dataframe with all species observations from the 10 randomly selected mins
# for each aruday
aru10r <- semi_join(arurand, s10r, by = c("selec", "aruday"))


### add up species detected per 20 random minutes-------------------------------
## make new df that will have one row per aruday (effectively a count id). 
# get unique arudays
aruday <- unique(arurand$aruday)
# make sp_detected col
sp_detected <- NA
# combine them into df
spdet_22r <- data.frame(aruday, sp_detected)

## for loop that will add up the number of species detected per point count
for (i in 1:nrow(spdet_22r)){
  thisct <- spdet_22r$aruday[i]
  thisdat <- aru22r[aru22r$aruday == thisct, ]
  n_sp <- length(unique(thisdat$species_code))
  if (any(thisdat$species_code == "no birds")){
    spdet_22r$sp_detected[i] <- n_sp-1
  }
  else {spdet_22r$sp_detected[i] <- n_sp}
}

spdet_10r <- data.frame(aruday, sp_detected)
## add up species detected per 10 random min 
for (i in 1:nrow(spdet_10r)){
  thisct <- spdet_10r$aruday[i]
  thisdat <- aru10r[aru10r$aruday == thisct, ]
  n_sp <- length(unique(thisdat$species_code))
  if (any(thisdat$species_code == "no birds")){
    spdet_10r$sp_detected[i] <- n_sp-1
  }
  else {spdet_10r$sp_detected[i] <- n_sp}
}

#create count type col
spdet_10r$count_type <- "aru_10r"
spdet_22r$count_type <- "aru_22r"

### end total species detected per count ---------------------------------------

### merge with spdet_paired dataframe-----------------------------------------
## join spdet_10r and spdet_22r
spdet_r <- full_join(spdet_10r, spdet_22r)
## PREP for join-- add missing variables back to spdet20r
# create dfs with place and date variables
covs <- select(arurand, c("aruday", "date", "point_id", "ptct_id", "aru_id",
                          "wind", "rain", "noise", "aru_sample"))

# get rid of dupicate rows in the variable dataframes
covs <- distinct(covs)

# join variable dfs to spdet dfs
spdet_r <- left_join(spdet_r, covs, by = "aruday")

## drop the two days with no associated weather variables from the spdet_r df
spdet_r <- spdet_r[spdet_r$aruday %nin% drop, ]

##create day of year and min past sun cols in spdet_20r
spdet_r$day_of_yr <- yday(spdet_r$date)
#spdet_r$min_past_sun <- NA

# actually merge spdet_aru and spdet_ptct
spdet_4ct <- full_join(spdet_paired, spdet_r, by = NULL)

### end merge dataframes--------------------------------------------------------

## organize df so that it's sorted by date and ptctid
spdet_4ct <- spdet_4ct[order(spdet_4ct$date, spdet_4ct$ptct_id),]


### calculate species accumulation curves ------------------------------------
# make array to hold each aru/day combo, with a row for each minute (22), and\
# a column for each replicate resample (100).  
arudays <- unique(aru22r$aruday) # 126 aru/day combinations
sp_accum <- vector("list", length(arudays))
names(sp_accum) <- arudays

for(sl in 1:length(sp_accum)) {
  # make this element of the list a df with a row for each minute and a column
  # for each replicate resamples
  sp_accum[[sl]] <- data.frame(matrix(nrow = 22, ncol = 100))
  # get data for this aru/day 
  tdat <- aru22r[aru22r$aruday == names(sp_accum)[sl], ]
  
  for(rp in 1:100) { # loop through replicate samples
    # draw resample
    sampdat <- tdat[sample(1:nrow(tdat), size = nrow(tdat), replace = F), ]
    mins <- unique(sampdat$selec) # get names of minutes in this aru/day combo
    if(length(mins) != 22) warning(paste0("Number of minutes is not 22, sl = ", 
                                          sl))
    sp_list <- c() # initialize vector of species detected
    
    for(mn in 1:length(mins)) { # loop through minutes
      tmin <- sampdat[sampdat$selec == mins[mn], ]
      sp_list <- c(sp_list, tmin$species_code)
      # remove non-species observations
      sp_list <- sp_list[sp_list != "no birds"]
      sp_list <- sp_list[!grepl(".* sp.", sp_list)]
      sp_accum[[sl]][mn, rp] <- length(unique(sp_list))
    }
  }
  # add columns for minutes, date, and aruday code
  sp_accum[[sl]]$minute <- 1:22
  if(length(unique(tdat$date)) != 1) stop("multiple dates in tdat") 
  sp_accum[[sl]]$date <- unique(tdat$date)[1]
  if(length(unique(tdat$aruday)) != 1) stop("multiple arudays in tdat") 
  sp_accum[[sl]]$aruday <- unique(tdat$aruday)[1]
  
  # make long format (for easier plotting later)
  sp_accum[[sl]] <- pivot_longer(sp_accum[[sl]], 1:100, names_to = "replicate", 
                                 values_to = "n_species_detected")
}
sp_accum <- bind_rows(sp_accum)
sp_accum$unique_replicate <- paste0(sp_accum$replicate, sp_accum$aruday)

ggplot(data = sp_accum, aes(x = minute, y = n_species_detected, 
                            group = unique_replicate, 
                            color = date)) + 
  geom_line()

# make columns indicating date bins
sp_accum$April2_April14 <- sp_accum$date <= "2019-04-14"
sp_accum$April15_April27 <- sp_accum$date > "2019-04-14" & 
  sp_accum$date <= "2019-04-27"
sp_accum$April28_May9 <- sp_accum$date > "2019-04-27" & 
  sp_accum$date <= "2019-05-09"
sp_accum$May10_May22 <- sp_accum$date > "2019-05-09" 


# ## old method
# sp_accum <- array(data = NA, dim = c(22, 100, length(arudays)), 
#                   dimnames = list(minute = 1:22, replicate = 1:100, 
#                                   aruday = arudays))
# 
# # loop through aru/day combos, replicates, and rows to build data
# for(sl in 1:dim(sp_accum)[3]) { # loop through slices (aru/day combos)
#   # get data for this aru/day 
#   tdat <- aru22r[aru22r$aruday == arudays[sl], ]
#   for(rp in 1:dim(sp_accum)[2]) { # loop through replicate resamples
#     # draw resample
#     tdat <- tdat[sample(1:nrow(tdat), size = nrow(tdat), replace = F), ]
#     mins <- unique(tdat$selec) # get names of minutes in this aru/day combo
#     if(length(mins) != 22) warning(paste0("Number of minutes is not 22, sl = ", 
#                                           sl))
#     sp_list <- c() # initialize vector of species detected
#     for(mn in 1:dim(sp_accum)[1]) { # loop through minutes
#       tmin <- tdat[tdat$selec == mins[mn], ]
#       sp_list <- c(sp_list, tmin$species_code)
#       # remove non-species observations
#       sp_list <- sp_list[sp_list != "no birds"]
#       sp_list <- sp_list[!grepl(".* sp.", sp_list)]
#       sp_accum[mn, rp, sl] <- length(unique(sp_list))
#     }
#   }
#   # add date column 
#   sp_accum[, dim(sp_accum[, , sl])[2] + 1, sl] <- 
# }


# # test
# plot(x = 0:22, y = 0:22)
# 
# for(sl in 1:dim(sp_accum)[3]) {
#   for (rp in 1:dim(sp_accum)[2]) {
#       lines(x = names(sp_accum[, rp, sl]), y = sp_accum[, rp, sl])
#   }
# }
# # end test

### end calculate species accumulation curves --------------------------------



### clean up environment
rm(covs, aru10r, aru22r, filenames, locs, recs, s10r, s22r, spdet_22r, spdet_aru, 
   spdet_10r, spdet_ptct, thisdat, weathervar, aru_id, aruday, n_sp, rec, 
   sp_detected, thisct, tdat, tmin, min)






