################################
## Data Prep for Abundance estimation for ARU counts
## Point Abbaye 2019 data
## 
## author: Ellie Roark
## created: 9 Mar 2020
## last modified: 18 Mar 2020
## 
## inputs: *Corrected_PointCounts_PtAbbaye2019.csv- original data file with all 
##           point count data from the 2019 field season at Point Abbaye
##         *SunriseTimes_PtAbbaye2019.csv- csv with sunrise times from the US
##           Navy (see citation list for details)          
##           
## outputs: *
##            
## TODO: * 
################################

library(Hmisc)
library(tidyverse)
library(lubridate)

#read in 10 consecutive min ARU data
widearu <- read_csv(file = "./2019data/ARUPointCounts_WIDE_PtAbbaye2019.csv")
longaru <- read_csv(file = "./2019data/ARUPointCounts_PtAbbaye2019.csv")
filenames <- read_csv(file = "./2019data/anonymized_file_key.csv")

#read in 22 rand min data
arurand <- read_csv(file = "./2019data/ARU20randmin_final.csv")
filenames22r <- read_csv(file = "./2019data/filename_key_20randmin.csv", 
                      col_types = cols(.default = "?", folder = "c"))

#### 10 minute ARU count data prep (GCKI)---------------------------------------
#subset longaru to gcki
aru_gcki <- longaru[which(longaru$species_code == "GCKI"), ]

#subset widearu to gcki
aru_gckiw <- widearu[which(widearu$species_code == "GCKI"), ]

#get unique ptctids for counts on which there were no GCKIs
lnogcki <- longaru[which(longaru$anon_file_name %nin% aru_gcki$anon_file_name), ]
wnogcki <- widearu[which(widearu$anon_filename %nin% aru_gckiw$anon_filename), ]

#create a count column for the nogcki dfs
lnogcki$count <- 0
wnogcki$count <- 0

#get list of anon filenames with 
keepw <- c("anon_filename", "count")
keepl <- c("anon_file_name", "count")
lnogcki <- lnogcki[ , (names(lnogcki) %in% keepl)]
wnogcki <- wnogcki[ , (names(wnogcki) %in% keepw)]

# get only a single row for each 10 min aru count (counts with no GCKI)
wnogcki <- dplyr::distinct(wnogcki)
lnogcki <- dplyr::distinct(lnogcki)

## add up number of 30 sec periods in which a GCKI was detected- wide aru
aru_gckiw[is.na(aru_gckiw)] <- "0"
aru_gckiw[4:23] <- as.integer(aru_gckiw[4:23] != 0)
aru_gckiw <- aru_gckiw %>%
  mutate(count = rowSums(.[4:23]))
#subset to only count and anon_filename cols
aru_gckiw <- aru_gckiw[ , (names(aru_gckiw) %in% keepw)]
#add .wav extension to filenames
aru_gckiw$anon_filename <- paste(aru_gckiw$anon_filename, ".wav", sep = "")
wnogcki$anon_filename <- paste(wnogcki$anon_filename, ".wav", sep = "")

## add up number of 30 sec periods in which a GCKI was detected- long aru
# change minute half values from 1 + 2 to :00 and :30
aru_gcki[which(aru_gcki$minute_half == "1"), "minute_half"] <- ":00"
aru_gcki[which(aru_gcki$minute_half == "2"), "minute_half"] <- ":30"
# paste those minute half values onto the minute_detected col
aru_gcki$minute_detected <- paste0(as.character(aru_gcki$minute_detected), 
                                   as.character(aru_gcki$minute_half))
aru_gcki$minute_half <- NULL

# pivot_wider to make each half min a column
aru_gcki <- pivot_wider(aru_gcki, names_from = "minute_detected", 
                         values_from = "det_code")
#add up each row 
aru_gcki[is.na(aru_gcki)] <- "0"
aru_gcki[4:9] <- as.integer(aru_gcki[4:9] != 0)
aru_gcki <- aru_gcki %>%
  mutate(count = rowSums(.[4:9]))
aru_gcki <- aru_gcki[ , (names(aru_gcki) %in% keepl)]

#prep for join: make sure col names are the same for both dfs
aru_gcki <- rename(aru_gcki, anon_filename = "anon_file_name")
lnogcki <- rename(lnogcki, anon_filename = "anon_file_name")

## merge aru_gcki and aru_gckiw
arugcki <- rbind(aru_gcki, aru_gckiw, lnogcki, wnogcki)

## clean up workspace
rm(aru_gcki, aru_gckiw, lnogcki, wnogcki)

### un-anonymize the aru file names---------------------------------------------

#get rid of columns we don't need in filenames
filenames <- subset(filenames, select = c(original_name, anon_name, date))
#change column name to match arugcki
filenames <- rename(filenames, anon_filename = "anon_name")

#join filenames to arugcki to add a column that has the un-anonymized file names
arugcki <- left_join(arugcki, filenames, by = "anon_filename")

### end un-anonymize------------------------------------------------------------

### get rid of duplicated ARU counts--------------------------------------------
## subset to only aru counts that are duplicated
arudup <- table(arugcki$original_name)
w.arudup <- arugcki[arugcki$original_name %in% names(arudup[arudup > 1]), ]

##order arudup by original name
w.arudup <- w.arudup[order(w.arudup$original_name), ]


#for each matching "original file name" pair, check whether the species detected
# are the same
match <- c()
no_match <- c()

for (i in 1:nrow(w.arudup)){
  if (identical(w.arudup[i, "original_name"], w.arudup[(i+1), "original_name"])){
    if (identical(w.arudup[i, 2:ncol(w.arudup)], 
                  w.arudup[(i+1), 2:ncol(w.arudup)])){
      match <- c(match, w.arudup[i, "original_name"])
    }
    else {no_match <- c(no_match, w.arudup[i, "original_name"])}
  }
}

#show rows with "no match" original names
dup_no_match <- data.frame(w.arudup[w.arudup$original_name %in% no_match, ])

#subset all spdet aru data to only duplicated files
t.arudup <- table(arugcki$original_name)
gcki_arudup <- arugcki[arugcki$original_name %in% 
                            names(t.arudup[t.arudup > 1]), ]
#order spdet_arudup by original filename
gcki_arudup <- gcki_arudup[order(gcki_arudup$original_name), ]

#randomly sample one of each duplicate aru recording
drop_anon <- c()
for (i in 1:length(unique(gcki_arudup$original_name))){
  nm <- unique(gcki_arudup$original_name)[i]
  pair <- gcki_arudup$anon_filename[gcki_arudup$original_name == nm]
  dr <- sample(pair, size = 1)
  drop_anon <- c(drop_anon, dr)
}

#eliminate duplicate aru values selected in drop_anon from arugcki
arugcki <- arugcki[arugcki$anon_filename %nin% drop_anon, ]

# clean up environment
rm(dup_no_match, gcki_arudup, match, no_match, w.arudup, arudup, dr, drop_anon, 
   nm, pair, t.arudup)

### end get rid of duplicated ARU counts----------------------------------------

### add predictor variables needed for analysis---------------------------------
#create a point_id column for arugcki from the point id indicated in original_name
arugcki$point_id <- gsub(".*_...._", "", arugcki$original_name)
arugcki$point_id <- gsub(".wav", "", arugcki$point_id)

#create a point count id column by pasting the point_id and date cols
arugcki$ptct_id <- paste(as.character(arugcki$date), arugcki$point_id, sep = "_")

#create an aru_id col for arugcki
arugcki$aru_id <- gsub("_........_...._...", "", arugcki$original_name)
arugcki$aru_id <- gsub("wav", "", arugcki$aru_id)
arugcki$aru_id <- gsub("[:.:]", "", arugcki$aru_id)

# create a new df with just weather variables and ptctids
weathervar <- gcki[ , c("start_time", "wind", "rain", "noise", "cloud_cover", 
                        "min_past_sun", "day_of_yr", "ptct_id")]
weathervar <- distinct(weathervar)
# join to arugcki df to add weather and start time variables
arugcki <- left_join(arugcki, weathervar, by = "ptct_id")

### end add predictor variables for analysis------------------------------------

### aggregate GCKI observations per day to get mean # of 30 sec intervals with a 
### vocalization per recorder each day (meandet), and total # of 30 sec 
### intervals per day (count)
sum_arugcki <- arugcki %>%
  group_by(day_of_yr) %>%
  summarize(meandet = mean(count), count = sum(count))

## add average windspeed for the day to sum_arugcki df
sum_arugcki <- left_join(sum_arugcki, windday, by = "day_of_yr")

### end aggregate by day--------------------------------------------------------

#### end 10 min ARU count data prep (GCKI)--------------------------------------


##### random min ARU data prep (10 min and 22 min) (GCKI)-----------------------
#subset arurand to gcki observations
arugcki22 <- arurand[which(arurand$species_code == "GCKI"), ]

#get unique ptctids for counts on which there were no GCKIs
nogcki <- arurand[which(arurand$filename %nin% arugcki22$filename), ]

#create a count column for the nogcki dfs
nogcki$count <- 0

#only keep filenames and count for nogcki counts
keep <- c("filename", "count")
nogcki <- nogcki[ , (names(nogcki) %in% keep)]

# get only a single row for each minute with no GCKI
nogcki <- dplyr::distinct(nogcki)

## add up number of 30 sec periods in which a GCKI was detected
#get rid of comments column
arugcki22$comments <- NULL
arugcki22[is.na(arugcki22)] <- "0"
arugcki22[3:4] <- as.integer(arugcki22[3:4] != 0)
arugcki22 <- arugcki22 %>%
  mutate(count = rowSums(.[3:4]))
#subset to only count and anon_filename cols
arugcki22 <- arugcki22[ , (names(arugcki22) %in% keep)]

#prep for join: make sure col names are the same for both dfs
arugcki22 <- rename(arugcki22, anon_name = "filename")
nogcki <- rename(nogcki, anon_name = "filename")

#join gcki and no gcki min observations
arugcki22 <- rbind(arugcki22, nogcki)
rm(nogcki)

#get rid of columns we don't need in filenames
filenames22r <- subset(filenames22r, select = c(selec, sound.files, 
                                                anon_name, date))

#de-anonymize
arugcki22 <- left_join(arugcki22, filenames22r, by = "anon_name")

### add point id, count type, aru id, weather, aru_sample columns
#create dataframe with key for the anonymized recorder names
rec <- c("rec1", "rec2", "rec3", "rec4")
aru_id <- c("swift02", "swift01", "AM", "swift03")
recs <- data.frame(aru_id, rec)

#create an aru_id col for allaru
arugcki22$rec <- gsub("day.{2,3}_?", "", arugcki22$anon_name)
arugcki22$rec <- gsub("_.*_.*", "", arugcki22$rec)

#de-anonymize aru_id
arugcki22 <- left_join(arugcki22, recs, by = "rec")

#remove "rec" column-- aru_id is all that's needed
arugcki22$rec <- NULL

# fetch the point id for a particular date and aru from the allaru df
# locs <- data.frame(allaru$point_id, allaru$date, allaru$aru_id)
# locs <- rename(locs, point_id = "allaru.point_id", date = "allaru.date", 
#                aru_id = "allaru.aru_id")

#create an aruday column that says both the date and the aru
# locs$aruday <- paste(as.character(locs$date), as.character(locs$aru_id), 
#                      sep = "_")
arugcki22$aruday <- paste(as.character(arugcki22$date), 
                          as.character(arugcki22$aru_id), 
                        sep = "_")

# #get rid of unnecessary columns and rows in locs
# locs$date <- NULL
# locs$aru_id <- NULL
# locs <- unique(locs)
# 
# #add point id to arurand by joining locs and arurand by aruday
# arurand <- left_join(arurand, locs, by = "aruday")

## add wind to arugcki22. 
# make day of year col for arugcki22
arugcki22$day_of_yr <- yday(arugcki22$date)
# join windday and arugcki22 by day of yr
arugcki22 <- left_join(arugcki22, windday, by = "day_of_yr")

#### get 10 random minutes and 22 random minutes for each unit on each day
#### from all random min data.

## get unique values of selec for each aruday
min <- arugcki22 %>% group_by(aruday) %>% distinct(selec)
#table(min$aruday)

##randomly sample 10 rand min from each aruday and 22 rand min from each aruday
# get 10 random min for each aruday from "min" df 
s10r <- sample_n(min, 10, replace = FALSE)

# get 22 random min for each aruday from "min" df
s22r <- sample_n(min, 22, replace = FALSE)

# get dataframe with all species observations from the 22 randomly selected mins
# for each aruday
arugcki22r <- semi_join(arugcki22, s22r, by = c("selec", "aruday"))

# get dataframe with all species observations from the 10 randomly selected mins
# for each aruday
arugcki10r <- semi_join(arugcki22, s10r, by = c("selec", "aruday"))

### add up individuals detected per 22 and 10  random minutes-------------------

sum_arugcki22r <- arugcki22r %>%
  group_by(aruday, day_of_yr) %>%
  summarize(count = sum(count)) %>%
  group_by(day_of_yr) %>%
  summarize(meandet = mean(count), count = sum(count))

sum_arugcki10r <- arugcki10r %>%
  group_by(aruday, day_of_yr) %>%
  summarize(count = sum(count)) %>%
  group_by(day_of_yr) %>%
  summarize(meandet = mean(count), count = sum(count))


#create count type col
sum_arugcki10r$count_type <- "aru_10r"
sum_arugcki22r$count_type <- "aru_22r"

# join windday to sum_arugcki dfs by day of yr
sum_arugcki22r <- left_join(sum_arugcki22r, windday, by = "day_of_yr")
sum_arugcki10r <- left_join(sum_arugcki10r, windday, by = "day_of_yr")

### end mean # individuals per count ---------------------------------------

##### end 10 and 22 rand min data prep (GCKI)-----------------------------------



#### 10 min ARU count data prep (WIWR)------------------------------------------
aru_wiwr <- longaru[which(longaru$species_code == "WIWR"), ]

#subset widearu to WIWR
aru_wiwrw <- widearu[which(widearu$species_code == "WIWR"), ]

#get unique ptctids for counts on which there were no WIWRs
lnowiwr <- longaru[which(longaru$anon_file_name %nin% aru_wiwr$anon_file_name), ]
wnowiwr <- widearu[which(widearu$anon_filename %nin% aru_wiwrw$anon_filename), ]

#create a count column for the nowiwr dfs
lnowiwr$count <- 0
wnowiwr$count <- 0

#get list of anon filenames with 
keepw <- c("anon_filename", "count")
keepl <- c("anon_file_name", "count")
lnowiwr <- lnowiwr[ , (names(lnowiwr) %in% keepl)]
wnowiwr <- wnowiwr[ , (names(wnowiwr) %in% keepw)]

# get only a single row for each 10 min aru count (counts with no wiwr)
wnowiwr <- dplyr::distinct(wnowiwr)
lnowiwr <- dplyr::distinct(lnowiwr)

## add up number of 30 sec periods in which a wiwr was detected- wide aru
aru_wiwrw[is.na(aru_wiwrw)] <- "0"
aru_wiwrw[4:23] <- as.integer(aru_wiwrw[4:23] != 0)
aru_wiwrw <- aru_wiwrw %>%
  mutate(count = rowSums(.[4:23]))
#subset to only count and anon_filename cols
aru_wiwrw <- aru_wiwrw[ , (names(aru_wiwrw) %in% keepw)]
#add .wav extension to filenames
aru_wiwrw$anon_filename <- paste(aru_wiwrw$anon_filename, ".wav", sep = "")
wnowiwr$anon_filename <- paste(wnowiwr$anon_filename, ".wav", sep = "")

## add up number of 30 sec periods in which a wiwr was detected- long aru
# change minute half values from 1 + 2 to :00 and :30
aru_wiwr[which(aru_wiwr$minute_half == "1"), "minute_half"] <- ":00"
aru_wiwr[which(aru_wiwr$minute_half == "2"), "minute_half"] <- ":30"
# paste those minute half values onto the minute_detected col
aru_wiwr$minute_detected <- paste0(as.character(aru_wiwr$minute_detected), 
                                   as.character(aru_wiwr$minute_half))
aru_wiwr$minute_half <- NULL

# pivot_wider to make each half min a column
aru_wiwr <- pivot_wider(aru_wiwr, names_from = "minute_detected", 
                        values_from = "det_code")
#add up each row 
aru_wiwr[is.na(aru_wiwr)] <- "0"
aru_wiwr[4:9] <- as.integer(aru_wiwr[4:9] != 0)
aru_wiwr <- aru_wiwr %>%
  mutate(count = rowSums(.[4:9]))
aru_wiwr <- aru_wiwr[ , (names(aru_wiwr) %in% keepl)]

#prep for join: make sure col names are the same for both dfs
aru_wiwr <- rename(aru_wiwr, anon_filename = "anon_file_name")
lnowiwr <- rename(lnowiwr, anon_filename = "anon_file_name")

## merge aru_wiwr and aru_wiwrw
aruwiwr <- rbind(aru_wiwr, aru_wiwrw, lnowiwr, wnowiwr)

## clean up workspace
rm(aru_wiwr, aru_wiwrw, lnowiwr, longaru, widearu, wnowiwr)

### un-anonymize the aru file names---------------------------------------------

#join filenames to aruwiwr to add a column that has the un-anonymized file names
aruwiwr <- left_join(aruwiwr, filenames, by = "anon_filename")

### end un-anonymize------------------------------------------------------------

### get rid of duplicated ARU counts--------------------------------------------
## subset to only aru counts that are duplicated
arudup <- table(aruwiwr$original_name)
w.arudup <- aruwiwr[aruwiwr$original_name %in% names(arudup[arudup > 1]), ]

##order arudup by original name
w.arudup <- w.arudup[order(w.arudup$original_name), ]


#for each matching "original file name" pair, check whether the species detected
# are the same
match <- c()
no_match <- c()

for (i in 1:nrow(w.arudup)){
  if (identical(w.arudup[i, "original_name"], w.arudup[(i+1), "original_name"])){
    if (identical(w.arudup[i, 2:ncol(w.arudup)], 
                  w.arudup[(i+1), 2:ncol(w.arudup)])){
      match <- c(match, w.arudup[i, "original_name"])
    }
    else {no_match <- c(no_match, w.arudup[i, "original_name"])}
  }
}

#show rows with "no match" original names
dup_no_match <- data.frame(w.arudup[w.arudup$original_name %in% no_match, ])

#subset all spdet aru data to only duplicated files
t.arudup <- table(aruwiwr$original_name)
wiwr_arudup <- aruwiwr[aruwiwr$original_name %in% 
                         names(t.arudup[t.arudup > 1]), ]
#order spdet_arudup by original filename
wiwr_arudup <- wiwr_arudup[order(wiwr_arudup$original_name), ]

#randomly sample one of each duplicate aru recording
drop_anon <- c()
for (i in 1:length(unique(wiwr_arudup$original_name))){
  nm <- unique(wiwr_arudup$original_name)[i]
  pair <- wiwr_arudup$anon_filename[wiwr_arudup$original_name == nm]
  dr <- sample(pair, size = 1)
  drop_anon <- c(drop_anon, dr)
}

#eliminate duplicate aru values selected in drop_anon from aruwiwr
aruwiwr <- aruwiwr[aruwiwr$anon_filename %nin% drop_anon, ]

# clean up environment
rm(dup_no_match, filenames, wiwr_arudup, match, no_match, w.arudup,
   arudup, dr, drop_anon, nm, pair, t.arudup)

### get rid of duplicated ARU counts--------------------------------------------

### add predictor variables needed for analysis---------------------------------
#create a point_id column for aruwiwr from the point id indicated in original_name
aruwiwr$point_id <- gsub(".*_...._", "", aruwiwr$original_name)
aruwiwr$point_id <- gsub(".wav", "", aruwiwr$point_id)

#create a point count id column by pasting the point_id and date cols
aruwiwr$ptct_id <- paste(as.character(aruwiwr$date), aruwiwr$point_id, sep = "_")

#create an aru_id col for aruwiwr
aruwiwr$aru_id <- gsub("_........_...._...", "", aruwiwr$original_name)
aruwiwr$aru_id <- gsub("wav", "", aruwiwr$aru_id)
aruwiwr$aru_id <- gsub("[:.:]", "", aruwiwr$aru_id)

# join to aruwiwr df to add weather and start time variables
aruwiwr <- left_join(aruwiwr, weathervar, by = "ptct_id")

### end add predictor variables for analysis------------------------------------

### aggregate wiwr observations per day-----------------------------------------
sum_aruwiwr <- aruwiwr %>%
  group_by(day_of_yr) %>%
  summarize(meandet = mean(count), count = sum(count))

## add average windspeed for the day to sum_aruwiwr df
sum_aruwiwr <- left_join(sum_aruwiwr, windday, by = "day_of_yr")

#### end 10 min ARU count data prep (WIWR)--------------------------------------

##### random min ARU data prep (10 min and 22 min) (WIWR)-----------------------
#subset arurand to wiwr observations
aruwiwr22 <- arurand[which(arurand$species_code == "WIWR"), ]

#get unique ptctids for counts on which there were no GCKIs
nowiwr <- arurand[which(arurand$filename %nin% aruwiwr22$filename), ]

#create a count column for the nogcki dfs
nowiwr$count <- 0

#only keep filenames and count for nowiwr counts
nowiwr <- nowiwr[ , (names(nowiwr) %in% keep)]

# get only a single row for each minute with no WIWR
nowiwr <- dplyr::distinct(nowiwr)

## add up number of 30 sec periods in which a WIWR was detected
#get rid of comments column
aruwiwr22$comments <- NULL
aruwiwr22[is.na(aruwiwr22)] <- "0"
aruwiwr22[3:4] <- as.integer(aruwiwr22[3:4] != 0)
aruwiwr22 <- aruwiwr22 %>%
  mutate(count = rowSums(.[3:4]))
#subset to only count and anon_filename cols
aruwiwr22 <- aruwiwr22[ , (names(aruwiwr22) %in% keep)]

#prep for join: make sure col names are the same for both dfs
aruwiwr22 <- rename(aruwiwr22, anon_name = "filename")
nowiwr <- rename(nowiwr, anon_name = "filename")

#join wiwr and no wiwr min observations
aruwiwr22 <- rbind(aruwiwr22, nowiwr)
rm(nowiwr)

#de-anonymize
aruwiwr22 <- left_join(aruwiwr22, filenames22r, by = "anon_name")

#create an aru_id col for allaru
aruwiwr22$rec <- gsub("day.{2,3}_?", "", aruwiwr22$anon_name)
aruwiwr22$rec <- gsub("_.*_.*", "", aruwiwr22$rec)

#de-anonymize aru_id
aruwiwr22 <- left_join(aruwiwr22, recs, by = "rec")

#remove "rec" column-- aru_id is all that's needed
aruwiwr22$rec <- NULL

#create an aruday column that says both the date and the aru
aruwiwr22$aruday <- paste(as.character(aruwiwr22$date), 
                          as.character(aruwiwr22$aru_id), 
                          sep = "_")

## add wind to aruwiwr22. 
# make day of year col for aruwiwr22
aruwiwr22$day_of_yr <- yday(aruwiwr22$date)
# join windday and arugcki22 by day of yr
aruwiwr22 <- left_join(aruwiwr22, windday, by = "day_of_yr")

#### get 10 random minutes and 22 random minutes for each unit on each day
#### from all random min data.

##randomly sample 10 rand min from each aruday and 22 rand min from each aruday
# get 10 random min for each aruday from "min" df 
s10r_2 <- sample_n(min, 10, replace = FALSE)

# get 22 random min for each aruday from "min" df
s22r_2 <- sample_n(min, 22, replace = FALSE)

# get dataframe with all species observations from the 22 randomly selected mins
# for each aruday
aruwiwr22r <- semi_join(aruwiwr22, s22r_2, by = c("selec", "aruday"))

# get dataframe with all species observations from the 10 randomly selected mins
# for each aruday
aruwiwr10r <- semi_join(aruwiwr22, s10r_2, by = c("selec", "aruday"))

### add up individuals detected per 22 and 10  random minutes-------------------

sum_aruwiwr22r <- aruwiwr22r %>%
  group_by(aruday, day_of_yr) %>%
  summarize(count = sum(count)) %>%
  group_by(day_of_yr) %>%
  summarize(meandet = mean(count), count = sum(count))

sum_aruwiwr10r <- aruwiwr10r %>%
  group_by(aruday, day_of_yr) %>%
  summarize(count = sum(count)) %>%
  group_by(day_of_yr) %>%
  summarize(meandet = mean(count), count = sum(count))


#create count type col
sum_aruwiwr10r$count_type <- "aru_10r"
sum_aruwiwr22r$count_type <- "aru_22r"

# join windday to sum_arugcki dfs by day of yr
sum_aruwiwr22r <- left_join(sum_aruwiwr22r, windday, by = "day_of_yr")
sum_aruwiwr10r <- left_join(sum_aruwiwr10r, windday, by = "day_of_yr")

### end mean # individuals per count ---------------------------------------

##### end 10 and 22 rand min data prep (GCKI)-----------------------------------


# clean up workspace
rm(arugcki10r, arugcki22r, arugcki22, arurand, aruwiwr10r, aruwiwr22, aruwiwr22r, 
   filenames22r, min, recs, s10r, s10r_2, s22r, s22r_2, weathervar, windday, 
   aru_id, keep, keepl, keepw, rec)

