################################
## Data Prep for Abundance estimation for ARU counts
## Point Abbaye 2019 data
## 
## author: Ellie Roark
## created: 9 Mar 2020
## last modified: 5 Feb 2021
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
widearu <- read_csv(file = "./data/ARUPointCounts_WIDE_PtAbbaye2019.csv")
longaru <- read_csv(file = "./data/ARUPointCounts_PtAbbaye2019.csv")
filenames <- read_csv(file = "./data/anonymized_file_key.csv")

#read in 22 rand min data
arurand <- read_csv(file = "./data/ARU20randmin_final.csv")
filenames22r <- read_csv(file = "./data/filename_key_20randmin.csv", 
                      col_types = cols(.default = "?", folder = "c"))
addrand <- read_csv(file = "./data/ARU20randmin_additional_days106107.csv")

#### 10 consecutive minute ARU count data prep (GCKI)---------------------------
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

#get list of anon filenames with no gcki
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

### sample a max of 3 10c cts per day.
# temporarily drop dates 2019-04-16 and 2019-04-17 from the df since they don't 
# have 3 samples each.
tmp <- arugcki[which(arugcki$day_of_yr == 106 | arugcki$day_of_yr == 107), ]
arugcki <- arugcki[which(arugcki$ptct_id %nin% tmp$ptct_id), ]
arugcki <- arugcki %>% group_by(date)
std3 <- sample_n(arugcki, size= 3, replace = FALSE)
arugcki <- semi_join(arugcki, std3)
arugcki <- bind_rows(arugcki, tmp)

### end add predictor variables for analysis------------------------------------

### aggregate GCKI observations per day to get proportion of 30 sec intervals
###  with a vocalization per day (resp), and total # of 30 sec 
### intervals per day (count)
sum_arugcki <- arugcki %>%
  group_by(day_of_yr) %>%
  summarize(count = sum(count), segs = 20*n())
sum_arugcki$resp <- sum_arugcki$count/sum_arugcki$segs

## add average windspeed for the day to sum_arugcki df
sum_arugcki <- left_join(sum_arugcki, windday, by = "day_of_yr")




### end aggregate by day--------------------------------------------------------

#### end 10 min ARU count data prep (GCKI)--------------------------------------


##### random min ARU data prep (10 min and 22 min) (GCKI)-----------------------
#subset arurand and addrand to gcki observations
arugcki22 <- arurand[which(arurand$species_code == "GCKI"), ]
addgcki <- addrand[which(addrand$species_code == "GCKI"), ]

#get unique ptctids for counts on which there were no GCKIs
nogcki <- arurand[which(arurand$filename %nin% arugcki22$filename), ]
addnogcki <- addrand[which(addrand$filename %nin% addgcki$filename), ]

#create a count column for the nogcki dfs
nogcki$count <- 0
addnogcki$count <- 0

#only keep filenames and count for nogcki counts
keep <- c("filename", "count")
nogcki <- nogcki[ , (names(nogcki) %in% keep)]
addnogcki <- addnogcki[ , (names(addnogcki) %in% keep)]

# get only a single row for each minute with no GCKI
nogcki <- dplyr::distinct(nogcki)
addnogcki <- dplyr::distinct(addnogcki)

## add up number of 30 sec periods in which a GCKI was detected
#get rid of comments column
arugcki22$comments <- NULL
arugcki22[is.na(arugcki22)] <- "0"
arugcki22[3:4] <- as.integer(arugcki22[3:4] != 0)
arugcki22 <- arugcki22 %>%
  mutate(count = rowSums(.[3:4]))
#subset to only count and anon_filename cols
arugcki22 <- arugcki22[ , (names(arugcki22) %in% keep)]

addgcki$comments <- NULL
addgcki[is.na(addgcki)] <- "0"
addgcki[3:4] <- as.integer(addgcki[3:4] != 0)
addgcki <- addgcki %>%
  mutate(count = rowSums(.[3:4]))
#subset to only count and anon_filename cols
addgcki <- addgcki[ , (names(addgcki) %in% keep)]

#prep for join: make sure col names are the same for both dfs
arugcki22 <- rename(arugcki22, anon_name = "filename")
nogcki <- rename(nogcki, anon_name = "filename")

#join gcki and no gcki min observations
arugcki22 <- rbind(arugcki22, nogcki)
addgcki <- rbind(addgcki, addnogcki)
rm(nogcki, addnogcki)

#get rid of columns we don't need in filenames
filenames22r <- subset(filenames22r, select = c(selec, sound.files, 
                                                anon_name, date))

#de-anonymize
arugcki22 <- left_join(arugcki22, filenames22r, by = "anon_name")

# join addrand to arugcki22
## make date a new column in addrand
addgcki$date <- regmatches(addgcki$filename, regexpr("2019-04-..", 
                                                     addgcki$filename))
addgcki$date <- as.Date(addgcki$date)

## make a "selec" column for addgcki
addgcki$selec <- regmatches(addgcki$filename, regexpr("_[a-e][0-9]+", 
                                                      addgcki$filename))
addgcki$selec <- gsub("_", "", addgcki$selec)

# change "anon_name" to "filename" in arugcki22
arugcki22 <- rename(arugcki22, filename = "anon_name")

### add point id, count type, aru id, weather, aru_sample columns
#create dataframe with key for the anonymized recorder names
rec <- c("rec1", "rec2", "rec3", "rec4")
aru_id <- c("swift02", "swift01", "AM", "swift03")
recs <- data.frame(aru_id, rec)

#create an aru_id col for allaru
arugcki22$rec <- regmatches(arugcki22$filename, regexpr("rec.", 
                                                      arugcki22$filename))
addgcki$rec <- regmatches(addgcki$filename, regexpr("rec.", 
                                                        addgcki$filename))

#de-anonymize aru_id
arugcki22 <- left_join(arugcki22, recs, by = "rec")
addgcki <- left_join(addgcki, recs, by = "rec")

#remove "rec" column-- aru_id is all that's needed
arugcki22$rec <- NULL
addgcki$rec <- NULL

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
addgcki$aruday <- paste(as.character(addgcki$date), 
                        as.character(addgcki$aru_id), sep = "_")

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
addgcki$day_of_yr <- yday(addgcki$date)
# join windday and arugcki22 by day of yr
arugcki22 <- left_join(arugcki22, windday, by = "day_of_yr")
addgcki <- left_join(addgcki, windday, by = "day_of_yr")

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

#add addgcki counts to arugcki22r df
arugcki22r <- bind_rows(arugcki22r, addgcki)

#subsample 5 min from each aruday in addgcki and add to arugcki10r
addmin <- addgcki %>% group_by(aruday) %>% distinct(selec)
s5r <- sample_n(addmin, 5, replace = FALSE)
addgcki5 <- semi_join(addgcki, s5r, by = c("selec", "aruday"))

arugcki10r <- bind_rows(arugcki10r, addgcki5)

## standardize survey effort to 66 random min per day for 22min cts and
## 30 random min per day for 10min cts.
arugcki10r <- arugcki10r %>% group_by(date)
std30 <- sample_n(arugcki10r, 30, replace = FALSE)
arugcki10r <- semi_join(arugcki10r, std30)

arugcki22r <- arugcki22r %>% group_by(date)
std66 <- sample_n(arugcki22r, 66, replace = FALSE)
arugcki22r <- semi_join(arugcki22r, std66)

### summarize proportion of 30 sec intervals with a vocalization per day--------
sum_arugcki22r <- arugcki22r %>%
  group_by(day_of_yr) %>%
  summarize(count = sum(count), segs = 2*n())
sum_arugcki22r$resp <- sum_arugcki22r$count/sum_arugcki22r$segs

sum_arugcki10r <- arugcki10r %>%
  group_by(day_of_yr) %>%
  summarize(count = sum(count), segs = 2*n())
sum_arugcki10r$resp <- sum_arugcki10r$count/sum_arugcki10r$segs

## old abundance proxy method: mean number of 30 sec intervals with a voc. per 
## aru per day. 
# sum_arugcki22r <- arugcki22r %>%
#   group_by(aruday, day_of_yr) %>%
#   summarize(count = sum(count)) %>%
#   group_by(day_of_yr) %>%
#   summarize(meandet = mean(count), count = sum(count))
# 
# sum_arugcki10r <- arugcki10r %>%
#   group_by(aruday, day_of_yr) %>%
#   summarize(count = sum(count)) %>%
#   group_by(day_of_yr) %>%
#   summarize(meandet = mean(count), count = sum(count))


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
## sample a max of 3 10c cts per day.
# temporarily drop dates 2019-04-16 and 2019-04-17 from the df since they don't 
# have 3 samples each.
wtmp <- aruwiwr[which(aruwiwr$day_of_yr == 106 | aruwiwr$day_of_yr == 107), ]
aruwiwr <- aruwiwr[which(aruwiwr$ptct_id %nin% wtmp$ptct_id), ]
aruwiwr <- aruwiwr %>% group_by(date)
wstd3 <- sample_n(aruwiwr, size= 3, replace = FALSE)
aruwiwr <- semi_join(aruwiwr, wstd3)
aruwiwr <- bind_rows(aruwiwr, wtmp)

## aggregate GCKI observations per day to get proportion of 30 sec intervals
##  with a vocalization per day (resp), and total # of 30 sec 
## intervals per day (count)
sum_aruwiwr <- aruwiwr %>%
  group_by(day_of_yr) %>%
  summarize(count = sum(count), segs = 20*n())
sum_aruwiwr$resp <- sum_aruwiwr$count/sum_aruwiwr$segs

## add average windspeed for the day to sum_aruwiwr df
sum_aruwiwr <- left_join(sum_aruwiwr, windday, by = "day_of_yr")

#### end 10 min ARU count data prep (WIWR)--------------------------------------

##### random min ARU data prep (10 min and 22 min) (WIWR)-----------------------
#subset arurand to wiwr observations
aruwiwr22 <- arurand[which(arurand$species_code == "WIWR"), ]
addwiwr <- addrand[which(addrand$species_code == "WIWR"), ]

#get unique ptctids for counts on which there were no GCKIs
nowiwr <- arurand[which(arurand$filename %nin% aruwiwr22$filename), ]
addnowiwr <- addrand[which(addrand$filename %nin% addwiwr$filename), ]

#create a count column for the nogcki dfs
nowiwr$count <- 0
addnowiwr$count <- 0

#only keep filenames and count for nowiwr counts
nowiwr <- nowiwr[ , (names(nowiwr) %in% keep)]
addnowiwr <- addnowiwr[ , (names(addnowiwr) %in% keep)]

# get only a single row for each minute with no WIWR
nowiwr <- dplyr::distinct(nowiwr)
addnowiwr <- dplyr::distinct(addnowiwr)

## add up number of 30 sec periods in which a WIWR was detected
#get rid of comments column
aruwiwr22$comments <- NULL
aruwiwr22[is.na(aruwiwr22)] <- "0"
aruwiwr22[3:4] <- as.integer(aruwiwr22[3:4] != 0)
aruwiwr22 <- aruwiwr22 %>%
  mutate(count = rowSums(.[3:4]))
#subset to only count and anon_filename cols
aruwiwr22 <- aruwiwr22[ , (names(aruwiwr22) %in% keep)]

addwiwr$comments <- NULL
addwiwr[is.na(addwiwr)] <- "0"
addwiwr[3:4] <- as.integer(addwiwr[3:4] != 0)
addwiwr <- addwiwr %>%
  mutate(count = rowSums(.[3:4]))
#subset to only count and anon_filename cols
addwiwr <- addwiwr[ , (names(addwiwr) %in% keep)]

#prep for join: make sure col names are the same for both dfs
aruwiwr22 <- rename(aruwiwr22, anon_name = "filename")
nowiwr <- rename(nowiwr, anon_name = "filename")

#join wiwr and no wiwr min observations
aruwiwr22 <- rbind(aruwiwr22, nowiwr)
addwiwr <- rbind(addwiwr, addnowiwr)
rm(nowiwr, addnowiwr)

## make date a new column in addrand
addwiwr$date <- regmatches(addwiwr$filename, regexpr("2019-04-..", 
                                                     addwiwr$filename))
addwiwr$date <- as.Date(addwiwr$date)

## make a "selec" column for addgcki
addwiwr$selec <- regmatches(addwiwr$filename, regexpr("_[a-e][0-9]+", 
                                                      addwiwr$filename))
addwiwr$selec <- gsub("_", "", addwiwr$selec)

#de-anonymize
aruwiwr22 <- left_join(aruwiwr22, filenames22r, by = "anon_name")

# change "anon_name" to "filename" in arugcki22
aruwiwr22 <- rename(aruwiwr22, filename = "anon_name")

#create an aru_id col for allaru
aruwiwr22$rec <- gsub("day.{2,3}_?", "", aruwiwr22$filename)
aruwiwr22$rec <- gsub("_.*_.*", "", aruwiwr22$rec)

addwiwr$rec <- regmatches(addwiwr$filename, regexpr("rec.", 
                                                    addwiwr$filename))

#de-anonymize aru_id
aruwiwr22 <- left_join(aruwiwr22, recs, by = "rec")
addwiwr <- left_join(addwiwr, recs, by = "rec")

#remove "rec" column-- aru_id is all that's needed
aruwiwr22$rec <- NULL
addwiwr$rec <- NULL

#create an aruday column that says both the date and the aru
aruwiwr22$aruday <- paste(as.character(aruwiwr22$date), 
                          as.character(aruwiwr22$aru_id), 
                          sep = "_")
addwiwr$aruday <- paste(as.character(addwiwr$date), 
                        as.character(addwiwr$aru_id), sep = "_")

## add wind to aruwiwr22. 
# make day of year col for aruwiwr22
aruwiwr22$day_of_yr <- yday(aruwiwr22$date)
addwiwr$day_of_yr <- yday(addwiwr$date)

# join windday and arugcki22 by day of yr
aruwiwr22 <- left_join(aruwiwr22, windday, by = "day_of_yr")
addwiwr <- left_join(addwiwr, windday, by = "day_of_yr")

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

#add addgcki counts to arugcki22r df
aruwiwr22r <- bind_rows(aruwiwr22r, addwiwr)

# get dataframe with all species observations from the 10 randomly selected mins
# for each aruday
aruwiwr10r <- semi_join(aruwiwr22, s10r_2, by = c("selec", "aruday"))

#subsample 5 min from each aruday and add to aruwiwr10r
addmin <- addwiwr %>% group_by(aruday) %>% distinct(selec)
s5r <- sample_n(addmin, 5, replace = FALSE)
addwiwr5 <- semi_join(addwiwr, s5r, by = c("selec", "aruday"))

aruwiwr10r <- bind_rows(aruwiwr10r, addwiwr5)

## standardize survey effort to 66 random min per day for 22min cts and
## 30 random min per day for 10min cts.
aruwiwr10r <- aruwiwr10r %>% group_by(date)
std30 <- sample_n(aruwiwr10r, 30, replace = FALSE)
aruwiwr10r <- semi_join(aruwiwr10r, std30)

aruwiwr22r <- aruwiwr22r %>% group_by(date)
std66 <- sample_n(aruwiwr22r, 66, replace = FALSE)
aruwiwr22r <- semi_join(aruwiwr22r, std66)


### add up individuals detected per 22 and 10  random minutes-------------------

sum_aruwiwr22r <- aruwiwr22r %>%
  group_by(day_of_yr) %>%
  summarize(count = sum(count), segs = 2*n())
sum_aruwiwr22r$resp <- sum_aruwiwr22r$count/sum_aruwiwr22r$segs

sum_aruwiwr10r <- aruwiwr10r %>%
  group_by(day_of_yr) %>%
  summarize(count = sum(count), segs = 2*n())
sum_aruwiwr10r$resp <- sum_aruwiwr10r$count/sum_aruwiwr10r$segs

# old abundance proxy: mean number of 30-sec intervals with a vocalization per
# aru per day. DEFUNCT AS OF 25 MAY 2020
# sum_aruwiwr10r <- aruwiwr10r %>%
#   group_by(aruday, day_of_yr) %>%
#   summarize(count = sum(count)) %>%
#   group_by(day_of_yr) %>%
#   summarize(meandet = mean(count), count = sum(count))


#create count type col
sum_aruwiwr10r$count_type <- "aru_10r"
sum_aruwiwr22r$count_type <- "aru_22r"

# join windday to sum_arugcki dfs by day of yr
sum_aruwiwr22r <- left_join(sum_aruwiwr22r, windday, by = "day_of_yr")
sum_aruwiwr10r <- left_join(sum_aruwiwr10r, windday, by = "day_of_yr")

### end mean # individuals per count ---------------------------------------

##### end 10 and 22 rand min data prep (WIWR)-----------------------------------


##### Prep 10 consecutive minute data for ALL species---------------------------
## TODO: Here 5 Feb 2021 wg


##### end prep 10 consecutive minute data for ALL species-----------------------


##### Prep 22 and 10 rand min data for ALL species detected in arurand----------
aru_sp_codes <- unique(arurand$species_code)
aru_sp_codes <- aru_sp_codes[!grepl(".* .*", aru_sp_codes)]
# initiate list to hold dataframes for ARU detections of each sp.
sum_aru_dfs <- list() # for 22 random minute samples
sum_aru_dfs_10r <- list() # for 10 random minute samples

for(i in 1:length(aru_sp_codes)) {
  this_sp <- aru_sp_codes[i]
  
  #subset arurand (and addrand) to this_sp obersvations
  sp22 <- arurand[which(arurand$species_code == this_sp), ]
  add22 <- addrand[which(addrand$species_code == this_sp), ]
  # if(nrow(add22) == 0) rm(add22)
  
  #get unique ptctids for counts on which there were none of this_sp
  nosp <- arurand[which(arurand$filename %nin% sp22$filename), ]
  addnosp <- addrand[which(addrand$filename %nin% add22$filename), ]
  
  #create a count column for the nosp dfs
  nosp$count <- 0
  addnosp$count <- 0
  
  #only keep filenames and count for nosp counts
  nosp <- nosp[ , (names(nosp) %in% keep)]
  addnosp <- addnosp[ , (names(addnosp) %in% keep)]
  
  # get only a single row for each minute with none of this_sp
  nosp <- dplyr::distinct(nosp)
  addnosp <- dplyr::distinct(addnosp)
  
  ## add up number of 30 sec periods in which this_sp was detected
  #get rid of comments column
  sp22$comments <- NULL
  sp22[is.na(sp22)] <- "0"
  sp22[3:4] <- as.integer(sp22[3:4] != 0)
  sp22 <- sp22 %>%
    mutate(count = rowSums(.[3:4]))
  #subset to only count and anon_filename cols
  sp22 <- sp22[ , (names(sp22) %in% keep)]
  
  if(nrow(add22) > 0) {
    add22$comments <- NULL
    add22[is.na(add22)] <- "0"
    add22[3:4] <- as.integer(add22[3:4] != 0)
    add22 <- add22 %>%
      mutate(count = rowSums(.[3:4]))
    #subset to only count and anon_filename cols
    add22 <- add22[ , (names(add22) %in% keep)]
  }

  #prep for join: make sure col names are the same for both dfs
  sp22 <- rename(sp22, anon_name = "filename")
  nosp <- rename(nosp, anon_name = "filename")
  
  #join this_sp and no this_sp min observations
  sp22 <- rbind(sp22, nosp)
  add22 <- rbind(add22, addnosp)
  rm(nosp, addnosp)
  
  ## make date a new column in addrand
  add22$date <- regmatches(add22$filename, regexpr("2019-04-..", 
                                                       add22$filename))
  add22$date <- as.Date(add22$date)
  
  ## make a "selec" column for add22
  add22$selec <- regmatches(add22$filename, regexpr("_[a-e][0-9]+", 
                                                        add22$filename))
  add22$selec <- gsub("_", "", add22$selec)
  
  #de-anonymize
  sp22 <- left_join(sp22, filenames22r, by = "anon_name")
  
  # change "anon_name" to "filename" in sp22
  sp22 <- rename(sp22, filename = "anon_name")
  
  #create an aru_id col for sp22
  sp22$rec <- gsub("day.{2,3}_?", "", sp22$filename)
  sp22$rec <- gsub("_.*_.*", "", sp22$rec)
  
  add22$rec <- regmatches(add22$filename, regexpr("rec.", 
                                                      add22$filename))
  
  #de-anonymize aru_id
  sp22 <- left_join(sp22, recs, by = "rec")
  add22 <- left_join(add22, recs, by = "rec")
  
  #remove "rec" column-- aru_id is all that's needed
  sp22$rec <- NULL
  add22$rec <- NULL
  
  #create an aruday column that says both the date and the aru
  sp22$aruday <- paste(as.character(sp22$date), 
                            as.character(sp22$aru_id), 
                            sep = "_")
  add22$aruday <- paste(as.character(add22$date), 
                          as.character(add22$aru_id), sep = "_")
  
  ## add wind to sp22. 
  # make day of year col for sp22
  sp22$day_of_yr <- yday(sp22$date)
  add22$day_of_yr <- yday(addwiwr$date)
  
  # join windday and sp22 by day of yr
  sp22 <- left_join(sp22, windday, by = "day_of_yr")
  add22 <- left_join(add22, windday, by = "day_of_yr")
  
  #### get 22 (and 10) random minutes for each unit on each day
  #### from all random min data.
  ## randomly sample 10 rand min from each aruday and 22 rand min from each aruday
  # get 22 random min for each aruday from "min" df
  s22r <- sample_n(min, 22, replace = FALSE)
  # get 10 random min for each aruday from "min" df
  s10r_loop <- sample_n(min, 10, replace = FALSE)
  
  # get dataframe with all species observations from the randomly selected mins
  # for each aruday
  sp22r <- semi_join(sp22, s22r, by = c("selec", "aruday"))
  sp10r <- semi_join(sp22, s10r_loop, by = c("selec", "aruday"))
  
  #add add22 counts to sp22r df
  sp22r <- bind_rows(sp22r, add22)
  # add add22 counts to sp10r df.  add22 contains only data from 16 and 17 April
  # (the days that had recorder malfunctions and therefore too few minutes).
  sp10r <- bind_rows(sp10r, add22)
  
  ## standardize survey effort to 66 random min per day for 22min cts
  sp22r <- sp22r %>% group_by(date)
  std66 <- sample_n(sp22r, 66, replace = FALSE) 
  sp22r <- semi_join(sp22r, std66) # well this is a bit of code for the ages. It
  # does the correct thing, but in a REALLY roundabout way (I think std66 is 
  # actually the thing we want also, but oh well, this gets it done).
  ## standardize survey effort to 30 random min per day for 10min cts
  sp10r <- sp10r %>% group_by(date)
  std30 <- sample_n(sp10r, 30, replace = FALSE)
  sp10r <- semi_join(sp10r, std30)
  
  ### add up individuals detected per 22 and 10  random minutes-------------------
  sum_sp22r <- sp22r %>%
    group_by(day_of_yr) %>%
    summarize(count = sum(count), segs = 2*n())
  sum_sp22r$resp <- sum_sp22r$count/sum_sp22r$segs
  sum_sp22r$count_type <- "aru_22r"
  
  sum_sp10r <- sp10r %>%
    group_by(day_of_yr) %>%
    summarize(count = sum(count), segs = 2*n())
  sum_sp10r$resp <- sum_sp10r$count/sum_sp10r$segs
  sum_sp10r$count_type <- "aru_10r"
  
  # join windday to sum dfs by day of yr
  sum_sp22r <- left_join(sum_sp22r, windday, by = "day_of_yr")
  sum_sp10r <- left_join(sum_sp10r, windday, by = "day_of_yr")
  
  ### end mean # individuals per count ---------------------------------------
  
  sum_aru_dfs[[i]] <- sum_sp22r
  sum_aru_dfs_10r[[i]] <- sum_sp10r
}
names(sum_aru_dfs) <- aru_sp_codes
names(sum_aru_dfs_10r) <- aru_sp_codes

# replace WIWR and GCKI resp and count values with the values from sum_wiwr and
# sum_gcki so that downstream results match the results calculated individually
# for those species
if(identical(sum_aru_dfs$WIWR$day_of_yr, sum_aruwiwr22r$day_of_yr)) { 
  sum_aru_dfs$WIWR$resp <- sum_aruwiwr22r$resp
  sum_aru_dfs$WIWR$count <- sum_aruwiwr22r$count
} else warning("WIWR data frames don't match near end of DataPrep_ARUabundance.R.  Downstream analyses for WIWR won't match the individually-calculated results for WIWR.")
if(identical(sum_aru_dfs$GCKI$day_of_yr, sum_arugcki22r$day_of_yr)) { 
  sum_aru_dfs$GCKI$resp <- sum_arugcki22r$resp
  sum_aru_dfs$GCKI$count <- sum_arugcki22r$count
} else warning("GCKI data frames don't match near end of DataPrep_ARUabundance.R.  Downstream analyses for GCKI won't match the individually-calculated results for GCKI")


##### end prep 22 rand min data for ALL species---------------------------------

# clean up workspace
try(rm(addgcki, addgcki5, addmin, addrand, addwiwr, addwiwr5, arugcki10r, 
   arugcki22r, arugcki22, arurand, aruwiwr10r, aruwiwr22, aruwiwr22r, 
   filenames22r, min, recs,s5r, s10r, s10r_2, s22r, s22r_2, std30, std66, 
   weathervar, windday, aru_id, keep, keepl, keepw, rec, s10r_loop, sum_sp22r, 
   sum_sp10r))

   rm()