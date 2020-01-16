#####################
## This script anonymizes the ARU file names so that when Ellie listens to the
## ARU recordings she won't know what point or day the recording came from. 
## This is becaues of concern that she might remember distinct point counts on
## which she knows she got some particular bird, and therefore might be better
## prepared to hear that bird on the recording.
## 
## I will also add 20% of the files a second time with different file names
## so that the listener will listen to and evaluate the same recordings twice,
## so we can check reliability of listening.
##
## Files will be named to indicate the 2-week period in which the recording 
## was made because the time of year or season does provide important contextual
## clues to a listener that would be available in a real-world ARU ony situation.
##
## 
## author: Willson Gaul wgaul@hotmail.com
## created: 27 June 2019
## last modified: 1 July 2019
########################

library(tidyverse)
library(lubridate)

setwd("C:/Users/Ellie/Dropbox/Ellie Roark/PointAbbaye")

set.seed(10061983 + 06272019)

# get original file names
fnames <- list.files("/2019Data/AudioFiles_10minptcts/")
fnames <- fnames[!grepl(".*txt", fnames)] # remove README file name

# get 20% of file names to include twice for reliability testing
duplicate_files <- sample(fnames, round(0.2*length(fnames)), replace = F)

# join duplicate file names onto original file names
fnames <- c(fnames, duplicate_files)
fnames <- sample(fnames, replace = F)

f_key <- data.frame(original_name = fnames, anon_name = NA)

# get dates of files
f_key$date <- gsub(".*_2019", "2019", f_key$original_name)
f_key$date <- gsub("_...._.*", "", f_key$date)
f_key$date <- as.Date(f_key$date, format = "%Y%m%d")
f_key$month <- month(f_key$date, label = T, abbr = T)
f_key$day <- day(f_key$date)
f_key$wk_half <- NA # mark each day as first or second half of month
f_key$wk_half[f_key$day <= 15] <- "early"
f_key$wk_half[f_key$day > 15] <- "late"

# I start file name numbers not at one bc I don't want small numbers to be
# unconciously interpreted as "earlier".  
f_key$anon_name <- paste0("f", 30:(nrow(f_key)+29))
f_key$anon_name <- paste0(f_key$anon_name, "_", f_key$wk_half, "_", f_key$month, 
                          ".wav")

# save file so I can use it to rename audio files.
write_csv(f_key, path = "./anonymized_file_key.csv")
