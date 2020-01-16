################################
## 20 random minutes audio file prep
## Point Abbaye 2019
## 
## author: Ellie Roark
## created: 1 November 2019
## last modified: 15 December 2019
## 
## inputs: *2019 point abbaye audio files, all days, all recorders
##         
## outputs: *20random minutes selected from each aru on each survey day of the 
##            field season. 
## 
## TODO: * 
################################

library(warbleR)
library(Hmisc)
library(tidyverse)
library(lubridate)
library(Rraven)

setwd("/media/emer/LaCie/PointAbbaye2019/AudioData/")

# first: create identifiers for each minute of the recording
# letters indicate the hour (file) on a given day (a=hour1, b=hour2, etc.)
fileid <- c("a", "b", "c", "d", "e")
# numbers identify the minute in that file
# only go through min 58 because min 59 is not exactly 60 seconds long
minid <- c(0:58)
# create table with all 300 min of recording
min_id <- expand.grid(fileid, minid)
# make sensible column names for the table
min_id <- rename(min_id, filecode = "Var1", minute = "Var2")

# set seed to ensure reproducibility of random selections
set.seed(10061983)

# make a random ID code for each day of the year
doy_id <- data.frame(doy = 1:366, code = sample(1:366, replace = F))
doy_id$code <- paste0("day", doy_id$code)

# make random ID for recorder
rec_id <- data.frame(rec = c("SWIFT01", "SWIFT02", "SWIFT03", "AM"), 
                     code = sample(1:4, replace = F))
rec_id$code <- paste0("rec", rec_id$code)

# make dataframe to hold the anonymized file names and the information that 
# says what day and minute those are from
filename_key <- data.frame()

# now: begin with SWIFT01 folder!-----------------------------------------------
# get a list of all folders in SWIFT01_001 dir
s1all_dirs <- list.dirs(path = "./SWIFT01_001/", full.names = FALSE)

# for loop that will create a warbleR selection table of randomly selected 
# minutes for each day of data: 

# make list to hold writing success indicator
#outs <- c() 
#loop through each folder (day)

for(i in 2:length(s1all_dirs)){
  #get a list of 24 random minutes
  sample1 <- sample_n(min_id, 24)
  sample1 <- sample1[order(sample1$filecode, sample1$minute), ]
  
  #create selection_table that will select minutes from audio files
  #make sure sample is in selection_df format
  sample1$selec <- paste0(sample1$filecode, sample1$minute)
  sample1$start <- sample1$minute
  sample1$end <- sample1$start + 1
  #convert start and end times to seconds
  sample1$start <- sample1$start*60
  sample1$end <- sample1$end*60
  #list all files in the day folder
  f_names <- list.files(path = paste0("/media/emer/LaCie/PointAbbaye2019/AudioData/SWIFT01_001/", 
                                      s1all_dirs[i], "/"))
  #make a df with the names of those files with letters a-e assigned to each file
  temp_df <- data.frame(filecode = fileid, sound.files = f_names)
  #join that df with filenames to the random sample df
  sample1 <- left_join(sample1, temp_df, by = "filecode")
  #convert to a selection table
  sample1 <- selection_table(sample1, 
                             path = paste0("/media/emer/LaCie/PointAbbaye2019/AudioData/SWIFT01_001/", 
                                      s1all_dirs[i], "/"))
  
  # Make column for anonymized name
  sample1$anon_name <- NA # this will be filled one row at a time
  
  # get dates of files
  sample1$date <- gsub(".*_2019", "2019", sample1$sound.files)
  sample1$date <- gsub("_...._.*", "", sample1$date)
  sample1$date <- as.Date(sample1$date, format = "%Y%m%d")
  sample1$month <- month(sample1$date, label = T, abbr = T)
  sample1$day <- day(sample1$date)
  sample1$wk_half <- NA # mark each day as first or second half of month
  sample1$wk_half[sample1$day <= 15] <- "early"
  sample1$wk_half[sample1$day > 15] <- "late"
  
  #loop through selection table sample1 to read in each randomly selected minute
  for(j in 1:nrow(sample1)){
      # read in file from a single row in selection table
      minclip <- read_wave(sample1, index = j, 
                           path = paste0("/media/emer/LaCie/PointAbbaye2019/AudioData/SWIFT01_001/", 
                                         s1all_dirs[i], "/"))
      
      # create an anonymous file name with a day ID, recorder ID, 2 week time
      # window, and unique minute identifier (for this day and recorder)
      d <- doy_id$code[doy_id$doy == yday(sample1$date[j])]
      rec <- rec_id$code[rec_id$rec == "SWIFT01"]
      newfile <- file.path(paste0(d, "_", rec, "_", sample1$wk_half[j], 
                                  sample1$month[j], "_", sample1$selec[j], 
                                  ".wav"))
      
      sample1$anon_name[j] <- newfile
      
      #write minute clip file to relevant 20 rand min folder. 
      writeWave(minclip, filename = paste0("/media/emer/LaCie/PointAbbaye2019/AudioData/20RandomMin/", 
                                           newfile))
  
      
      # #write selected file to the 20 random min folder
      # # IF writing the file returns an error, add the name of the file to a list
      # outs <- c(outs, tryCatch({
      #   savewav(minclip,
      #           filename = paste0("/media/emer/LaCie/PointAbbaye2019/AudioData/20RandomMin/SWIFT01/", 
      #                             newfile), rescale = c(range(minclip@left)))}, 
      #   error = function(x, failed = newfile) {as.character(failed)}))
  }
  filename_key <- bind_rows(filename_key, sample1)
}

# END SWIFT01 folder!----------------------------------------------------------

# Begin SWIFT02 folder--------------------------------------------------------
# get a list of all folders in SWIFT02_001 dir
s1all_dirs <- list.dirs(path = "./SWIFT02_001/", full.names = FALSE)

#loop through each folder (day)
for(i in 2:length(s1all_dirs)){
  #get a list of 24 random minutes
  sample1 <- sample_n(min_id, 24)
  sample1 <- sample1[order(sample1$filecode, sample1$minute), ]
  
  #create selection_table that will select minutes from audio files
  #make sure sample is in selection_df format
  sample1$selec <- paste0(sample1$filecode, sample1$minute)
  sample1$start <- sample1$minute
  sample1$end <- sample1$start + 1
  #convert start and end times to seconds
  sample1$start <- sample1$start*60
  sample1$end <- sample1$end*60
  #list all files in the day folder
  f_names <- list.files(path = paste0("/media/emer/LaCie/PointAbbaye2019/AudioData/SWIFT02_001/", 
                                      s1all_dirs[i], "/"))
  #make a df with the names of those files with letters a-e assigned to each file
  temp_df <- data.frame(filecode = fileid, sound.files = f_names)
  #join that df with filenames to the random sample df
  sample1 <- left_join(sample1, temp_df, by = "filecode")
  #convert to a selection table
  sample1 <- selection_table(sample1, 
                             path = paste0("/media/emer/LaCie/PointAbbaye2019/AudioData/SWIFT02_001/", 
                                           s1all_dirs[i], "/"))
  
  # Make column for anonymized name
  sample1$anon_name <- NA # this will be filled one row at a time
  
  # get dates of files
  sample1$date <- gsub(".*_2019", "2019", sample1$sound.files)
  sample1$date <- gsub("_...._.*", "", sample1$date)
  sample1$date <- as.Date(sample1$date, format = "%Y%m%d")
  sample1$month <- month(sample1$date, label = T, abbr = T)
  sample1$day <- day(sample1$date)
  sample1$wk_half <- NA # mark each day as first or second half of month
  sample1$wk_half[sample1$day <= 15] <- "early"
  sample1$wk_half[sample1$day > 15] <- "late"
  
  #loop through selection table sample1 to read in each file
  for(j in 1:nrow(sample1)){
    # read in file from a single row in selection table
    minclip <- read_wave(sample1, index = j, 
                         path = paste0("/media/emer/LaCie/PointAbbaye2019/AudioData/SWIFT02_001/", 
                                       s1all_dirs[i], "/"))

    # create an anonymous file name with a day ID, recorder ID, 2 week time
    # window, and unique minute identifier (for this day and recorder)
    d <- doy_id$code[doy_id$doy == yday(sample1$date[j])]
    rec <- rec_id$code[rec_id$rec == "SWIFT02"]
    newfile <- file.path(paste0(d, "_", rec, "_", sample1$wk_half[j], 
                                sample1$month[j], "_", sample1$selec[j], 
                                ".wav"))
    
    sample1$anon_name[j] <- newfile
    
    #write minute clip file to relevant 20 rand min folder. 
    writeWave(minclip, filename = paste0("/media/emer/LaCie/PointAbbaye2019/AudioData/20RandomMin/", 
                                         newfile))
  }
  filename_key <- bind_rows(filename_key, sample1)
}
# end SWIFT02 folder------------------------------------------------------------

# Begin SWIFT03 folder----------------------------------------------------------
# get a list of all folders in SWIFT01_001 dir
s1all_dirs <- list.dirs(path = "./SWIFT03_002/", full.names = FALSE)

#loop through each folder (day)
for(i in 2:length(s1all_dirs)){
  #get a list of 24 random minutes
  sample1 <- sample_n(min_id, 24)
  sample1 <- sample1[order(sample1$filecode, sample1$minute), ]
  
  #create selection_table that will select minutes from audio files
  #make sure sample is in selection_df format
  sample1$selec <- paste0(sample1$filecode, sample1$minute)
  sample1$start <- sample1$minute
  sample1$end <- sample1$start + 1
  #convert start and end times to seconds
  sample1$start <- sample1$start*60
  sample1$end <- sample1$end*60
  #list all files in the day folder
  f_names <- list.files(path = paste0("/media/emer/LaCie/PointAbbaye2019/AudioData/SWIFT03_002/", 
                                      s1all_dirs[i], "/"))
  #make a df with the names of those files with letters a-e assigned to each file
  temp_df <- data.frame(filecode = fileid, sound.files = f_names)
  #join that df with filenames to the random sample df
  sample1 <- left_join(sample1, temp_df, by = "filecode")
  #convert to a selection table
  sample1 <- selection_table(sample1, 
                             path = paste0("/media/emer/LaCie/PointAbbaye2019/AudioData/SWIFT03_002/", 
                                           s1all_dirs[i], "/"))
  
  # Make column for anonymized name
  sample1$anon_name <- NA # this will be filled one row at a time
  
  # get dates of files
  sample1$date <- gsub(".*_2019", "2019", sample1$sound.files)
  sample1$date <- gsub("_...._.*", "", sample1$date)
  sample1$date <- as.Date(sample1$date, format = "%Y%m%d")
  sample1$month <- month(sample1$date, label = T, abbr = T)
  sample1$day <- day(sample1$date)
  sample1$wk_half <- NA # mark each day as first or second half of month
  sample1$wk_half[sample1$day <= 15] <- "early"
  sample1$wk_half[sample1$day > 15] <- "late"
  
  #loop through selection table sample1 to read in each file
  for(j in 1:nrow(sample1)){
    # read in file from a single row in selection table
    minclip <- read_wave(sample1, index = j, 
                         path = paste0("/media/emer/LaCie/PointAbbaye2019/AudioData/SWIFT03_002/", 
                                       s1all_dirs[i], "/"))

    # create an anonymous file name with a day ID, recorder ID, 2 week time
    # window, and unique minute identifier (for this day and recorder)
    d <- doy_id$code[doy_id$doy == yday(sample1$date[j])]
    rec <- rec_id$code[rec_id$rec == "SWIFT03"]
    newfile <- file.path(paste0(d, "_", rec, "_", sample1$wk_half[j], 
                                sample1$month[j], "_", sample1$selec[j], 
                                ".wav"))
    
    sample1$anon_name[j] <- newfile

    #write minute clip file to relevant 20 rand min folder. 
    writeWave(minclip, filename = paste0("/media/emer/LaCie/PointAbbaye2019/AudioData/20RandomMin/", 
                                         newfile))
  }
  filename_key <- bind_rows(filename_key, sample1)
}
# end SWIFT 03 folder-----------------------------------------------------------

# Begin AudioMoth folder--------------------------------------------------------
# loop that will create selection table for each day and then select and save
# those selected files
# get a list of all folders in AudioMoth dir
s1all_dirs <- list.dirs(path = "./AudioMoth/", full.names = FALSE)

#loop through each folder (day)
for(i in 2:length(s1all_dirs)){
  #get a list of 24 random minutes- 20 to listen to, 4 extra in case there are human
  # voices on recording.
  sample1 <- sample_n(min_id, 24)
  sample1 <- sample1[order(sample1$filecode, sample1$minute), ]
  
  #create selection_table that will select minutes from audio files
  #make sure sample is in selection_df format
  sample1$selec <- paste0(sample1$filecode, sample1$minute)
  sample1$folder <- paste0(s1all_dirs[i])
  sample1$start <- sample1$minute
  sample1$end <- sample1$start + 1
  #convert start and end times to seconds
  sample1$start <- sample1$start*60
  sample1$end <- sample1$end*60
  #list all files in the day folder
  f_names <- list.files(path = paste0("/media/emer/LaCie/PointAbbaye2019/AudioData/AudioMoth/", 
                                      s1all_dirs[i], "/"))
  #make a df with the names of those files with letters a-e assigned to each file
  temp_df <- data.frame(filecode = fileid, sound.files = f_names)
  #join that df with filenames to the random sample df
  sample1 <- left_join(sample1, temp_df, by = "filecode")
  #convert to a selection table
  sample1 <- selection_table(sample1, 
                             path = paste0("/media/emer/LaCie/PointAbbaye2019/AudioData/AudioMoth/", 
                                           s1all_dirs[i], "/"))
  
  sample1$anon_name <- NA # this will be filled one row at a time
  
  # get dates of files
  sample1$date <- sample1$folder
  sample1$date <- as.Date(sample1$date, format = "%m-%d-%Y")
  sample1$month <- month(sample1$date, label = T, abbr = T)
  sample1$day <- day(sample1$date)
  sample1$wk_half <- NA # mark each day as first or second half of month
  sample1$wk_half[sample1$day <= 15] <- "early"
  sample1$wk_half[sample1$day > 15] <- "late"
  
  #loop through selection table sample1 to read in each file
  for(j in 1:nrow(sample1)){
    # read in file from a single row in selection table
    minclip <- read_wave(sample1, index = j, 
                         path = paste0("/media/emer/LaCie/PointAbbaye2019/AudioData/AudioMoth/", 
                                       s1all_dirs[i], "/"))

    # create an anonymous file name with a day ID, recorder ID, 2 week time
    # window, and unique minute identifier (for this day and recorder)
    d <- doy_id$code[doy_id$doy == yday(sample1$date[j])]
    rec <- rec_id$code[rec_id$rec == "AM"]
    newfile <- file.path(paste0(d, "_", rec, "_", sample1$wk_half[j], 
                                sample1$month[j], "_", sample1$selec[j], 
                                ".wav"))
    
    sample1$anon_name[j] <- newfile
    
    #write minute clip file to relevant 20 rand min folder. 
    writeWave(minclip, filename = paste0("/media/emer/LaCie/PointAbbaye2019/AudioData/20RandomMin/", 
                                         newfile))
  }
  filename_key <- bind_rows(filename_key, sample1)
}
# end AudioMoth folder----------------------------------------------------------

write_csv(filename_key, path = "/media/emer/LaCie/PointAbbaye2019/AudioData/20RandomMin/filename_key.csv")


# #test random sampling of table to retrieve a list of 25 random minutes
# 
# sample1 <- sample_n(min_id, 25)
# sample1 <- sample1[order(sample1$filecode, sample1$minute), ]
# 
# #EXAMPLE: create selection_table that will select minutes from audio files
# #set working dir to SWIFT01's April 2nd folder
# setwd("/media/emer/LaCie/PointAbbaye2019/AudioData/SWIFT01_001/SWIFT01_2019-04-02")
# sample1$selec <- paste0(sample1$filecode, sample1$minute)
# sample1$start <- sample1$minute
# sample1$end <- sample1$start + 1
# sample1$start <- sample1$start*60
# sample1$end <- sample1$end*60
# f_names <- unique(list.files(path = "./"))
# temp_df <- data.frame(filecode = fileid, sound.files = f_names)
# fsample <- left_join(sample1, temp_df, by = "filecode")
# selection_table(fsample)
# 
# # test read in files from selection table
# x1 <-read_wave(fsample, index = 1)
# setwd("/media/emer/LaCie/PointAbbaye2019/AudioData/20RandomMin")
# savewav(x1, filename = "./SWIFT01_20190402_a10.wav")
# 
# 
# ######### warbleR VIGNETTE INTRO EXAMPLE #######################################
# dir.create(file.path(getwd(),"warbleR_example"))
# setwd(file.path(getwd(),"warbleR_example"))
# 
# # Load Raven example selection tables
# data("selection_files")
# 
# # Write out Raven example selection tables as physical files
# out <- lapply(1:2, function(x) 
#   writeLines(selection_files[[x]], con = names(selection_files)[x]))
# 
# # Write example sound files out as physical .wav files
# data(list = c("Phae.long1", "Phae.long2"))
# 
# writeWave(Phae.long1, "Phae.long1.wav")
# writeWave(Phae.long2, "Phae.long2.wav")
# 
# sels <- imp_raven(sound.file.col = "Begin.File", ext.case = "lower", 
#                   all.data = FALSE, freq.cols = FALSE)
# str(sels)
# 
# # Write out the imported selections as a .csv for later use
# write.csv(sels, "Raven_sels.csv", row.names = FALSE)
