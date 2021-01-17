################################
## Species Diversity and Turnover between survey methods
## Point Abbaye 2019
## 
## author: Ellie Roark
## created: 31 October 2019
## last modified: 17 January 2021
## 
## inputs: *ARUDuplicateReview2019- script that randomly selects which of the 
##            duplicate ARU recordings to use, and sources:
##            DataCleaningPtAbbaye2019.R- which loads the aru and point ct data 
##            from 2019 and returns the following:
##            - allaru: df of every 10 min paired ct aru observation
##            - ptct: df of every point count observation
##            - spdet_paired: df with # of species detected per count by count
##              type
##
##         
## outputs: *table showing species detected by method
##            
## TODO: * 
################################

library(Hmisc)
library(tidyverse)

#setwd("/home/emer/Dropbox/Ellie Roark/R/PointAbbaye/")

##don't forget to run data prep and ARUDuplicate script before running this script! See 
## StopoverHabitatMonitoring.R for details. 

alpha_codes <- read_csv(file = "./data/BirdAlphaCodes.csv")

# read in original random minute ARU data file with calls and songs noted
aru_CS <- read_csv(file = "./data/ARU20randmin_final.csv")


## transform original 10 consecutive minute data from longaru and widearu objects
## to a single dataframe that contains not only the names of species but also the 
## number of detections

## transform longaru into a wide version of the data
  # change minute half values from 1 + 2 to :00 and :30
longaru[which(longaru$minute_half == "1"), "minute_half"] <- ":00"
longaru[which(longaru$minute_half == "2"), "minute_half"] <- ":30"
# paste those minute half values onto the minute_detected col
longaru$minute_detected <- paste0(as.character(longaru$minute_detected), 
                                   as.character(longaru$minute_half))
longaru$minute_half <- NULL

# fix error that assigns two S detections of YRWA to min 1:00 of a recording and
# two C detections of CANG to minute 3:00 of a recording, by getting only unique
# rows from longaru
longaru <- distinct(longaru)

# fix error of downy woodpecker "singing" detections (should be drums)
widearu[which(widearu$anon_filename == "f86_early_May"), '9:00'] <- "D"
widearu[which(widearu$anon_filename == "f86_early_May"), '9:30'] <- "D"

# pivot_wider to make each half min a column
ltow <- pivot_wider(longaru, names_from = "minute_detected", 
                        values_from = "det_code")

# prep ltow for joining with widearu
ltow <- rename(ltow, anon_filename = "anon_file_name")
#get rid of species_common_name from widearu... we'll replace it in a moment
# with automated values from alpha_codes
widearu$species_common_name <- NULL

# join ltow and widearu to get a complete dataframe of 10 consec min ARU data
allaru_det <- bind_rows(widearu, ltow)

#get list of species detected by aru counts
#first add common name to aru counts-- ARU 10 consec counts
aru_names_10c <- left_join(
  allaru_det, 
  alpha_codes[, which(colnames(alpha_codes) %in% c("SPEC", "COMMONNAME"))], 
  by = c("species_code" = "SPEC"))
colnames(aru_names_10c)[colnames(aru_names_10c)=="COMMONNAME"] <- "species_common_name"

#first add common name to aru counts-- ARU random counts
aru_names_rand <- left_join(
  aru_CS, 
  alpha_codes[, which(colnames(alpha_codes) %in% c("SPEC", "COMMONNAME"))], 
  by = c("species_code" = "SPEC"))
colnames(aru_names_rand)[colnames(aru_names_rand)=="COMMONNAME"] <- "species_common_name"

#select only species common names for all ARU count types and bind rows
# aru_names_10c <- select(aru_names_10c, "species_common_name", "aru_id", 
#                         "species_code")
# aru_names_rand <- select(aru_names_rand, "species_common_name", "aru_id", 
#                         "species_code")
                  
# prep for join-- change filename to anon_filename
aru_names_rand <- rename(aru_names_rand, anon_filename = "filename")

#join 10 consecutive minute records and random minute ARU records
aru_names <- bind_rows(aru_names_10c, aru_names_rand)

## remove rows with "no birds" counts
aru_names <- aru_names[which(aru_names$species_code != "no birds"), ]

#add in "sp" taxa to species common name
for (i in 1:nrow(aru_names)){
  if (aru_names$species_code[i] == "passerine sp."){
    aru_names$species_common_name[i] <- "passerine sp."
  }
  else if (aru_names$species_code[i] == "bird sp."){
    aru_names$species_common_name[i] <- "bird sp."
  }
  else if (aru_names$species_code[i] == "corvid sp."){
    aru_names$species_common_name[i] <- "corvid sp."
  }
  else if (aru_names$species_code[i] == "flycatcher sp."){
    aru_names$species_common_name[i] <- "flycatcher sp."
  }
  else if(aru_names$species_code[i] == "woodpecker sp."){
    aru_names$species_common_name[i] <- "woodpecker sp."
  }
  else if(aru_names$species_code[i] == "setophaga sp."){
    aru_names$species_common_name[i] <- "setophaga sp."
  }
}

#change common name of RCKI and SWTH in ptct data so it matches aru data
for (i in 1:nrow(ptct)){
  if (ptct$species_code[i] == "RCKI"){
    ptct$species_common_name[i] <- "Ruby-crowned Kinglet"
  }
  if (ptct$species_code[i] == "SWTH"){
    ptct$species_common_name[i] <- "Swainson's Thrush"
  }
}

#get list of species detected by arus
arudet <- unique(aru_names$species_common_name)
#subset to only unique species 
arudet <- unique(arudet)

#get list of species detected by point counts
ptdet <- unique(ptct$species_common_name)

#get df of detections of species seen on pt cts but not on aru cts
onlypt <- ptct[ptct$species_common_name %nin% arudet, ]

#get df of detections of species seen on aru cts but not on ptcts
onlyaru <- aru_names[aru_names$species_common_name %nin% ptdet, ]

# How many times was each bird detected by each detection code?
# create dataframe that will hold the number of detections of each type for each
# species
sp_detection_method <- data.frame(species_common_name = 
                                    unique(c(ptct$species_common_name, 
                                             onlyaru$species_common_name)), 
                                  det_V = NA, det_C = NA, det_S = NA, 
                                  det_D = NA, det_aru_C = NA, det_aru_S = NA, 
                                  det_aru_D = NA)

# add up the number of detections from point counts
for(i in 1:nrow(sp_detection_method)) {
  sdf <- ptct[ptct$species_common_name == 
                sp_detection_method$species_common_name[i], ]
  sp_detection_method$det_V[i] <- length(grep("V", sdf$det_code))
  sp_detection_method$det_C[i] <- length(grep("C", sdf$det_code))
  sp_detection_method$det_S[i] <- length(grep("S", sdf$det_code))
  sp_detection_method$det_D[i] <- length(grep("D", sdf$det_code))
}

# add up the number of detections from ARU counts
for(i in 1:nrow(sp_detection_method)) {
  this_sp <- aru_names[aru_names$species_common_name == 
                sp_detection_method$species_common_name[i], ]
  sp_detection_method$det_aru_S[i] <- sum(grepl("^S$", 
                                                as.character(unlist(this_sp)), 
                                                ignore.case = TRUE))
  sp_detection_method$det_aru_C[i] <- sum(grepl("^C$", 
                                                as.character(unlist(this_sp)), 
                                                ignore.case = TRUE))
  sp_detection_method$det_aru_D[i] <- sum(grepl("^D$", 
                                                as.character(unlist(this_sp)), 
                                                ignore.case = TRUE))
}

# # was this species ever detected by ARU?
# sp_detection_method$aru_det <- sp_detection_method$species_common_name %in% arudet
# 
# #change TRUE/FALSE aru det to yes/no
# sp_detection_method$aru_det[which(sp_detection_method$aru_det == TRUE)] <- "yes"
# sp_detection_method$aru_det[which(sp_detection_method$aru_det == FALSE)] <- "no"

#add scientific names to sp_detection_method
sp_detection_method <- left_join(sp_detection_method,
  alpha_codes[, which(colnames(alpha_codes) %in% c("COMMONNAME", "SCINAME"))], 
  by = c("species_common_name" = "COMMONNAME"))
colnames(sp_detection_method)[colnames(sp_detection_method)=="SCINAME"] <- "scientific_name"

non_species <- sp_detection_method[sp_detection_method$scientific_name %nin% 
                                     alpha_codes$SCINAME, ]
keep_sp <- sp_detection_method$scientific_name[
  sp_detection_method$scientific_name %in% alpha_codes$SCINAME]
sp_detection_method <- left_join(alpha_codes[, which(colnames(alpha_codes) %in%
                                                       c("COMMONNAME", "SCINAME"))], 
                                 sp_detection_method, 
                                 by = c("SCINAME" = "scientific_name"))
sp_detection_method <- sp_detection_method[sp_detection_method$SCINAME %in% 
                                             keep_sp, ]
sp_detection_method <- bind_rows(sp_detection_method, non_species)

write.csv(sp_detection_method, file = "./SpeciesDetByMethod_PtAbbaye2019.csv")
  