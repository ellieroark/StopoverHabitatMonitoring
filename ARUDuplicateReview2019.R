################################
## ARU Duplicate Review Script- Point Abbaye 2019
## 
## author: Ellie Roark
## created: 10 Oct 2019
## last modified: 16 Oct 2019
## 
## inputs: *DataPrep_SR~counttype_10mincts_2019.R - sources and cleans 2019 field
##          season data
##
## outputs: *spdet_paired -- dataframe with all aru and pt counts, with one of 
##            each duplicate aru count randomly selected for inclusion
##          *w.arudup - dataframe with species presence/absence for duplicate 
##            aru counts in wide format (used in recording_reliability.R 
##            script to check reliability of listening to recordings)
##          
##            
## TODO: 
################################

library(Hmisc)
library(tidyverse)

newkrip <- FALSE

#setwd("/home/emer/Dropbox/Ellie Roark/R/PointAbbaye/")

## read in oiriginal data again because later in the script we need to create 
## df that treats each 30 sec interval as a sampling event
widearu <- read_csv(file = "./data/ARUPointCounts_WIDE_PtAbbaye2019.csv")
longaru <- read_csv(file = "./data/ARUPointCounts_PtAbbaye2019.csv")
filenames <- read_csv(file = "./data/anonymized_file_key.csv")


#get rid of column values irrelevant for this analysis
allaru_s <- subset(allaru, select = c(anon_filename, original_name, species_code))

## convert data to wide format
# create dummy "present" variable
w.allaru <- allaru_s
w.allaru$present <- 1
# actually spread to wide format
w.allaru <- spread(w.allaru, species_code, present, fill = 0)

## subset to only aru counts that are duplicated
arudup <- table(w.allaru$original_name)
w.arudup <- w.allaru[w.allaru$original_name %in% names(arudup[arudup > 1]), ]

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
t.arudup <- table(spdet_aru$original_name)
spdet_arudup <- spdet_aru[spdet_aru$original_name %in% 
                            names(t.arudup[t.arudup > 1]), ]
#order spdet_arudup by original filename
spdet_arudup <- spdet_arudup[order(spdet_arudup$original_name), ]

#randomly sample one of each duplicate aru recording
drop_anon <- c()
for (i in 1:length(unique(spdet_arudup$original_name))){
  nm <- unique(spdet_arudup$original_name)[i]
  pair <- spdet_arudup$anon_filename[spdet_arudup$original_name == nm]
  dr <- sample(pair, size = 1)
  drop_anon <- c(drop_anon, dr)
}

#eliminate duplicate aru values selected in drop_anon from spdet_paired 
# and from spdet_aru
spdet_aru <- spdet_aru[spdet_aru$anon_filename %nin% drop_anon, ]
spdet_paired <- spdet_paired[spdet_paired$anon_filename %nin% drop_anon, ]


if (newkrip) {
#### PREP FOR KRIPPENDORF'S ALPHA REVIEW OF GCKI AND WIWR OBSERVATIONS----------

## GCKI data prep
# #subset longaru to gcki
# kgcki <- longaru[which(longaru$species_code == "GCKI"), ]
# 
# #subset widearu to gcki
# kgckiw <- widearu[which(widearu$species_code == "GCKI"), ]

kw <- widearu
kl <- longaru

# prep widearu kinglet samples
kw[is.na(kw)] <- "0"
kw[4:23] <- as.integer(kw[4:23] != 0)

#add .wav extension to filenames
kw$anon_filename <- paste(kw$anon_filename, ".wav", sep = "")

## add up number of 30 sec periods in which a GCKI was detected- long aru
# change minute half values from 1 + 2 to :00 and :30
kl[which(kl$minute_half == "1"), "minute_half"] <- ":00"
kl[which(kl$minute_half == "2"), "minute_half"] <- ":30"
# paste those minute half values onto the minute_detected col
kl$minute_detected <- paste0(as.character(kl$minute_detected), 
                                   as.character(kl$minute_half))
kl$minute_half <- NULL

# pivot_wider to make each half min a column
### TODO: DEBUG HERE!!! right now det code is converting to a list when pivoting
###  wide. why? fix this!!!
kripdf <- pivot_wider(kl, names_from = "minute_detected", 
                        values_from = "det_code")
kripdf[is.na(kripdf)] <- "0"
kripdf[4:9] <- as.integer(kripdf[4:9] != 0)
kripdf <- rename(kripdf, anon_filename = "anon_file_name")

kripdf <- full_join(kw, kripdf)

kripdf[is.na(kripdf)] <- 0

#get rid of columns we don't need in filenames
filenames <- subset(filenames, select = c(original_name, anon_name))
#change column name to match allaru
filenames <- rename(filenames, anon_filename = "anon_name")

#join filenames to allaru to add a column that has the un-anonymized file names
kripdf <- left_join(kripdf, filenames, by = "anon_filename")

#get rid of species common name column (not needed)
kripdf$species_common_name <- NULL

## subset to only aru counts that are duplicated
dup <- table(kripdf$original_name)
kripdup <- kripdf[kripdf$original_name %in% names(dup[dup > 1]), ]

# order df by original file name so duplicates appear next to each other. 
kripdup <- kripdup[order(kripdup$original_name, kripdup$species_code),]
}

# clean up environment
rm(allaru_s, dup_no_match, match, no_match, spdet_arudup, w.allaru, w.arudup,
   arudup, dr, drop_anon, nm, pair, t.arudup)
