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

#setwd("/home/emer/Dropbox/Ellie Roark/R/PointAbbaye/")


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




