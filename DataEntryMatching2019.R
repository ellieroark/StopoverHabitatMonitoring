################################
## Data Entry Matching Script- Point Abbaye 2019
## 
## author: Ellie Roark
## created: 8 July 2019
## last modified: 14 Aug 2019
## 
## inputs: *PointCounts_PtAbbaye2019.csv- spreadsheet with observations from 
##          in person point counts during the 2019 field season at Point Abbaye
##        * REENTRY_PointCounts_PtAbbaye2019.csv- spreadsheet with same pt ct 
##          observations, re-entered
##        * ARUPointCounts_WIDE_PtAbbaye2019.csv- spreadsheet with observations
##          from listening to ARU recordings of 10 minute point count periods
##        * REENTRY_ARUPointCounts_WIDE_PtAbbaye2019.csv- spreadsheet with same
##          ARU point count listening observations, re-entered
##
## outputs: *NONE-- this script only ensures the double-entered spreadsheets match.
##            They are subsequently saved as NEW .csv files once errors found 
##            using this script are corrected by hand in the original .csv files.
##          
##            
## TODO: 
################################


library(Hmisc)
library(tidyverse)
library(lubridate)

setwd("/home/emer/Dropbox/Ellie Roark/R/PointAbbaye/")
ptct_df1 <- read_csv(file = "./2019data/PointCounts_PtAbbaye2019.csv", 
                     col_types = "ccccccccccccccccccccc")
ptct_df2 <- read_csv(file = "./2019data/REENTRY_PointCounts_PtAbbaye2019.csv", 
                     col_types = "ccccccccccccccccccccc")

ARU_df1 <- read_csv(file = "./2019data/ARUPointCounts_WIDE_PtAbbaye2019.csv", 
                    col_types = "cccccccccccccccccccccccc")

ARU_df2 <- read_csv(file = "./2019data/REENTRY_ARUPointCounts_WIDE_PtAbbaye2019.csv", 
                    col_types = "cccccccccccccccccccccccc")

## compare duplicate pt ct data frames -----------------------------------------

# add "no birds" to the species_common_name column of ptct_df1 
ptct_df1$species_common_name[ptct_df1$species_code == "no birds"] <- "no birds"

#re-order columns
ptct_df1 <- ptct_df1[, c(1:6, 9:12, 7, 8, 13:21)]
identical(colnames(ptct_df1), colnames(ptct_df2))

# check to see if data frames are identical
identical(ptct_df1, ptct_df2)
identical(ptct_df1[1, ], ptct_df2[1, ])

# run through each line one by one to see if identical
for(i in 1:nrow(ptct_df2)){
  if(!identical(ptct_df2[i, 1:20],ptct_df1[i, 1:20])) {
    print(i)
    print(data.frame(ptct_df2[i, ]))
    print(data.frame(ptct_df1[i, ]))
    print(colnames(ptct_df1)[ptct_df1[i, 1:20] != ptct_df2[i, 1:20]])
    stop("Look! A seagull!")
  }
}

## end compare duplicate pt ct data frames -------------------------------------


## compare duplicate ARU pt ct data frames -----------------------------------------

#check to make sure columns are in order
identical(colnames(ARU_df1), colnames(ARU_df2))

# check to see if data frames are identical
identical(ARU_df1, ARU_df2)
identical(ARU_df1[1, ], ARU_df2[1, ])

# run through each line one by one to see if identical
for(i in 1:nrow(ARU_df2)){
  if(!identical(ARU_df2[i, 1:23],ARU_df1[i, 1:23])) {
    print(i)
    print(data.frame(ARU_df2[i, ]))
    print(data.frame(ARU_df1[i, ]))
    print(colnames(ARU_df1)[ARU_df1[i, 1:23] != ARU_df2[i, 1:23]])
    stop("Look! A seagull!")
  }
}

## end compare duplicate ARU pt ct data frames -------------------------------------
