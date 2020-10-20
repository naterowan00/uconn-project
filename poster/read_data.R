#####################################
### Created by: Nate Rowan
### Date Created: 9/14/2020
### Date Last Modified: 9/15/2020
### Description: This script is used to read in all 2019 pitches. I need to 
### run this before doing any analysis
#####################################

### Important note: This is a ton of data, so I haven't actually uploaded it to
### GitHub. It is all availabe at https://baseballsavant.mlb.com/statcast_search
### I went through and downloaded all pitches from the 2019 season week-by-week

library(plyr) ### Used for the ldply() function
library(tidyverse)

### I have a .csv file containing all the pitches from each week of the 2019 MLB
### season, so I used the following chunk of code to combine them all into
### one tibble. Code chunk is adapted from this link:
### https://datascienceplus.com/how-to-import-multiple-csv-files-simultaneously-in-r-and-create-a-data-frame/

mydir = "datasets"
myfiles = list.files(path = mydir, pattern = "2019_pitch_data_\\d\\d.csv", full.names = TRUE)

all_pitches <- ldply(myfiles, read_csv, na = c("null", "", "NA"))

### Convert to a tibble
all_pitches <- as_tibble(all_pitches)



