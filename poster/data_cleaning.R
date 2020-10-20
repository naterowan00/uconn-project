#####################################
### Created by: Nate Rowan
### Date Created: 9/16/2020
### Date Last Modified: 10/16/2020
### Description: This script contains several functions that are used
### to add variables to the Baseball Savant pitch data. Primarily, it 
### adds information about the previous pitch to each pitch in the data
### frame. The second half of the script applies these functions to the
### 2019 Baseball Savant data we read in in the previous script
#####################################

library(tidyverse)


# define functions --------------------------------------------------------


###
# This function accepts a data frame from Baseball Savant as an argument, and 
# returns a data frame that has been arranged by pitcher name. This MUST be done before
# the add previous pitch function to ensure that the lead() part of the add_prev_pitch()
# function will work properly
###
arrange_by_pitcher <- function(data) {
  newdata <- data %>%
    arrange(desc(player_name))
  newdata
}

###
# This function accepts a data frame from Baseball Savant as an argument, 
# and returns a data frame. It adds to each row what the pitch type 
# and name of the previous pitch was. One important thing to note:
# it still adds the last pitch to the first pitch of an at-bat. We 
# have to be careful to exclude the first pitches of at-bats from 
# our future analysis
###
add_prev_pitch <- function(data) {
  newdata <- data %>%
    mutate(
      prev_pitch_type1 = lead(data$pitch_type),
      prev_pitch_name1 = lead(data$pitch_name),
      prev_pitch_type2 = lead(data$pitch_type, 2),
      prev_pitch_name2 = lead(data$pitch_name, 2)
    ) 
  newdata
}

###
# This function accepts a data frame from Baseball Savant and returns a 
# data frame. It adds an indicator of whether a pitch was a swinging strike
# or not, where 1 is a swinging strike, and 0 is not.
###
add_swstr_flag <- function(data) {
  newdata <- data %>%
    mutate(
      swstr = case_when(
        description == "swinging_strike" | description == "swinging_strike_blocked" ~ 1, 
        description != "swinging_strike" ~ 0
      )
    )
  newdata
}

###
# This function adds to each pitch whether the previous pitch was a swinging 
# strike
###
add_prev_swstr <- function(data) {
  newdata <- data %>%
    mutate(
      prev_pitch_swstr = lead(data$swstr)
    )
  newdata
}

###
# This function accepts a data frame from Baseball Savant and returns a data
# frame. It adds to each pitch what the description (ball, swinging strike, 
# foul, etc) to each pitch
###
add_prev_description <- function(data) {
  newdata <- data %>% 
    mutate(
      prev_pitch_description = lead(data$description)
    )
}  

###
# This function accepts a data frame from Baseball Savant and returns a data
# frame. It adds to each pitch an indicator called same_pitch that tells 
# whether it is the same as the last pitch type
###
add_same_pitch_variable <- function(data) {
  newdata <- data %>%
    mutate(
      same_pitch = case_when(
        prev_pitch_type1 == pitch_type ~ TRUE,
        T ~ FALSE
      )
    )
  newdata
}

###
# This function accepts a data frame from Baseball Savant and returns a data
# frame. It adds a variable that groups the previous pitch description into
# six categories: ball, called strike, swinging strike, foul, foul tip or other
###
group_prev_pitch_description <- function(data) {
  newdata <- data %>%
    mutate(
      prev_pitch_description_group = case_when(
        prev_pitch_description == "ball" | prev_pitch_description == "blocked_ball" ~ "ball",
        prev_pitch_description == "swinging_strike_blocked" | 
          prev_pitch_description == "swinging_strike" ~ "swinging_strike",
        prev_pitch_description == "foul" ~ "foul",
        prev_pitch_description == "foul_tip" ~ "foul_tip",
        prev_pitch_description == "called_strike" ~ "called_strike",
        T ~ "other"
      )
    )
  newdata
}

###
# This function accepts a data frame from Baseball Savant and returns a data
# frame. It adds a character variable to the data frame with the current pitch
# count.
###
add_pitch_count <- function(data) {
  newdata <- data %>%
    unite(
      "count", balls, strikes, sep = "-", remove = FALSE
    )
  newdata
}


# data cleaning -----------------------------------------------------------


### Arrange 2019 pitches tibble by pitcher name
all_pitches <- all_pitches %>%
  arrange_by_pitcher()

### Adds previous two pitch types to each pitch
all_pitches <- all_pitches %>%
  add_prev_pitch()

### Adds indicator of whether the pitch resulted in swinging strike
all_pitches <- all_pitches %>% 
  add_swstr_flag()

### Adds to each pitch whether the previous pitch was a swinging strike
all_pitches <- all_pitches %>%
  add_prev_swstr()

### Adds to each pitch the description of the previous pitch 
all_pitches <- all_pitches %>%
  add_prev_description()

### Adds to each pitch whether it was the same as the last pitch
all_pitches <- all_pitches %>%
  add_same_pitch_variable()

### Adds previous pitch description group to each pitch
all_pitches <- all_pitches %>% 
  group_prev_pitch_description()

### Adds pitch count to each pitch
all_pitches <- all_pitches %>%
  add_pitch_count()