
########## IMPORTANT NOTE: MUST FILTER OUT FIRST PITCH OF AT-BAT MANUALLY ###########


library(tidyverse); theme_set(theme_minimal())
library(gt)
library(patchwork)

########## Functions ##############

###
# This function accepts a data frame from Baseball Savant as an argument, and 
# returns a data frame that has been arranged by pitcher name. This MUST be done before
# the add previous pitch function to ensure that the lead() part of the add previous
# pitch function will work properly
###
arrange_by_pitcher <- function(data) {
  newdata <- data %>%
    arrange(desc(player_name))
  newdata
}

###
# This function accepts a data frame from Baseball Savant as an argument, and returns
# a data frame. It adds to each row what the pitch type and name of the previous pitch was
# Additionally, it adds a flag indicating whether a pitch was the first pitch of a new at-bat
###
add_prev_pitch <- function(data) {
  newdata <- data %>%
    mutate(
      prev_pitch_type = lead(data$pitch_type),
      prev_pitch_name = lead(data$pitch_name),
      new_ab = case_when(
        pitch_number == 1 ~ TRUE,
        T ~ FALSE
      )
    ) 
  newdata
}

###
# This function accepts a Baseball Savant data frame as input, as well as a logical value 
# that states whether the user wants to include the first pitch of an at-bat in the 
# calculations. It creates a flag that indicates whether the previous and current pitch match.
# Then, It calculates the proportion of observations where the two pitches match.
###
calc_repeat_pitch_prop <- function(data, first_pitch = FALSE) {
  
  if(first_pitch == FALSE){
    data <- data %>%
      filter(new_ab == FALSE)
  }
  
  newdata <- data %>%
    mutate(
      same_pitch = case_when(
        pitch_type == prev_pitch_type ~ TRUE,
        T ~ FALSE
      )
    ) %>%
    select(pitch_type, prev_pitch_type, same_pitch) %>%
    summarise(
      repeat_pitch = mean(same_pitch)
    )
  newdata
}


########## Read in data ############

### What is this dataset? It's a random sample of 15 pitchers and all of their
### pitches from 2019. There are just under 40,000 pitches (the max that can 
### be downloaded from Savant as once

### Read in the data, add "null" as an NA value
all_data <- read_csv(
  here::here("datasets/random_pitcher_sample_2019.csv"),
  na = c("", "NA", "null")
)

### Arrange the pitchers by name
all_data <- all_data %>%
  arrange_by_pitcher()

### Calculate the proportion of pitches where Zack Greinke threw his previous pitch
all_data %>%
  filter(player_name == "Zack Greinke") %>%
  add_prev_pitch() %>%
  calc_repeat_pitch_prop()

### Perform same calculation for all non-Greinke pitchers
all_data %>%
  filter(player_name != "Zack Greinke") %>%
  arrange_by_pitcher() %>%
  add_prev_pitch() %>%
  calc_repeat_pitch_prop()

### Calculate proportion of repeated pitches for all pitchers
all_data %>%
  arrange_by_pitcher() %>%
  add_prev_pitch() %>%
  group_by(player_name) %>%
  calc_repeat_pitch_prop() %>%
  arrange(desc(repeat_pitch)) %>%
  mutate(
    repeat_pitch = round(repeat_pitch, 2)
  ) %>%
  gt()

#######################################################
### Can I predict the next pitch based on the previous pitch?
#######################################################

### Filter out only Greinke data
greinke <- all_data %>%
  filter(player_name == "Zack Greinke")

### Add the previous pitch to each row
greinke <- greinke %>%
  add_prev_pitch()

### Make pitch name a factor and order it the way I want
greinke <- greinke %>%
  mutate(
    pitch_name = as_factor(pitch_name),
    pitch_name = factor(
      pitch_name, 
      levels = c("4-Seam Fastball", 
                 "Changeup", 
                 "Slider", 
                 "Curveball", 
                 "2-Seam Fastball", 
                 "Eephus", 
                 "Cutter", 
                 "Split-Finger"))
  )

### Astros orange color
astros_orange <- "#eb6e1f"
astros_blue <- "#002d62"

### Pitch proportions given he has thrown a fastball (non-first pitch)
greinke %>%
  filter(prev_pitch_name == "4-Seam Fastball", new_ab == FALSE) %>%
  group_by(prev_pitch_name, pitch_name) %>%
  summarise(n = n()) %>%
  mutate(
    prop = n / sum(n)
  ) %>%
  filter(n > 5) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = pitch_name, y = prop)) +
    geom_col(fill = astros_orange) +
    geom_text(aes(label = round(prop, 2)), vjust = -.3) +
    labs(
      title = "Zack Greinke Pitch Distribution After Throwing Fastball, 2019 Season",
      subtitle = "Data via Baseball Savant",
      x = "Pitch",
      y = "Proportion",
      caption = "Visualization by Nate Rowan, @nate_rowan17"
    ) +
    scale_y_continuous(limits = c(0, .45))

### Pitch proportions in all situations (non-first pitch)
greinke %>%
  filter(new_ab == FALSE) %>% 
  group_by(pitch_name) %>%
  summarise(n = n()) %>%
  mutate(
    prop = n / sum(n)
  ) %>%
  filter(n > 10) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = pitch_name, y = prop)) +
    geom_col(fill = astros_blue) +
    geom_text(aes(label = round(prop, 2)), vjust = -.3) +
    labs(
      title = "Zack Greinke Pitch Distribution, 2019 Season",
      subtitle = "Data via Baseball Savant",
      x = "Pitch",
      y = "Proportion",
      caption = "Visualization by Nate Rowan, @nate_rowan17"
    ) +
    scale_y_continuous(limits = c(0, .45))
  

### Pitch proportions given he has thrown a changeup
greinke %>%
  filter(prev_pitch_name == "Changeup", new_ab == FALSE) %>%
  group_by(prev_pitch_name, pitch_name) %>%
  summarise(n = n()) %>%
  mutate(
    prop = n / sum(n)
  ) %>%
  filter(n > 1) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = pitch_name, y = prop)) +
    geom_col(fill = astros_orange) +
    geom_text(aes(label = round(prop, 2)), vjust = -.3) +
    labs(
      title = "Zack Greinke Pitch Distribution After Throwing Changeup, 2019 Season",
      subtitle = "Data via Baseball Savant",
      x = "Pitch",
      y = "Proportion",
      caption = "Visualization by Nate Rowan, @nate_rowan17"
    ) +
    scale_y_continuous(limits = c(0, .45))

### Put the entire Greinke pitch distribution into a table
greinke_non_first_pitch_counts <- greinke %>%
  filter(new_ab == FALSE) %>% 
  group_by(pitch_name) %>%
  summarise(n = n()) %>%
  mutate(
    prop = n / sum(n)
  ) %>%
  filter(n > 10) %>%
  arrange(desc(n))

### Put the pitch distribution post-changeup into a table
greinke_after_changeup_counts <- greinke %>%
  filter(prev_pitch_name == "Changeup", new_ab == FALSE) %>%
  group_by(prev_pitch_name, pitch_name) %>%
  summarise(n = n()) %>%
  mutate(
    prop = n / sum(n)
  ) %>%
  filter(n > 1) %>%
  arrange(desc(n))

### Join the normal distribution with the post-changeup distribution
greinke_pitch_dist <- greinke_non_first_pitch_counts %>%
  left_join(greinke_after_changeup_counts, by = "pitch_name") %>%
  select(-prev_pitch_name) %>%
  rename(
    n_total = n.x,
    prop_total = prop.x,
    n_after_ch = n.y,
    prop_after_ch = prop.y
  )



### Plot showing the how the distribution is affected by throwing a changeup
greinke_pitch_dist %>%
  ggplot(aes(x = pitch_name, y = prop_total)) +
    geom_col(aes(fill = "All Pitches")) +
    geom_col(aes(y = prop_after_ch, fill = "After Changeup"), width = .5, alpha = .8) +
    geom_text(aes(y = prop_after_ch, label = round(prop_after_ch, 2)), vjust = -.3) +
    labs(
      title = "How does Zack Greinke's pitch distribution change after throwing a changeup?",
      subtitle = "2019 season, data via Baseball Savant",
      x = "Pitch",
      y = "Proportion",
      caption = "Visualization by Nate Rowan, @nate_rowan17"
    ) +
    scale_y_continuous(limits = c(0, .45)) +
    scale_fill_manual(
      name = "Pitch Distribution",
      values = c("All Pitches" = astros_orange, "After Changeup" = astros_blue)
    ) +
    theme(
      legend.justification = c(1,1),
      legend.position = c(1,1)
    )

greinke_after_curveball_counts <- greinke %>%
  filter(prev_pitch_name == "Curveball", new_ab == FALSE) %>%
  group_by(prev_pitch_name, pitch_name) %>%
  summarise(n = n()) %>%
  mutate(
    prop = n / sum(n)
  ) %>%
  filter(n > 1) %>%
  arrange(desc(n))

temp_name <- greinke_non_first_pitch_counts %>%
  left_join(greinke_after_curveball_counts, by = "pitch_name") %>%
  select(-prev_pitch_name) %>%
  rename(
    n_total = n.x,
    prop_total = prop.x,
    n_after_cu = n.y,
    prop_after_cu = prop.y
  )

temp_name %>%
  ggplot(aes(x = pitch_name, y = prop_total)) +
    geom_col(aes(fill = "All Pitches")) +
    geom_col(aes(y = prop_after_cu, fill = "After Curveball"), width = .5, alpha = .8) +
    geom_text(aes(y = prop_after_cu, label = round(prop_after_cu, 2)), vjust = -.3) +
    labs(
      title = "How does Zack Greinke's pitch distribution change after throwing a curveball?",
      subtitle = "2019 season, data via Baseball Savant",
      x = "Pitch",
      y = "Proportion",
      caption = "Visualization by Nate Rowan, @nate_rowan17"
    ) +
    scale_y_continuous(limits = c(0, .45)) +
    scale_fill_manual(
      name = "Pitch Distribution",
      values = c("All Pitches" = astros_orange, "After Curveball" = astros_blue)
    ) +
    theme(
      legend.justification = c(1,1),
      legend.position = c(1,1)
    )


greinke_after_slider_counts <- greinke %>%
  filter(prev_pitch_name == "Slider", new_ab == FALSE) %>%
  group_by(prev_pitch_name, pitch_name) %>%
  summarise(n = n()) %>%
  mutate(
    prop = n / sum(n)
  ) %>%
  filter(n > 1) %>%
  arrange(desc(n))

temp_name <- greinke_non_first_pitch_counts %>%
  left_join(greinke_after_slider_counts, by = "pitch_name") %>%
  select(-prev_pitch_name) %>%
  rename(
    n_total = n.x,
    prop_total = prop.x,
    n_after_sl = n.y,
    prop_after_sl = prop.y
  )

temp_name %>%
  ggplot(aes(x = pitch_name, y = prop_total)) +
    geom_col(aes(fill = "All Pitches")) +
    geom_col(aes(y = prop_after_sl, fill = "After Slider"), width = .5, alpha = .8) +
    geom_text(aes(y = prop_after_sl, label = round(prop_after_sl, 2)), vjust = -.3) +
    labs(
      title = "How does Zack Greinke's pitch distribution change after throwing a slider?",
      subtitle = "2019 season, data via Baseball Savant",
      x = "Pitch",
      y = "Proportion",
      caption = "Visualization by Nate Rowan, @nate_rowan17"
    ) +
    scale_y_continuous(limits = c(0, .45)) +
    scale_fill_manual(
      name = "Pitch Distribution",
      values = c("All Pitches" = astros_orange, "After Slider" = astros_blue)
    ) +
    theme(
      legend.justification = c(1,1),
      legend.position = c(1,1)
    )

