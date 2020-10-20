##################


library(tidyverse)


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
      prev_pitch_type1 = lead(data$pitch_type),
      prev_pitch_name1 = lead(data$pitch_name),
      prev_pitch_type2 = lead(data$pitch_type, 2),
      prev_pitch_name2 = lead(data$pitch_name, 2),
      new_ab = case_when(
        pitch_number == 1 ~ TRUE,
        T ~ FALSE
      ),
      second_pitch_ab = case_when(
        pitch_number <= 2 ~ TRUE,
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

all_pitches <- all_pitches %>%
  arrange_by_pitcher()

all_pitches <- all_pitches %>%
  add_prev_pitch()




greinke <- all_pitches %>%
  filter(player_name == "Zack Greinke")

greinke <- greinke %>%
  add_prev_pitch()

### Most common three pitch sequences for Greinke
greinke %>%
  filter(new_ab == FALSE, second_pitch_ab == FALSE) %>%
  group_by(prev_pitch_type2, prev_pitch_type1, pitch_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(prop = n / sum(n)) %>% 
  arrange(desc(n))

### Greinke pitch distribution on 0-2, pitch 3,  after going fastabll-fastball
greinke %>%
  filter(pitch_number == 3, balls == 0, strikes == 2, prev_pitch_type2 == "FF", prev_pitch_type1 == "FF") %>%
  group_by(prev_pitch_type2, prev_pitch_type1, pitch_type) %>%
  summarise(n = n(), .groups = "drop") %>% 
  mutate(prop = n / sum(n)) %>% 
  arrange(desc(n)) 

### Greinke pitch distribution on 0-2, pitch 3, not counting fastball-fastball prior
greinke %>%
  filter(pitch_number == 3, balls == 0, strikes == 2, (prev_pitch_type2 != "FF" | prev_pitch_type1 != "FF")) %>% 
  group_by(pitch_type) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  mutate(prop = n / sum(n)) %>% 
  arrange(desc(n))




### Greinke pitch distribution on 0-2, after going fastabll-fastball
greinke %>%
  filter(balls == 0, strikes == 2, prev_pitch_type2 == "FF", prev_pitch_type1 == "FF") %>%
  group_by(prev_pitch_type2, prev_pitch_type1, pitch_type) %>%
  summarise(n = n(), .groups = "drop") %>% 
  mutate(prop = n / sum(n)) %>% 
  arrange(desc(n)) 

### Greinke pitch distribution on 0-2, not counting fastball-fastball prior
greinke %>%
  filter(balls == 0, strikes == 2, (prev_pitch_type2 != "FF" | prev_pitch_type1 != "FF")) %>% 
  group_by(pitch_type) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  mutate(prop = n / sum(n)) %>% 
  arrange(desc(n))



### Most common three pitch sequences across baseball
all_pitches %>%
  filter(new_ab == FALSE, second_pitch_ab == FALSE) %>%
  group_by(prev_pitch_type2, prev_pitch_type1, pitch_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(prop = n / sum(n)) %>% 
  arrange(desc(n))





###### Does previous pitch affect wOBA? #############

greinke %>%
  filter(prev_pitch_type1 != "FF", pitch_number >= 2) %>% 
  group_by(pitch_type) %>% 
  summarise(
    n = n(),
    woba = mean(woba_value, na.rm = TRUE)
  ) %>% 
  arrange(desc(n))

greinke %>%
  filter(prev_pitch_type1 == "FF", pitch_number >=2) %>% 
  group_by(pitch_type) %>% 
  summarise(
    n = n(),
    woba = mean(woba_value, na.rm = TRUE)
  ) %>% 
  arrange(desc(n))




post_non_fastball_woba <- all_pitches %>%
  filter(prev_pitch_type1 != "FF", pitch_number == 2, strikes == 1) %>% 
  group_by(pitch_type) %>% 
  summarise(
    n = n(),
    woba = mean(woba_value, na.rm = TRUE)
  ) %>% 
  arrange(desc(n)) 


post_fastball_woba <- all_pitches %>%
  filter(prev_pitch_type1 == "FF", pitch_number == 2, strikes == 1) %>% 
  group_by(pitch_type) %>% 
  summarise(
    n = n(),
    woba = mean(woba_value, na.rm = TRUE)
  ) %>% 
  arrange(desc(n)) %>%
  rename(
    n_post_ff = n,
    woba_post_ff = woba
  )

astros_orange <- "#eb6e1f"
astros_blue <- "#002d62"


### Observations: it looks like curveballs and sinkers are more effective after a fastball

left_join(post_non_fastball_woba, post_fastball_woba) %>% 
  filter(n > 100) %>% 
  ggplot(aes(x = pitch_type, y = woba)) +
    geom_col(aes(fill = "Not After Fastball")) +
    geom_col(aes(y = woba_post_ff, fill = "After Fastball"), width = .5, alpha = .8) +
    labs(
      title = "Does wOBA change if the previous pitch was a fastball?",
      subtitle = "wOBA on 0-1 count pitches, 2019 season, data via Baseball Savant",
      x = "Pitch",
      y = "wOBA",
      caption = "Visualization by Nate Rowan, @nate_rowan17"
    ) +
    scale_fill_manual(
      name = "Pitch Distribution",
      values = c("Not After Fastball" = astros_orange, "After Fastball" = astros_blue)
    )











post_non_fastball_woba <- all_pitches %>%
  filter(prev_pitch_type1 != "FF", strikes == 2, balls == 2) %>% 
  group_by(pitch_type) %>% 
  summarise(
    n = n(),
    woba = mean(woba_value, na.rm = TRUE)
  ) %>% 
  arrange(desc(n)) 


post_fastball_woba <- all_pitches %>%
  filter(prev_pitch_type1 == "FF", strikes == 2, balls == 2) %>% 
  group_by(pitch_type) %>% 
  summarise(
    n = n(),
    woba = mean(woba_value, na.rm = TRUE)
  ) %>% 
  arrange(desc(n)) %>%
  rename(
    n_post_ff = n,
    woba_post_ff = woba
  )

astros_orange <- "#eb6e1f"
astros_blue <- "#002d62"


### Observations: it looks like curveballs and sinkers are more effective after a fastball

left_join(post_non_fastball_woba, post_fastball_woba) %>% 
  filter(n > 100) %>% 
  ggplot(aes(x = pitch_type, y = woba)) +
  geom_col(aes(fill = "Not After Fastball")) +
  geom_col(aes(y = woba_post_ff, fill = "After Fastball"), width = .5, alpha = .8) +
  labs(
    title = "Does wOBA change if the previous pitch was a fastball?",
    subtitle = "wOBA on 0-1 count pitches, 2019 season, data via Baseball Savant",
    x = "Pitch",
    y = "wOBA",
    caption = "Visualization by Nate Rowan, @nate_rowan17"
  ) +
  scale_fill_manual(
    name = "Pitch Distribution",
    values = c("Not After Fastball" = astros_orange, "After Fastball" = astros_blue)
  )







