####################
### If a pitcher gets a swinging strike, is he more likely to throw the same
### pitch again?
####################


library(tidyverse); theme_set(theme_minimal())
library(gt)
library(ggrepel)

add_prev_swstr <- function(data) {
  newdata <- data %>%
    mutate(
      prev_pitch_swstr = lead(data$swstr)
    )
  newdata
}

all_pitches <- all_pitches %>%
  add_prev_swstr()

### Try comparing to after a called strike or foul ball
  
add_prev_description <- function(data) {
  newdata <- data %>% 
    mutate(
      prev_pitch_description = lead(data$description)
    )
}  

all_pitches <- all_pitches %>%
  add_prev_description()
  


### Group prev pitch result into: swinging strike, called strike, foul, foul tip, ball
all_pitches <- all_pitches %>%
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


all_pitches <- all_pitches %>%
  add_same_pitch_variable()

### Add a column with the count as a character
all_pitches <- all_pitches %>%
  unite(
    "count", balls, strikes, sep = "-", remove = FALSE
  )

### table that shows frequency a pitcher repeats the previous pitch, based on previous pitch result
### excludes first pitch of at-bat. Other category includes pitchouts, bunts
### Normally, I'll want to filter out the other category
all_pitches %>%
  filter(pitch_number != 1) %>% 
  group_by(prev_pitch_description_group) %>% 
  summarise(
    n = n(),
    repeat_freq = mean(same_pitch)
  ) %>% 
  filter(prev_pitch_description_group != "foul_tip", prev_pitch_description_group != "other") %>% 
  mutate(
    prev_pitch_description_group = str_replace(prev_pitch_description_group, "_", " "),
    prev_pitch_description_group = str_to_title(prev_pitch_description_group)
  ) %>% 
  gt() %>% 
    tab_header(
      title = "Pitch Repitition by Previous Pitch Result",
      subtitle = ""
    ) %>% 
    tab_source_note(
      "Data: Baseball Savant"
    ) %>% 
    tab_style(
      list(
        cell_borders(
          sides = "left",
          color = "black",
          weight = px(2)
        )
      ),
      locations = list(
        cells_body(
          columns = vars(repeat_freq)
        )
      )
    ) %>% 
    data_color(
      columns = vars(repeat_freq),
      colors = scales::col_numeric(
        palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
        domain = NULL
      )
    ) %>% 
    cols_label(
      prev_pitch_description_group = "Previous Result",
      n = "Sample Size",
      repeat_freq = "Repeat Pitch Frequency"
    ) %>% 
    fmt_percent(
      columns = vars(repeat_freq),
      decimals = 1
    ) %>% 
    cols_align(
      columns = vars(n),
      align = "right"
    ) %>% 
    gtsave(
      filename = here::here("pictures/repeat_freq_table.png")
    )

####### Controlling for count

### Group pitches by current count and calculate repeat pitch frequency
repeat_freq_by_count <- all_pitches %>%
  filter(pitch_number != 1) %>% 
  group_by(prev_pitch_description_group, count) %>% 
  summarise(
    n = n(),
    repeat_freq = mean(same_pitch)
  ) %>% 
  filter(
    prev_pitch_description_group != "other", 
    prev_pitch_description_group != "foul_tip", 
    n > 5, 
    !str_detect(count, "-0")
  )

### Plot repeat pitch frequencies grouped by current count
repeat_freq_by_count %>% 
  ggplot(aes(x = prev_pitch_description_group, y = repeat_freq)) +
    geom_col(
      aes(fill = prev_pitch_description_group), 
      position = position_dodge()
    ) +
    geom_hline(
      data = filter(repeat_freq_by_count, prev_pitch_description_group == "swinging_strike"),
      aes(yintercept = repeat_freq),
      linetype = "dashed"
    ) +
    facet_wrap(~count, nrow = 2) +
    labs(
      title = "Frequency that a pitcher repeated the previous pitch",
      subtitle = "Grouped by current count, colored by previous pitch result",
      caption = "Data via Baseball Savant",
      y = "Repeat Pitch Proportion",
      fill = "Previous Pitch Result"
    ) +
    scale_y_continuous(
      labels = scales::percent
    ) +
    scale_fill_brewer(
      palette = "Set1",
      labels = c("Ball", "Called Strike", "Foul", "Swinging Strike")
    ) +
    theme(
      legend.position = "top",
      title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      strip.text = element_text(size = 12),
      strip.background = element_rect(fill = "pink", linetype = "blank")
    )

### Save control for count plot
ggsave(here::here("pictures/repeat_pitch_by_count_facet.png"))


### Alternate bar chart controlling for count, not facetted
all_pitches %>%
  filter(pitch_number != 1) %>% 
  group_by(prev_pitch_description_group, count) %>% 
  summarise(
    n = n(),
    repeat_freq = mean(same_pitch)
  ) %>% 
  filter(
    prev_pitch_description_group != "other", 
    prev_pitch_description_group != "foul_tip", 
    n > 5, 
    !str_detect(count, "-0")
  ) %>% 
  ggplot(aes(x = count, y = repeat_freq)) +
    geom_col(
      aes(group = prev_pitch_description_group, fill = prev_pitch_description_group), 
      position = position_dodge()
    ) +
    labs(
      title = "Frequency that a pitcher repeated the previous pitch",
      subtitle = "Grouped by current count, colored by previous pitch result",
      caption = "Data via Baseball Savant",
      x = "Count",
      y = "Repeat Pitch Proportion",
      fill = "Previous Pitch Result"
    ) +
    scale_y_continuous(
      labels = scales::percent
    ) +
    scale_fill_brewer(
      palette = "Set1",
      labels = c("Ball", "Called Strike", "Foul", "Swinging Strike")
    ) +
    theme(
      legend.position = "top",
      title = element_text(size = 16),
      axis.text = element_text(size = 14),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12)
    )

ggsave(here::here("pictures/repeat_pitch_by_count.png"))








###### Controlling for prev pitch

### Group pitches by prev pitch name and calculate repeat pitch frequency
### of each group
repeat_freq_by_prev_pitch_name <- all_pitches %>%
  filter(pitch_number != 1) %>% 
  group_by(prev_pitch_description_group, prev_pitch_name1) %>% 
  summarise(
    n = n(),
    repeat_freq = mean(same_pitch)
  ) %>% 
  filter(
    prev_pitch_description_group != "other", 
    prev_pitch_description_group != "foul_tip", 
    n > 100, 
    !is.na(prev_pitch_name1)
  )

### Plot the repeat pitch frequencies, grouped by prev pitch
repeat_freq_by_prev_pitch_name %>% 
  ggplot(aes(x = prev_pitch_description_group, y = repeat_freq)) +
  geom_col(
    aes(fill = prev_pitch_description_group), 
    position = position_dodge()
  ) +
  geom_hline(
    data = filter(repeat_freq_by_prev_pitch_name, prev_pitch_description_group == "swinging_strike"),
    aes(yintercept = repeat_freq),
    linetype = "dashed"
  ) +
  facet_wrap(~prev_pitch_name1, nrow = 2) +
  labs(
    title = "Frequency that a pitcher repeated the previous pitch",
    subtitle = "Grouped by previous pitch type, colored by previous pitch result",
    caption = "Data via Baseball Savant",
    x = "Count",
    y = "Repeat Pitch Proportion",
    fill = "Previous Pitch Result"
  ) +
  scale_y_continuous(
    labels = scales::percent
  ) +
  scale_fill_brewer(
    palette = "Set1",
    labels = c("Ball", "Called Strike", "Foul", "Swinging Strike")
  ) +
  theme(
    legend.position = "top",
    title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    strip.text = element_text(size = 12),
    strip.background = element_rect(fill = "pink", linetype = "blank")
  )

### Save control for prev pitch type plot
ggsave(here::here("pictures/repeat_pitch_by_prev_pitch_facet.png"))




### Looking at just curveballs, controlling for count
repeat_freq_curveballs <- all_pitches %>%
  filter(pitch_number != 1, prev_pitch_type1 == "CU") %>% 
  group_by(prev_pitch_description_group, count) %>% 
  summarise(
    n = n(),
    repeat_freq = mean(same_pitch)
  ) %>% 
  filter(
    prev_pitch_description_group != "other", 
    prev_pitch_description_group != "foul_tip", 
    n > 5, 
    !str_detect(count, "-0")
  )

repeat_freq_curveballs_filtered <- repeat_freq_curveballs %>% 
  filter(count == "0-1" | count == "0-2" | count == "1-1" | count == "1-2")

### Plot the repeat frequencies after curveballs
repeat_freq_curveballs_filtered %>% 
  ggplot(aes(x = prev_pitch_description_group, y = repeat_freq)) +
  geom_col(
    aes(fill = prev_pitch_description_group), 
    position = position_dodge()
  ) +
  geom_text(
    aes(label = glue::glue("{round(repeat_freq, 3) * 100}%")),
    nudge_y = .06
  ) +
  geom_hline(
    data = filter(repeat_freq_curveballs_filtered, prev_pitch_description_group == "swinging_strike"),
    aes(yintercept = repeat_freq),
    linetype = "dashed"
  ) +
  facet_wrap(~count, nrow = 2) +
  labs(
    title = "Repeat pitch frequencies after curveballs",
    subtitle = "Grouped by current count, colored by previous pitch result",
    caption = "Data via Baseball Savant",
    y = "Repeat Pitch Proportion",
    fill = "Previous Pitch Result"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_fill_brewer(
    palette = "Set1",
    labels = c("Ball", "Called Strike", "Foul", "Swinging Strike")
  ) +
  theme(
    legend.position = "top",
    title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    strip.text = element_text(size = 12),
    strip.background = element_rect(fill = "pink", linetype = "blank")
  )

ggsave("pictures/repeat_freq_after_curveball.png")





### Frequency pitcher repeats pitch when the last pitch was a slider
all_pitches %>%
  filter(pitch_number != 1, prev_pitch_type1 == "SL") %>% 
  group_by(prev_pitch_description_group, count) %>% 
  summarise(
    n = n(),
    repeat_freq = mean(same_pitch)
  ) %>% 
  filter(
    prev_pitch_description_group != "other", 
    prev_pitch_description_group != "foul_tip", 
    n > 5, 
    !str_detect(count, "-0")
  ) %>% 
  ggplot(aes(x = count, y = repeat_freq)) +
    geom_col(
      aes(group = prev_pitch_description_group, fill = prev_pitch_description_group), 
      position = position_dodge()
    ) +
    labs(
      title = "Proportion of time a pitcher repeated a SLIDER",
      subtitle = "Grouped by count and previous pitch result",
      x = "Count",
      y = "Repeat Pitch Proportion",
      fill = "Previous Pitch Result"
    ) +
    theme(
      legend.position = "top",
      title = element_text(size = 16),
      axis.text = element_text(size = 14)
    ) 

all_pitches %>%
  filter(pitch_number != 1, prev_pitch_type1 == "SL") %>% 
  group_by(prev_pitch_description_group, count) %>% 
  summarise(
    n = n(),
    repeat_freq = mean(same_pitch)
  ) %>% View()


##### Frequency pitcher repeats prev pitch when the last pitch was a fastball
all_pitches %>%
  filter(pitch_number != 1, prev_pitch_type1 == "FF") %>% 
  group_by(prev_pitch_description_group, count) %>% 
  summarise(
    n = n(),
    repeat_freq = mean(same_pitch)
  ) %>% 
  filter(
    prev_pitch_description_group != "other", 
    prev_pitch_description_group != "foul_tip", 
    n > 5, 
    !str_detect(count, "-0")
  ) %>% 
  ggplot(aes(x = count, y = repeat_freq)) +
    geom_col(
      aes(group = prev_pitch_description_group, fill = prev_pitch_description_group), 
      position = position_dodge()
    ) +
    labs(
      title = "Proportion of time a pitcher repeated a 4-SEAMER",
      subtitle = "Grouped by count and previous pitch result",
      x = "Count",
      y = "Repeat Pitch Proportion",
      fill = "Previous Pitch Result"
    ) +
    theme(
      legend.position = "top",
      title = element_text(size = 16),
      axis.text = element_text(size = 14)
    ) 

all_pitches %>%
  filter(pitch_number != 1, prev_pitch_type1 == "FF") %>% 
  group_by(prev_pitch_description_group, count) %>% 
  summarise(
    n = n(),
    repeat_freq = mean(same_pitch)
  ) %>% 
  filter(
    prev_pitch_description_group != "other", 
    prev_pitch_description_group != "foul_tip", 
    n > 5
  ) %>% View()


### Table that ranks pitchers by difference b/w swinging strike repeat freq
### and called strike repeat freq, requires 100 of each event
repeat_freq_by_pitcher_data <- all_pitches %>%
  filter(pitch_number != 1) %>% 
  group_by(prev_pitch_description_group, player_name) %>% 
  summarise(
    n = n(),
    repeat_freq = mean(same_pitch)
  ) %>% 
  pivot_wider(names_from = prev_pitch_description_group, values_from = n:repeat_freq) %>% 
  mutate(
    swstr_diff = repeat_freq_swinging_strike - repeat_freq_called_strike
  ) %>% 
  filter(n_called_strike > 100, n_swinging_strike > 100) %>% 
  arrange(desc(swstr_diff)) %>% 
  select(
    player_name,
    n_called_strike, 
    n_swinging_strike, 
    repeat_freq_called_strike, 
    repeat_freq_swinging_strike,
    swstr_diff
  )

### Plot of the above swinging strike - called strike data
repeat_freq_by_pitcher_data %>% 
  ggplot(aes(x = swstr_diff, y = 0)) +
    geom_boxplot(outlier.shape = NA, fill = "firebrick2", color = "black") +
    geom_jitter(size = 0.9, alpha = 0.8) +
    geom_text() +
    scale_y_continuous(limits = c(-1,1))


### Difference in repeat pitch frequency after swinging strike and after non-swinging strike
swstr_vs_other_repeat_freq_data <- all_pitches %>%
  filter(
    pitch_number != 1,
    prev_pitch_description_group != "other", 
    prev_pitch_description_group != "foul_tip"
  ) %>% 
  group_by(prev_pitch_swstr, player_name) %>% 
  summarise(
    n = n(),
    repeat_freq = mean(same_pitch)
  ) %>% 
  pivot_wider(names_from = prev_pitch_swstr, values_from = n:repeat_freq) %>% 
  mutate(
    swstr_diff = repeat_freq_1 - repeat_freq_0,
    pct_swstr_diff = swstr_diff / repeat_freq_0
  ) %>% 
  filter(n_0 > 100, n_1 > 100) %>% 
  rename(
    repeat_freq_swstr = repeat_freq_1,
    repeat_freq_other = repeat_freq_0
  ) %>% 
  arrange(desc(swstr_diff))

whiff_effect_five_num_summary <- swstr_vs_other_repeat_freq_data %>% 
  summarise(
    five = list(fivenum(swstr_diff))
  ) %>% 
  unnest(cols = c(five)) %>% 
  round(3)



### Plot that shows the "whiff effect" from the above table
swstr_vs_other_repeat_freq_data %>% 
  ggplot(aes(x = swstr_diff, y = 0)) +
    geom_boxplot(outlier.shape = NA, fill = "pink", color = "black") +
    geom_text(
      data = whiff_effect_five_num_summary,
      aes(x = five, y = 0, label = glue::glue("{five * 100}%")),
      nudge_y = 0.49,
      size = 5
    ) +
    geom_jitter(
      size = if_else(
        swstr_vs_other_repeat_freq_data$player_name == "Madison Bumgarner" |
          swstr_vs_other_repeat_freq_data$player_name == "Max Scherzer" |
          swstr_vs_other_repeat_freq_data$player_name == "Matthew Boyd",
        4,
        2
      ),
      alpha = if_else(
        swstr_vs_other_repeat_freq_data$player_name == "Madison Bumgarner" |
          swstr_vs_other_repeat_freq_data$player_name == "Max Scherzer" |
          swstr_vs_other_repeat_freq_data$player_name == "Matthew Boyd",
        1,
        0.7
      ),
      color = if_else(
        swstr_vs_other_repeat_freq_data$player_name == "Madison Bumgarner" |
          swstr_vs_other_repeat_freq_data$player_name == "Max Scherzer" |
          swstr_vs_other_repeat_freq_data$player_name == "Matthew Boyd",
        "red",
        "black"
        )
    ) +
    scale_x_continuous(
      labels = scales::percent
    ) +
    scale_y_continuous(limits = c(-.5,.55)) +
    labs(
      title = "\"Whiff Effect\" For Pitchers With 100+ Called Strikes",
      subtitle = "Whiff Effect = repeat pitch frequency after whiff - repeat pitch frequency after non-whiff",
      x = "Whiff Effect",
      caption = "Data via Baseball Savant"
    ) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      title = element_text(size = 12),
      axis.text.x = element_text(size = 14),
      axis.title.x = element_text(size = 14)
    )

ggsave(here::here("pictures/whiff_effect_boxplot_plot.png"))


swstr_vs_other_repeat_freq_data %>% 
  ggplot(aes(x = repeat_freq_other, y = repeat_freq_swstr)) +
    geom_point(
      color = if_else(
        swstr_vs_other_repeat_freq_data$player_name == "Max Scherzer" |
          swstr_vs_other_repeat_freq_data$player_name == "Madison Bumgarner",
        "red",
        "black"
      ),
      size = if_else(
        swstr_vs_other_repeat_freq_data$player_name == "Max Scherzer" |
          swstr_vs_other_repeat_freq_data$player_name == "Madison Bumgarner",
        4,
        2
      )
    ) +
    geom_text(
      aes(label = glue::glue("{round(repeat_freq, 3) * 100}%")),
      nudge_y = 0.1
    ) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    annotate(
      geom = "text", x = .80, y = .75, label = "y = x", size = 5
    ) +
    scale_x_continuous(
      limits = c(0.15, 0.9),
      labels = scales::percent
    ) +
    scale_y_continuous(
      limits = c(0.15, 0.9),
      labels = scales::percent
    ) +
    labs(
      title = "Repeat pitch frequencies for pitchers with 100+ swinging strikes",
      subtitle = "The further a pitcher is above the dotted line, the greater their \"whiff effect\" is",
      caption = "Data via Baseball Savant",
      x = "Repeat Pitch Frequency After All Other Results",
      y = "Repeat Pitch Frequency After Swinging Strikes"
    ) +
    theme(
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12)
    )

ggsave(here::here("pictures/repeat_pitch_dist.png"))
    
    
swstr_vs_other_repeat_freq_data %>%
  filter(swstr_diff >= .2)

swstr_vs_other_repeat_freq_data %>% 
  filter(swstr_diff > 0)


### Madison Bumgarner plot and table, who is one of the more extreme patterns
### He really shows the pattern in early counts
bumgarner_repeat_freq <- all_pitches %>%
  filter(
    pitch_number != 1, 
    player_name == "Madison Bumgarner",
  ) %>% 
  group_by(prev_pitch_description_group, count) %>% 
  summarise(
    n = n(),
    repeat_freq = mean(same_pitch)
  ) %>% 
  filter(
    prev_pitch_description_group != "other", 
    prev_pitch_description_group != "foul_tip", 
    !str_detect(count, "-0")
  ) 

bumgarner_repeat_freq_filtered <- bumgarner_repeat_freq %>% 
  filter(count == "0-2" | count == "1-2" | count == "1-1" | count == "0-1")

bumgarner_repeat_freq_filtered %>%
  ggplot(aes(x = prev_pitch_description_group, y = repeat_freq)) +
    geom_col(
      aes(fill = prev_pitch_description_group), 
      position = position_dodge()
    ) +
    geom_text(
      aes(label = glue::glue("{round(repeat_freq, 3) * 100}%")),
      nudge_y = 0.1
    ) +
    geom_hline(
      data = filter(bumgarner_repeat_freq_filtered, prev_pitch_description_group == "swinging_strike"),
      aes(yintercept = repeat_freq),
      linetype = "dashed"
    ) +
    facet_wrap(~count, nrow = 2) +
    labs(
      title = "Madison Bumgarner's whiff effect is largest in early counts",
      subtitle = "Grouped by current count, colored by previous pitch result",
      caption = "Data via Baseball Savant",
      y = "Repeat Pitch Proportion",
      fill = "Previous Pitch Result"
    ) +
    scale_y_continuous(
      labels = scales::percent
    ) +
    scale_fill_brewer(
      palette = "Set1",
      labels = c("Ball", "Called Strike", "Foul", "Swinging Strike")
    ) +
    theme(
      legend.position = "top",
      title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      strip.text = element_text(size = 12),
      strip.background = element_rect(fill = "pink", linetype = "blank")
    )

ggsave(here::here("pictures/madison_bumgarner_plot.png"))



bumgarner_repeat_freq %>% 
  View()


### Matthew Boyd plot and table, who is a more average pattern guy
### He really exhibits the pattern in two strike counts
boyd_repeat_freq <- all_pitches %>%
  filter(pitch_number != 1, player_name == "Matthew Boyd") %>% 
  group_by(prev_pitch_description_group, count) %>% 
  summarise(
    n = n(),
    repeat_freq = mean(same_pitch)
  ) %>% 
  filter(
    prev_pitch_description_group != "other", 
    prev_pitch_description_group != "foul_tip", 
    !str_detect(count, "-0")
  ) 

boyd_repeat_freq_filtered <- boyd_repeat_freq %>% 
  filter(
    count == "0-2" | count == "1-2" | count == "1-1" | count == "0-1"
  )

boyd_repeat_freq_filtered %>% 
  ggplot(aes(x = prev_pitch_description_group, y = repeat_freq)) +
    geom_col(
      aes(fill = prev_pitch_description_group), 
      position = position_dodge()
    ) +
    geom_hline(
      data = filter(boyd_repeat_freq_filtered, prev_pitch_description_group == "swinging_strike"),
      aes(yintercept = repeat_freq),
      linetype = "dashed"
    ) +
    facet_wrap(~count, ncol = 2) +
    labs(
      title = "For Matthew Boyd, the whiff effect appears in 2-strike counts",
      subtitle = "Grouped by current count, colored by previous pitch result",
      y = "Repeat Pitch Proportion",
      fill = "Previous Pitch Result"
    ) +
    scale_fill_brewer(
      palette = "Set1",
      labels = c("Ball", "Called Strike", "Foul", "Swinging Strike")
    ) +
    theme(
      legend.position = "top",
      title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      strip.text = element_text(size = 14)
    )

ggsave(here::here("pictures/matthew_boyd_plot.png"))




### Kyle Hendricks plot and table with count on x-axis, who completely bucks our trend
hendricks_repeat_freq <- all_pitches %>%
  filter(pitch_number != 1, player_name == "Kyle Hendricks") %>% 
  group_by(prev_pitch_description_group, count) %>% 
  summarise(
    n = n(),
    repeat_freq = mean(same_pitch)
  ) %>% 
  filter(
    prev_pitch_description_group != "other", 
    prev_pitch_description_group != "foul_tip", 
    !str_detect(count, "-0")
  ) 

hendricks_repeat_freq_filtered <- hendricks_repeat_freq %>% 
  filter(count == "0-2" | count == "1-2" | count == "1-1" | count == "0-1")

hendricks_repeat_freq_filtered %>%
  ggplot(aes(x = prev_pitch_description_group, y = repeat_freq)) +
    geom_col(
      aes(fill = prev_pitch_description_group), 
      position = position_dodge()
    ) +
    geom_hline(
      data = filter(hendricks_repeat_freq_filtered, prev_pitch_description_group == "swinging_strike"),
      aes(yintercept = repeat_freq),
      linetype = "dashed"
    ) +
    facet_wrap(~count, nrow = 2) +
    labs(
      title = "Kyle Hendricks does not exhibit the whiff effect",
      subtitle = "Grouped by current count, colored by previous pitch result",
      y = "Repeat Pitch Proportion",
      fill = "Previous Pitch Result"
    ) +
    scale_fill_brewer(
      palette = "Set1",
      labels = c("Ball", "Called Strike", "Foul", "Swinging Strike")
    ) +
    theme(
      legend.position = "top",
      title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      strip.text = element_text(size = 14)
    )

ggsave(here::here("pictures/kyle_hendricks_plot.png"))


### Scherzer stuff
scherzer_repeat_freq <- all_pitches %>%
  filter(pitch_number != 1, player_name == "Max Scherzer") %>% 
  group_by(prev_pitch_description_group, count) %>% 
  summarise(
    n = n(),
    repeat_freq = mean(same_pitch)
  ) %>% 
  filter(
    prev_pitch_description_group != "other", 
    prev_pitch_description_group != "foul_tip", 
    !str_detect(count, "-0")
  ) 

scherzer_repeat_freq_filtered <- scherzer_repeat_freq %>% 
  filter(count == "0-2" | count == "1-2" | count == "1-1" | count == "0-1")

scherzer_repeat_freq_filtered %>%
  ggplot(aes(x = prev_pitch_description_group, y = repeat_freq)) +
    geom_col(
      aes(fill = prev_pitch_description_group), 
      position = position_dodge()
    ) +
    geom_text(
      aes(label = glue::glue("{round(repeat_freq, 3) * 100}%")),
      nudge_y = 0.045
    ) +
    geom_hline(
      data = filter(scherzer_repeat_freq_filtered, prev_pitch_description_group == "swinging_strike"),
      aes(yintercept = repeat_freq),
      linetype = "dashed"
    ) +
    facet_wrap(~count, nrow = 2) +
    labs(
      title = "Max Scherzer does not exhibit the whiff effect...",
      subtitle = "...but his foul ball trends might be worth exploring",
      caption = "Data via Baseball Savant",
      y = "Repeat Pitch Proportion",
      fill = "Previous Pitch Result"
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1)
    ) +
    scale_fill_brewer(
      palette = "Set1",
      labels = c("Ball", "Called Strike", "Foul", "Swinging Strike")
    ) +
    theme(
      legend.position = "top",
      title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      strip.text = element_text(size = 12),
      strip.background = element_rect(fill = "pink", linetype = "blank")
    )

ggsave(here::here("pictures/max_scherzer_plot.png"))











 

######## Correlate the repeat pitch stuff with FIP, ERA, SWStr

### Read in pitcher leaderboard from Fangraphs
fangraphs_data <- read_csv(here::here("datasets/2019_fangraphs_leaderboard.csv"))

fangraphs_data <- fangraphs_data %>%
  janitor::clean_names() %>% 
  rename(
    xfip = x_fip
  )

### Create tibble with the repeat frequencies of each pitcher
repeat_freq_by_pitcher_data <- all_pitches %>%
  filter(pitch_number != 1) %>% 
  group_by(prev_pitch_description_group, player_name) %>% 
  summarise(
    n = n(),
    repeat_freq = mean(same_pitch)
  ) %>% 
  pivot_wider(names_from = prev_pitch_description_group, values_from = n:repeat_freq) %>% 
  mutate(
    swstr_diff = repeat_freq_swinging_strike - repeat_freq_called_strike
  ) %>% 
  filter(n_called_strike > 100, n_swinging_strike > 100) %>% 
  arrange(desc(swstr_diff)) %>% 
  select(
    player_name,
    n_called_strike, 
    n_swinging_strike, 
    repeat_freq_called_strike, 
    repeat_freq_swinging_strike,
    swstr_diff
  )

combined_data <- left_join(repeat_freq_by_pitcher_data, fangraphs_data, by = c("player_name" = "name"))

### I guess there's a relationship between repeat after swstr and k per 9
combined_data %>%
  ggplot(aes(x = repeat_freq_swinging_strike, y = k_9)) +
    geom_point() +
    geom_smooth(method = "lm")

repeat_freq_k_9_fit <- lm(k_9 ~ repeat_freq_swinging_strike, data = combined_data)

repeat_freq_k_9_fit %>% 
  summary()

### What about the difference between swstr and called strike?
combined_data %>%
  ggplot(aes(x = swstr_diff, y = k_9)) +
    geom_point() +
    geom_smooth(method = "lm")

### No relationship with era and fip
repeat_freq_era_fit <- lm(era ~ repeat_freq_swinging_strike, data = combined_data)

repeat_freq_era_fit %>% 
  summary()

repeat_freq_fip_fit <- lm(fip ~ repeat_freq_swinging_strike, data = combined_data)

repeat_freq_fip_fit %>% 
  summary()
















all_pitches %>%
  filter(pitch_number != 1, player_name == "Robbie Ray") %>% 
  group_by(prev_pitch_type1, prev_pitch_description_group) %>% 
  summarise(
    n = n(),
    repeat_freq = mean(same_pitch)
  ) %>% 
  filter(prev_pitch_description_group != "other", prev_pitch_description_group != "foul_tip", n > 5) 

all_pitches %>%
  filter(pitch_number != 1, player_name == "Gerrit Cole") %>% 
  group_by(prev_pitch_type1, strikes, prev_pitch_description_group) %>% 
  summarise(
    n = n(),
    repeat_freq = mean(same_pitch)
  ) %>% 
  filter(prev_pitch_description_group != "other", prev_pitch_description_group != "foul_tip", n > 5) %>% 
  View()

all_pitches %>%
  filter(pitch_number != 1, player_name == "Robbie Ray") %>% 
  group_by(pitch_type, balls, strikes) %>% 
  summarise(
    n = n(),
    repeat_freq = mean(same_pitch)
  ) %>% 
  filter(n > 5) %>% 
  View()


all_pitches %>%
  filter(pitch_number != 1) %>% 
  group_by(player_name, prev_pitch_description_group) %>% 
  summarise(
    n = n(),
    repeat_freq = mean(same_pitch)
  ) %>% 
  filter(prev_pitch_description_group != "other", prev_pitch_description_group != "foul_tip", n > 250) %>% 
  arrange(desc(repeat_freq)) %>% 
  View()

all_pitches %>%
  filter(pitch_number != 1) %>% 
  group_by(player_name) %>% 
  summarise(
    n = n(),
    repeat_freq = mean(same_pitch)
  ) %>% 
  filter(n > 1500) %>% 
  arrange(desc(repeat_freq)) %>% 
  View()

