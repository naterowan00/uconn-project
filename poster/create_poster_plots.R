############################
### Created by: Nate Rowan
### Date Created: 10/19/2020
### Date Last Modified: 10/19/2020
### Description: This script contains all the required code
### to produce the plots for the slides in the UCSAS 2020 
### poster.
############################

library(tidyverse); theme_set(theme_minimal())
library(gt)


# Repeat Pitch Frequency Table -------------------------------


### table that shows frequency a pitcher repeats the previous pitch, 
### based on previous pitch result. excludes first pitch of at-bats. 
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


# Repeat Pitch Frequency by Count Bar Chart ---------------------


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


# Repeat Pitch Frequency by Previous Pitch Type Bar Chart ---------------


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


# Repeat Pitch Frequencies on Curveballs Bar Chart --------------------


### Repeat pitch frequencies by count for just curveballs
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

### Only include 0-1, 0-2, 1-1, 1-2 counts
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

### Save the curveballs plot
ggsave("pictures/repeat_freq_after_curveball.png")


# Individual Pitcher Analysis -------------------------------------


### Table that ranks pitchers by difference between swinging strike repeat freq
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

### Calculate difference between repeat pitch frequency after swinging strikes
### and other pitch results
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

### Scatterplot showing each pitcher's repeat pitch frequency after swinging
### strikes and after all other results
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

### Save scatterplot
ggsave(here::here("pictures/repeat_pitch_dist.png"))


### Calculate what proportion of pitchers see certain differences in 
### repeat pitch frequencies
swstr_vs_other_repeat_freq_data %>%
  filter(swstr_diff >= .2)

swstr_vs_other_repeat_freq_data %>% 
  filter(swstr_diff > 0)


# Madison Bumgarner Plot -------------------------------------------

### Calculate Bumgarner repeat pitch frequencies by count and prev pitch type
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

### only include early counts; one thing to note is that later counts
### have very small sample sizes
bumgarner_repeat_freq_filtered <- bumgarner_repeat_freq %>% 
  filter(count == "0-2" | count == "1-2" | count == "1-1" | count == "0-1")

### Madison Bumgarner repeat pitch frequency plot
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

### Save Bumgarner plot
ggsave(here::here("pictures/madison_bumgarner_plot.png"))


# Max Scherzer Plot --------------------------------------


### Calculate Scherzer repeat pitch frequencies by count and pitch type
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

### Include only early counts in plot
scherzer_repeat_freq_filtered <- scherzer_repeat_freq %>% 
  filter(count == "0-2" | count == "1-2" | count == "1-1" | count == "0-1")

### Create Scherzer pitch frequency plot
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

### Save Scherzer plot
ggsave(here::here("pictures/max_scherzer_plot.png"))
