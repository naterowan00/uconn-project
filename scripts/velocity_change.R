####################
### Does speed difference between pitches contribute to success?
####################


library(tidyverse); theme_set(theme_minimal())
library(tidymodels)
library(RColorBrewer)


add_prev_velocity <- function(data) {
  newdata <- data %>%
    mutate(
      prev_pitch_velo = lead(data$release_speed)
      )
  newdata
}

all_pitches <- add_prev_velocity(all_pitches) 


all_pitches <- all_pitches %>%
  mutate(
    velo_diff = release_speed - prev_pitch_velo,
    swstr = case_when(
      description == "swinging_strike" | description == "swinging_strike_blocked" ~ 1, 
      description != "swinging_strike" ~ 0
    )
  )



fastballs <- all_pitches %>%
  filter(pitch_number != 1, pitch_type == "FF", release_speed > 85, !is.na(release_speed), !is.na(prev_pitch_velo))

fastballs_in_play <- fastballs %>% 
  filter(!is.na(woba_value))

changeups <- all_pitches %>%
  filter(pitch_number != 1, pitch_type == "CH", release_speed > 60, !is.na(release_speed), !is.na(prev_pitch_velo))





group_pitches_by_velo <- function(data, lower, upper, ngrid){
  newdata <- data %>%
    mutate(
      pitch_velo_group = cut(release_speed, seq(lower, upper, length.out = ngrid)),
    )
  newdata
}

group_pitches_by_velo_diff <- function(data, lower, upper, ngrid) {
  newdata <- data %>% 
    mutate(
      velo_diff_group = cut(velo_diff, seq(lower, upper, length.out = ngrid))
    )
  newdata
}

group_pitches_by_vertical_movement <- function(data, lower, upper, ngrid){
  newdata <- data %>%
    mutate(
      vertical_movement_group = cut(pfx_z, seq(lower, upper, length.out = ngrid)),
    )
  newdata
}

calc_average_swstr <- function(data){
  newdata <- data %>% 
    group_by(pitch_velo_group, velo_diff_group) %>% 
    summarise(
      n = n(),
      mean_swing_str = mean(swstr)
    )
  newdata
}

remove_na_and_small_n <- function(data, min_pitches = 0){
  newdata <- data %>% 
    filter(!is.na(pitch_velo_group), !is.na(velo_diff_group), n >= min_pitches)
  newdata
}
  

fastballs %>%
  group_pitches_by_velo(85, 100, ngrid = 11) %>% 
  group_pitches_by_velo_diff(-5, 20, ngrid = 11) %>% 
  calc_average_swstr() %>% 
  remove_na_and_small_n(min_pitches = 100) %>% 
  ggplot(aes(x = pitch_velo_group, y = velo_diff_group, fill = mean_swing_str)) +
    geom_tile() +
    geom_text(aes(label = glue::glue("{round(mean_swing_str * 100, 1)}%"))) +
    scale_fill_distiller(palette = "Blues", direction = 1) +
    labs(
      title = "Swinging Strike % on Fastballs, Grouped By Velo and Velo Change",
      subtitle = "2019 Season, data via Baseball Savant",
      fill = "SwStr %",
      x = "Velocity",
      y = "Change in Velocity"
    )

ggsave("pictures/swing_strike_fastballs.png")


fastballs %>%
  group_pitches_by_velo(lower = 85, upper = 100, ngrid = 6) %>% 
  group_pitches_by_velo_diff(lower = -5, upper = 5, ngrid = 6) %>% 
  calc_average_swstr() %>% 
  remove_na_and_small_n(min_pitches = 300) %>% 
  ggplot(aes(x = pitch_velo_group, y = velo_diff_group, fill = mean_swing_str)) +
    geom_tile() +
    geom_text(aes(label = glue::glue("{round(mean_swing_str * 100, 1)}%"))) +
    scale_fill_distiller(palette = "Blues", direction = 1) +
    labs(
      title = "Swinging Strike % on Fastballs, Grouped By Velo and Velo Change",
      subtitle = "2019 Season, data via Baseball Savant",
      fill = "SwStr %",
      x = "Velocity",
      y = "Change in Velocity"
    ) +
    theme(
      axis.text = element_text(size = 14)
    )

ggsave("zoomed_in_swinging_strike_fastball_plot.png")
  


### Changeups
changeups %>%
  group_pitches_by_velo(lower = 77, upper = 95, ngrid = 10) %>% 
  group_pitches_by_velo_diff(lower = -15, upper = 10, ngrid = 11) %>% 
  calc_average_swstr() %>% 
  remove_na_and_small_n(min_pitches = 200) %>% 
  ggplot(aes(x = pitch_velo_group, y = velo_diff_group, fill = mean_swing_str)) +
    geom_tile() +
    geom_text(aes(label = glue::glue("{round(mean_swing_str * 100, 1)}%"))) +
    scale_fill_distiller(palette = "Blues", direction = 1) +
    labs(
      title = "Swinging Strike % on Changeups, Grouped By Velo and Velo Change",
      subtitle = "2019 Season, data via Baseball Savant",
      fill = "SwStr %",
      x = "Velocity",
      y = "Change in Velocity"
    ) 



changeups %>%
  group_pitches_by_velo(lower = 70, upper = 95, ngrid = 11) %>% 
  group_pitches_by_velo_diff(lower = -15, upper = 20, ngrid = 11) %>% 
  calc_average_swstr() %>% 
  remove_na_and_small_n(min_pitches = 200) %>% 
  ggplot(aes(x = pitch_velo_group, y = velo_diff_group, fill = mean_swing_str)) +
    geom_tile() +
    geom_text(aes(label = n)) +
    scale_fill_distiller(palette = "Blues", direction = 1) +
    labs(
      title = "Swinging Strike % on Changeups, Grouped By Velo and Velo Change",
      subtitle = "2019 Season, data via Baseball Savant",
      fill = "SwStr %",
      x = "Velocity",
      y = "Change in Velocity"
    ) 


changeups %>%
  group_pitches_by_vertical_movement(lower = -1, upper = 2, ngrid = 7) %>% 
  group_by(vertical_movement_group, prev_pitch_type1) %>% 
  summarise(
    n = n(),
    mean_swing_str = mean(swstr)
  ) %>% 
  filter(!is.na(vertical_movement_group), !is.na(prev_pitch_type1), n > 210) %>% 
  ggplot(aes(x = fct_rev(vertical_movement_group), y = prev_pitch_type1, fill = mean_swing_str)) +
    geom_tile() +
    geom_text(aes(label = n)) +
    scale_fill_distiller(palette = "Blues", direction = 1) +
    labs(
      title = "Swinging Strike % on Changeups, Grouped by Vert Move and Prev Pitch",
      subtitle = "2019 Season, data via Baseball Savant",
      fill = "SwStr %",
      x = "Vertical Movement",
      y = "Previous Pitch Type"
    ) 

changeups %>%
  group_pitches_by_vertical_movement(lower = -2, upper = 2, ngrid = 7) %>% 
  group_by(player_name, strikes, prev_pitch_type1) %>% 
  summarise(
    n = n(),
    mean_swing_str = mean(swstr)
  ) %>% 
  filter(!is.na(prev_pitch_type1), player_name == "Zac Gallen", n > 6) %>% 
  ggplot(aes(x = strikes, y = prev_pitch_type1, fill = mean_swing_str)) +
    geom_tile() +
    geom_text(aes(label = n)) +
    geom_text(aes(label = glue::glue("{round(mean_swing_str * 100, 1)}%")), vjust = 2.5) +
    scale_fill_distiller(palette = "Blues", direction = 1) +
    labs(
      title = "Swinging Strike % on Changeups, Grouped by Vert Move and Prev Pitch",
      subtitle = "2019 Season, data via Baseball Savant",
      fill = "SwStr %",
      x = "Strikes",
      y = "Previous Pitch Type"
    ) 




all_pitches %>%
  summarise(mean(swstr))


changeups %>%
  select(release_pos_z, plate_z, pfx_z)
  












fastballs %>% 
  ggplot(aes(x = release_speed, y = velo_diff, color = swing_str)) +
    geom_point(alpha = .1)



fastballs_in_play$id <- 1:nrow(fastballs_in_play)


train_data <- sample_frac(fastballs_in_play, 0.8)
test_data <- anti_join(fastballs_in_play, train_data, by = "id")

folds <- vfold_cv(fastballs_in_play, folds = 10)


knn_mod <- nearest_neighbor(mode = "regression", neighbors = 200) %>% 
  set_engine("kknn") %>% 
  fit(woba_value ~ release_speed + velo_diff, train_data)

mesh <- expand_grid(
  release_speed = seq(85, 100, length.out = 200),
  velo_diff = seq(-5, 20, length.out = 200)
  )


predictions <- knn_mod %>% 
  predict(mesh) %>% 
  bind_cols(mesh, .) 

predictions %>%
  ggplot(aes(x = release_speed, y = velo_diff, color = .pred)) +
    geom_point() +
    scale_color_viridis_c()
  














