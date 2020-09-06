#########################
### Name: Nate Rowan
### Date Created: 9/2/2020
### Date Last Modified: 9/2/2020
### Description: load in all of 2019 pitch data
#########################

library(tidyverse); theme_set(theme_minimal())
library(ggridges)

scherzer <- read_csv(here::here("datasets/scherzer_2019_pitches.csv"))

scherzer %>%
  mutate(
    pfx_x = as.numeric(pfx_x),
    pfx_z = as.numeric(pfx_z),
    release_speed = as.numeric(release_speed)
  ) %>%
  filter(pitch_type != "null") %>%
  ggplot(aes(pfx_x, pfx_z, alpha = release_speed)) +
    geom_point(aes(color = pitch_name))

scherzer <- scherzer %>%
  filter(pitch_name != "null")

scherzer %>%
  mutate(release_speed = as.numeric(release_speed)) %>%
  ggplot(aes(release_speed, pitch_name)) +
    geom_density_ridges() +
    scale_x_continuous()






