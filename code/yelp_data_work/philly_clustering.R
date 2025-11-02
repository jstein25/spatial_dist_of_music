library(tigris)
library(mapview)
library(sf)
library(tidyverse)
library(dplyr)
options(tigris_use_cache = TRUE)
# States (cartographic boundary = simplified)

# get place -------------------------------
pa_places <- places(state = "PA", cb = TRUE)
pa_places

philly <- pa_places %>%
  filter(NAME == "Philadelphia")

# get food/drink and music venues --------------
philly_food_drink <- food_drink_data %>%
  filter(city == "Philadelphia")

philly_music_places <- lm_reviews_food_drink %>%
  filter(city == "Philadelphia")

# make points -----------------------
philly_music_points <- st_as_sf(
  philly_music_places,
  coords = c("longitude", "latitude"),
  crs = st_crs(philly)
  ) %>%
  mutate(place_type = "Live Music venue")

philly_fd_points <- st_as_sf(
  philly_food_drink,
  coords = c("longitude", "latitude"),
  crs = st_crs(philly)
  ) %>%
  mutate(place_type = "Food/Drink place")

philly_points <- bind_rows(
  philly_music_points,
  philly_fd_points
) %>%
  select(place_type, longitude, latitude)

# plot ---------------------
ggplot() +
  geom_sf(data = philly, fill = "gray95", color = "black", size = 0.2) +
  geom_sf(
    data = philly_points, 
    aes(color = place_type), 
    size = 0.5, 
    alpha = 0.8
  ) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Food/Drink place" = "blue",
      "Live Music venue" = "red"
    )
  ) +
  ggtitle("Food/Drink places in Philadelphia") +
  theme_void() +
  theme(
    legend.position = "right"
  )
