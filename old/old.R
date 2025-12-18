# maybe they have a music attribute...?

music_attributes <- lm_reviews_food_drink %>%
  mutate(
    music = lm_reviews_food_drink$attributes.Music
  ) %>%
  select(name, text, city, state, music)
# ^ shows that music is not always an attribute even when
# live music is mentioned. TODO: join both mentions and non-mentions.

music_stats <- function(city) {
  #
  # function for quick summary stats
  #
  num_bus <- sum(business_data$city == city, na.rm = TRUE)
  num_food_drink <- sum(food_drink_data$city == city, na.rm = TRUE)
  # ^ these two are s.t. nrow == num_businesses
  num_mus <- lm_reviews_food_drink %>%
    filter(city == !!city) %>%
    summarise(n = n_distinct(business_id)) %>%
    pull(n) # count num businesses, not num reviews
  num_live_music_reviews <- sum(lm_reviews_food_drink$city == city)
  
  print(
    glue(
      "{city} stats:
      total businesses:  {num_bus}
      number of food/drink places:  {num_food_drink}
      number of live music places:  {num_mus}
      number of live music reviews: {num_live_music_reviews}"
    )
  )
}

# save(lm_reviews_food_drink, file = "lm_reviews.RData")


### old philly clustering:
library(tigris)
library(sf)
library(tidyverse)

options(tigris_use_cache = TRUE)

# --- get Philly polygon
pa_places <- places(state = "PA", cb = TRUE)

philly <- pa_places %>%
  filter(NAME == "Philadelphia")

# --- filter your businesses
philly_food_drink <- cleaned_business_data %>%
  filter(
    restaurant_or_bar == 1,
    str_detect(str_to_lower(city), "^philadelphia")
  )

philly_music_places <- philly_food_drink %>%
  filter(has_live_music_review == TRUE)

# --- STEP 1: make points in EPSG:4326 (lon/lat)
philly_fd_points <- st_as_sf(
  philly_food_drink,
  coords = c("longitude", "latitude"),
  crs = 4326        # WGS84 lon/lat
) %>%
  mutate(place_type = "1")

philly_music_points <- st_as_sf(
  philly_music_places,
  coords = c("longitude", "latitude"),
  crs = 4326
) %>%
  mutate(place_type = "2")

# --- combine
philly_points <- bind_rows(
  philly_music_points,
  philly_fd_points
) %>%
  select(place_type, geometry)

ggplot() +
  geom_sf(data = philly, fill = "gray95", color = "black", size = 0.5) +
  geom_sf(data = philly_fd_points, color = "blue", size = 0.5) +
  geom_sf(data = philly_music_places, color = "red", size = 0.5) +
  ggtitle("Food/Drink places in Philadelphia") +
  theme_void() +
  theme(legend.position = "right")
