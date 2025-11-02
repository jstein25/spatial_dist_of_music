library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(glue)

# Cities --------------------------
city_and_state <- business_data %>%
  distinct(city, state) %>%
  arrange(state, city)

city_and_state$city %>%
  unique() %>%
  length() # 1416

# Business Attributes -----------------------
# we want to take a look at the attributes of our businesses
# not everything that we have here is actually a food/drink spot.
# try to figure out if something serves food/drink:
business_attributes <- business_data$attributes
business_attributes %>%
  names()
# we pulled these manually. These are just things I assume
# a place that serves food/drink might have.
food_drink_attributes <- c(
  "RestaurantsPriceRange2", "RestaurantsTakeOut", "RestaurantsDelivery", 
  "Caters", "HappyHour", "OutdoorSeating", "RestaurantsReservations", 
  "Alcohol", "RestaurantsAttire", "Ambience", "RestaurantsTableService", 
  "RestaurantsGoodForGroups", "GoodForMeal", "BYOB", "Corkage", 
  "BYOBCorkage", "RestaurantsCounterService", "DietaryRestrictions") # manually filled in
# let attributes be discoverable by R
food_drink_data <- business_data %>%
  unnest_wider(attributes, names_sep = ".") %>%
  filter(if_any(all_of(paste0("attributes.", 
                              food_drink_attributes)), ~ . == "True"))
# note: ^ this is a slightly conservative filter that rules out
# places like Target. There may be liquor stores or similar here
# comes out to ALL restaurants/bars in ALL areas
# it is also a crude estimate because some fields do not 
# take the value "True"

# MUSIC REVIEWS ------------------------------------------
# all food and drink spots with live music mentions
lm_reviews_food_drink <- review_data %>%
  filter(str_detect(text, fixed("live music", ignore_case = TRUE))) %>%
  inner_join(food_drink_data, by = "business_id") # only keep matches

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