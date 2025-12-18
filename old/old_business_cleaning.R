# This file cleans some of the variables in business_data
# and adds the music treatment indicator. (as of 11/29, the name of this
# file is a bit misleading.)

library(tidyverse)

# primary cities and states  ----------------------------------------------
states <- c("ID", "NV", "CA", "AZ",
            "MO", "IN", "PA",
            "TN", "LA", "FL")

cities <- c("boise","reno","santa barbara","tucson","st louis",
            "indianapolis","philadelphia",
            "nashville","new orleans","tampa")

# variable cleaning ------------------------------------------------------
cleaned_business_data <- business_data |> 
  mutate(
    # city cleaning
    city = trimws(str_to_lower(city)),
    city = str_replace_all(city, regex("\\."), ""),
  )

# we might limit cities
# cleaned_business_data <- cleaned_business_data |> 
#   filter(city %in% cities)

## add live music treatment dummy -----------------------------------------

## CURRENT METHOD FOR DETERMINING IF A RESTAURANT OFFERS LIVE MUSIC:
# treatment: more than 1 music review
# control: 0 music reviews 
# (1 review is unclear)
cleaned_business_data <- cleaned_business_data |> 
  left_join(num_music_reviews_by_id, by = "business_id") |>
  mutate(
    # turn NA's into zeroes for number of music reviews
    num_music_reviews = replace_na(num_music_reviews, 0),
    # add restaurant/bar dummy
    restaurant_or_bar = if_else(str_detect(categories, regex("restaurant|bar",
                                                 ignore_case = TRUE)), 1, 0),
    # finally, add music treatment
    music_treatment = case_when(
      num_music_reviews > 1 & restaurant_or_bar ~ 1,
      num_music_reviews == 0 ~ 0
      )
  ) |>
  filter(!is.na(music_treatment)) |> # for now, only include treatment, control
  relocate(num_music_reviews, music_treatment, restaurant_or_bar)


cleaned_business_data |> 
  filter(music_treatment == 1 & restaurant_or_bar == 0) |> 
  count() # number of non-restaurants or bars in treatment: 0

cleaned_business_data |> 
  filter(music_treatment == 1) |> 
  count() # how many restaurants/bars offer live music: 3610


# restaurants_and_bars ----------------------------------------------------
restaurants_and_bars <- cleaned_business_data |> 
  filter(restaurant_or_bar == 1) |> 
  select(!restaurant_or_bar) |> 
  relocate(categories)












