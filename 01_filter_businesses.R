# script for filtering businesses down to those that offer food and/or drink
# based on a dictionary. Filtering is by Yelp classified "category"
library(tidyverse)
library(writexl)
library(readxl)
set.seed(123)

# FILTER FOR CITIES ----------------------------------------
# this might be a slightly controversial step, but
# estimating the model for the entirety of the metro area
# in consideration is slightly outside the scope of this paper.
# While it would be interesting to see the differences between suburban and
# urban areas, filtering down to only the city level speeds up computation and
# makes interpretation a bit simpler.

cities <- c("boise","reno","santa barbara","tucson","st louis",
            "indianapolis","philadelphia",
            "nashville","new orleans","tampa")

# variable cleaning ------------------------------------------------------
filtered_business_data <- business_data |> 
  mutate(
    # city cleaning
    city = trimws(str_to_lower(city)),
    city = str_replace_all(city, regex("\\."), ""),
  ) |>
  filter(city %in% cities)


# Data Description --------------------------------------------------------
# we have the categories of businesses. These include cuisines such as
# Italian, Chinese, Home Repair, etc.

## What are the different types of categories available in the data? --------
businesses_by_categories <- filtered_business_data |>
  separate_rows(categories, sep = ",") |> 
  mutate(categories = trimws(categories)) |> 
  relocate(categories)

## How many categories does a business have on average?
avg_category_counts <- businesses_by_categories |> 
  group_by(business_id) |> 
  count() |> 
  ungroup() |> 
  summarize(
    avg = mean(n)
  ) |> 
  pull(avg) # about 4.5
# hence, adding a slightly conservative filter would likely
# only leave out non-bars/restaurants.

all_business_categories <- businesses_by_categories |> 
  select(categories) |>
  filter(!is.na(categories)) |> 
  unique() # 1265 different categories

## export to excel for categorization. ---------
write_xlsx(all_business_categories, "output/business_categories.xlsx")

## After manually adding food/drink dummy -------
category_dummies <- read_excel("data/business_categories_with_dummy.xlsx")

# take sample for paper
category_sample <- category_dummies |> 
  slice_sample(n = 10)

# filter for restaurants/bars ----------------------------------------
restaurants_and_bars <- businesses_by_categories |> 
  inner_join(category_dummies, by = "categories") |>
  filter(indicates_food_drink == 1) |> 
  group_by(business_id) |>
  summarize(
    categories = paste(categories, collapse = ", "),
    across(everything(), first),
    .groups = "drop"
  ) # 26909 restaurants and bars

# get a sample to evaluate filtering
sample_restaurants_and_bars <- restaurants_and_bars |> 
  slice_sample(n = 100) # (progressively udpated business categories)

# ids to filter reviews with
restaurant_and_bar_ids <- restaurants_and_bars |> 
  select(business_id) |> 
  pull(business_id)

# count reviews per restaurant/bar ------------------------------------------
avg_num_reviews <- restaurants_and_bars |> 
  select(review_count) |> 
  summarize(
    avg_reviews = mean(review_count)
  ) |>
  pull(avg_reviews) # avg of 109 reviews per restaurant/bar

median_num_reviews <- restaurants_and_bars |> 
  select(review_count) |> 
  summarize(
    median_reviews = median(review_count)
  ) |>
  pull(median_reviews) # median of 37


# get review data for rb --------------------------------------------
restaurant_bar_reviews <- review_data |> 
  filter(business_id %in% restaurant_and_bar_ids) |> # 3 million total reviews
  left_join(filtered_business_data |> select(business_id, city), by = "business_id") |> 
  mutate(year = year(date))









