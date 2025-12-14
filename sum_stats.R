
# Cities ------------------------------------------------------------------
# which cities are in the data set?
states <- cleaned_business_data |> 
  distinct(state)

cities <- cleaned_business_data |> 
  distinct(city)


# Music Stats -------------------------------------------------------------
music_stats <- function(city_name) {
  # function for quick summary stats by city
  num_bus <- cleaned_business_data |> 
    filter(city == city_name) |> 
    nrow()
  
  num_food_drink <- cleaned_business_data |> 
    filter(restaurant_or_bar == 1 & city == city_name) |> 
    nrow()
  
  num_mus <- cleaned_business_data |> 
    filter(music_treatment == 1 & city == city_name) |> 
    nrow()
  
  print(
    str_glue(
      "{city_name} stats:
      total businesses:  {num_bus}
      number of food/drink places:  {num_food_drink}
      number of live music places:  {num_mus}"
    )
  )
}


# Reviews ------------------------------------------------------
# year span of review_data
# NOTE: the following code obtains the seq(2005, 2022)
# years <- review_data |>
#   select(date) |> 
#   str_sub(1, 4) |> 
#   ymd(truncated = 2L) |> 
#   unique()

# get number of reviews per year
num_reviews_per_year <- bind_rows(
  review_data |> mutate(type = "total"),
  live_music_reviews |> mutate(type = "music")
  ) |>
  mutate(year = year(as.Date(date))) |>
  count(year, type, name = "count") |>
  complete(year = 2005:2022, type, fill = list(count = 0)) |>
  mutate(log_count = log(pmax(count, 1))) 

# plot this
num_reviews_plot <- num_reviews_per_year |> 
  ggplot(aes(x = year, y = log_count)) +
  geom_line(aes(color = type)) +
  geom_point() +
  labs(
    title = "Log number of reviews in each year 2005-2022",
    x = "Year",
    y = "Log number of reviews"
  ) +
  theme_bw()



















