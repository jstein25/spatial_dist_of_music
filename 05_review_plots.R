library(viridis)
# total reviews and how many reviews per city ------------------
reviews_by_city_year <- restaurant_bar_reviews |>
  mutate(city = str_to_title(city)) |>
  count(city, year, name = "n_reviews")

review_distribution_plot <- ggplot(reviews_by_city_year,
       aes(x = year, y = n_reviews, colour = city, group = city)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5) +
  scale_colour_viridis_d(
    option = "plasma",
    end = 0.9,
    name = "City"
  ) +
  labs(
    title = "Number of Restaurant/Bar Reviews Over Time by City",
    x = "Year",
    y = "Number of reviews",
    colour = "City"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave("graphs/reviews/review_dist.png", review_distribution_plot)
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

# rb reviews with live music plot -------------
restaurant_bar_reviews_with_lm <- restaurants_and_bars |> 
  inner_join(review_data, by = "business_id") |> 
  mutate(year = year(date))

frac_lm_reviews_by_city_year <- restaurant_bar_reviews_with_lm |> 
  mutate(city = str_to_title(city)) |> 
  group_by(city, year) |> 
  summarize(
    num_total_reviews = n(),
    num_total_lm_reviews = sum(live_music == 1),
    frac_lm_reviews = num_total_lm_reviews / num_total_reviews
  ) |> 
  ungroup()

live_music_review_distribution_plot <- ggplot(frac_lm_reviews_by_city_year,
  aes(x = year, y = frac_lm_reviews, colour = city, group = city)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5) +
  scale_colour_viridis_d(
    option = "plasma",
    end = 0.9,
    name = "City"
  ) +
  labs(
    title = "Live Music Reviews Over Time",
    x = "Year",
    y = "Fraction of total reviews",
    colour = "City"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave(filename = "graphs/reviews/lm_fracs_review_dist.png", live_music_review_distribution_plot)

