library(dplyr)
library(stringr)
library(readxl)
library(writexl)
library(fixest)
library(texreg)

dir.create("output/regressions", showWarnings = FALSE, recursive = TRUE)
# overall -----------------------------------------------------------------
## 1) Cuisine list --------------------------------------------------------
cuisine_list <- read_excel("data/business_categories_with_dummy.xlsx") |> 
  filter(indicates_food_drink == 1) |> 
  filter(!(categories %in% c("Restaurants", "Bars")))

# write this for appendix
write_xlsx(cuisine_list, "output/restaurant_categories.xlsx")

cuisine_list <- cuisine_list |> 
  pull(categories)

## 2) Base regression data: add price ------------------------------------
rb_attributes <- restaurants_and_bars$attributes

reg_df <- restaurants_and_bars |>
  mutate(
    price_raw = attributes$RestaurantsPriceRange2,
    price_raw = na_if(price_raw, "None"),
    price = as.numeric(price_raw)
  ) |>
  filter(!is.na(price))   # drop rows without price

## 3) Cuisines expansion (only food/drink categories) --------------------
rb_by_categories <- reg_df |>
  separate_rows(categories, sep = ",") |>
  mutate(categories = str_trim(categories)) |>
  filter(categories %in% cuisine_list)


## regress -----------------------------------------------------------------
rating_model1 <- feols(stars ~ live_music, data = restaurants_and_bars)

rating_model2 <- feols(stars ~ live_music | city, 
                       data = restaurants_and_bars,
                       cluster = ~ city
                       )

rating_model3 <- feols(stars ~ live_music + review_count | city, 
                       data = restaurants_and_bars,
                       cluster = ~ city
                       )

rating_model4 <- feols(stars ~ live_music + review_count | price + city, 
                       data = reg_df, 
                       cluster = ~ city
                       )

rating_model4 <- feols(
  stars ~ live_music + review_count | price + categories + city,
  data = rb_by_categories, cluster = ~ city
)

texreg(
  list(rating_model1, rating_model2, rating_model3, rating_model4),
  custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
  custom.coef.map = list(
    "(Intercept)"  = "Constant",
    "live_music"   = "Live music",
    "review_count" = "Review Count"
  ),
  omit.coef = "factor\\(categories\\)",  # drop all cuisine dummies
  stars = c(0.01, 0.05, 0.1),
  digits = 3,
  booktabs = TRUE,
  use.packages = FALSE,
  caption = "Effect of live music on Yelp ratings",
  label   = "tab:ratings_live_music",
  file = "output/regressions/ratings_regression.tex"
)

# by city -----------------------------------------------------------------
run_city_rating_table <- function(city_name) {

  # 1) Filter to this city -----------------------------------------------
  city_base <- restaurants_and_bars |>
    filter(city == str_to_lower(city_name))

  # 3) Add price to this city's data -------------------------------------
  rb_attributes_city <- city_base$attributes

  reg_df_city <- city_base |>
    mutate(
      price_raw = rb_attributes_city$RestaurantsPriceRange2,
      price_raw = na_if(price_raw, "None"),
      price     = as.numeric(price_raw)
    ) |>
    filter(!is.na(price))

  # 4) Expand by cuisine categories for this city ------------------------
  rb_by_categories_city <- reg_df_city |>
    separate_rows(categories, sep = ",") |>
    mutate(categories = str_trim(categories)) |>
    filter(categories %in% cuisine_list)

  # 5) Models -------------
  rating_model1 <- feols(stars ~ live_music, data = city_base)

  rating_model2 <- feols(stars ~ live_music + review_count, data = city_base)

  rating_model3 <- feols(stars ~ live_music + review_count | price,
                      data = reg_df_city)

  rating_model4 <- feols(
    stars ~ live_music + review_count | price + categories,
    data = rb_by_categories_city
  )

  # 6) LaTeX table via texreg -------------------------------------------
  texreg(
    list(rating_model1, rating_model2, rating_model3, rating_model4),
    custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
    custom.coef.map = list(
      "(Intercept)"  = "Constant",
      "live_music"   = "Live music",
      "review_count" = "Review count",
      "price"        = "Price"
    ),
    omit.coef = "factor\\(categories\\)",  # hide cuisine dummies
    stars  = c(0.01, 0.05, 0.1),
    digits = 3,
    booktabs = TRUE,
    use.packages = FALSE,
    caption = paste("Effect of live music on Yelp ratings in", city_name),
    label   = paste0("tab:ratings_live_music_", city_name),
    file = paste0(paste0("output/regressions/", city_name), "_regression.tex")
  )
}

get_r4_model <- function(city_name) {
  # 1) Filter to this city -----------------------------------------------
  city_base <- restaurants_and_bars |>
    mutate(city = str_to_lower(city)) |>
    filter(city == str_to_lower(city_name))
  
  # 2) Add price to this city's data -------------------------------------
  reg_df_city <- city_base |>
    mutate(
      price_raw = attributes$RestaurantsPriceRange2,
      price_raw = na_if(price_raw, "None"),
      price     = as.numeric(price_raw)
    ) |>
    filter(!is.na(price))
  
  # 3) Expand to cuisines for this city ----------------------------------
  rb_by_categories_city <- reg_df_city |>
    separate_rows(categories, sep = ",") |>
    mutate(categories = str_trim(categories)) |>
    filter(categories %in% cuisine_list)
  
  # 4) City-specific regression with price + cuisine FE ------------------
  feols(
    stars ~ live_music + review_count | price + categories,
    data    = rb_by_categories_city,
    cluster = ~ business_id   # cluster at business level if available
  )
}


# run on all cities -------------------------------------------------------
cities_for_reg <- restaurants_and_bars |>
  mutate(city = str_to_lower(city)) |>
  distinct(city) |>
  pull(city)

# run city-specific models
city_models <- map(cities, get_r4_model)
names(city_models) <- cities

# pull out the live_music coefficient + SE for each city
live_music_coefs <- map2_df(city_models, cities, ~{
  b  <- coef(.x)["live_music"]
  V  <- vcov(.x)
  se <- sqrt(V["live_music", "live_music"])
  
  tibble(
    city = .y,
    beta = b,
    se   = se
  )
}) |>
  mutate(
    ci_low  = beta - 1.96 * se,
    ci_high = beta + 1.96 * se,
    city    = factor(city, levels = city[order(beta)])  # order by effect size
  )



# plot --------------------------------------------------------------------

regression_coeffs_plot <- ggplot(live_music_coefs, aes(x = city, y = beta)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_pointrange(aes(ymin = ci_low, ymax = ci_high)) +
  coord_flip() +
  labs(
    x = "City",
    y = "Effect of live music on stars",
    title = "City-specific effect of live music on ratings",
    subtitle = "95% CIs from city-by-city regressions with price and cuisine fixed effects"
  ) +
  theme_minimal(base_size = 11)


ggsave(filename = "graphs/regression_coeffs_plot.png", regression_coeffs_plot)


