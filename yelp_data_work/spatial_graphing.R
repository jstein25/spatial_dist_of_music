library(tigris)
library(sf)
library(tidyverse)
library(patchwork)
options(tigris_use_cache = TRUE)


# set top-level vars ------------------------------------------------------
COLORS <- c(
  "#0a75ad",
  "#f08080"
)
NUM_GRAPH_COLS <- 3

STATES <- c("ID", "NV", "CA", "AZ",
            "MO", "IN", "PA",
            "TN", "LA", "FL")

CITIES <- c("Boise","Reno","Santa Barbara","Tucson","St. Louis",
            "Indianapolis","Philadelphia",
            "Nashville","New Orleans","Tampa")


# points --------------------------------------------------------------------
business_points <- cleaned_business_data |>
  select(latitude, longitude, state, city, music_treatment, restaurant_or_bar) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

restaurant_bar_points <- business_points |>
  filter(restaurant_or_bar == 1)



# US ----------------------------------------------------------------------
# shape file
us_states <- states(cb = TRUE, resolution = "20m") |>
  shift_geometry()

# plot
us_plot <- ggplot() +
  geom_sf(data = us_states) +
  geom_sf(data = business_points, size = 0.2) +
  coord_sf() +
  theme_void()

ggsave(us_plot, filename = "graphs/us_businesses.png", device = "png")


# states ------------------------------------------------------------------
graph_state <- function(points_df, state_abbr, color = COLORS[1]) {
  # map a given state
  
  # get shape file
  state <- counties(state = state_abbr, cb = TRUE)
  
  # points
  points <- points_df |>
    filter(state == state_abbr) |>
    st_transform(st_crs(state))
  
  # zoom just to state
  bb <- st_bbox(state)
  xlim <- c(bb["xmin"], bb["xmax"])
  ylim <- c(bb["ymin"], bb["ymax"])
  
  ggplot() +
    geom_sf(data = state) +
    geom_sf(data = points, color = color, size = 0.2) +
    coord_sf(xlim = xlim, ylim = ylim) +
    theme_void()
}


# all businesses wrapper
graph_state_business <- function(state_abbr) {
  graph_state(business_points, state_abbr)
}

# all restaurants/bars wrapper
graph_state_restaurant_bar <- function(state_abbr) {
  graph_state(restaurant_bar_points, state_abbr, color = COLORS[2])
}

# plot
state_business_plots <- lapply(STATES, graph_state_business) |>
  wrap_plots(plotlist = ., ncol = NUM_GRAPH_COLS) +
  plot_annotation(tag_levels = list(STATES))

state_restaurant_bar_plots <- lapply(STATES, graph_state_restaurant_bar) |>
  wrap_plots(plotlist = ., ncol = NUM_GRAPH_COLS) +
  plot_annotation(tag_levels = list(STATES))

# save plots
ggsave(state_business_plots, filename = "graphs/state_business.png")
ggsave(state_restaurant_bar_plots, filename = "graphs/state_rb.png")


# cities ------------------------------------------------------------------
place_alias <- c(
  "Nashville"   = "Nashville-Davidson metropolitan government",
  "Indianapolis"= "Indianapolis city"
)

cities_and_states <- tibble(city = cities, state = states)

graph_city <- function(points_df, city_name, color = COLORS[1]) {
  # get state name
  state_name <- cities_and_states |>
    filter(city == city_name) |>
    pull(state)
  
  # ensure proper name is used for shape file
  query_name <- if_else(city_name %in% names(place_alias),
                        place_alias[[city_name]],
                        city_name)
  
  # get shape file
  city <- places(state = state_name, cb = TRUE) |>
    filter(str_detect(NAME, fixed(query_name, ignore_case = TRUE))) |>
    # if multiple (e.g., balance vs consolidated), take the largest area
    slice_max(ALAND, n = 1)
  
  # get points
  points <- points_df |>
    filter(city == city_name) |>
    st_transform(st_crs(city))
  
  # zoom just to city
  bb <- st_bbox(city)
  xlim <- c(bb["xmin"], bb["xmax"])
  ylim <- c(bb["ymin"], bb["ymax"])
  
  # plot
  ggplot() +
    geom_sf(data = city) +
    geom_sf(data = points, color = color, size = 0.2) +
    coord_sf(xlim = xlim, ylim = ylim) +
    theme_void()
}

# all businesses wrapper
graph_city_business <- function(city_name) {
  graph_city(business_points, city_name)
}

# all restaurants/bars wrapper
graph_city_restaurant_bar <- function(city_name) {
  graph_city(restaurant_bar_points, city_name, color = COLORS[2])
}

# plot
city_business_plots <- lapply(CITIES, graph_city_business) |>
  wrap_plots(plotlist = ., ncol = NUM_GRAPH_COLS) +
  plot_annotation(tag_levels = list(CITIES))

city_restaurant_plots <- lapply(CITIES, graph_city_restaurant_bar) |>
  wrap_plots(plotlist = ., ncol = NUM_GRAPH_COLS) +
  plot_annotation(tag_levels = list(CITIES))

# save plots
ggsave(city_business_plots, filename = "graphs/city_businesses.png")
ggsave(city_restaurant_plots, filename = "graphs/city_rb.png")



# graph restaurant/bar vs live music --------------------------------------
# get points
live_music_points <- business_points |>
  filter(music_treatment == 1)

# graph
graph_live_music_cities <- function(city_name) {
  # get state
  state_name <- cities_and_states |>
    filter(city == city_name) |>
    pull(state)
  
  # get proper query
    query_name <- if_else(city_name %in% names(place_alias),
                        place_alias[[city_name]],
                        city_name)
  
  # get shape file
  city <- places(state = state_name, cb = TRUE) |>
    filter(str_detect(NAME, fixed(query_name, ignore_case = TRUE))) |>
    # if multiple (e.g., balance vs consolidated), take the largest area
    slice_max(ALAND, n = 1)
  
  # get points
  rb_points <- restaurant_bar_points |>
    filter(city == city_name) |>
    st_transform(st_crs(city))
  
  lm_points <- live_music_points |>
    filter(city == city_name) |>
    st_transform(st_crs(city))
    
  # zoom just to city
  bb <- st_bbox(city)
  xlim <- c(bb["xmin"], bb["xmax"])
  ylim <- c(bb["ymin"], bb["ymax"])
  
  # plot
  ggplot() +
    geom_sf(data = city) +
    geom_sf(data = rb_points, color = COLORS[1], size = 0.2) +
    geom_sf(data = lm_points, color = COLORS[2], size = 0.2) +
    coord_sf(xlim = xlim, ylim = ylim) +
    theme_void()
}

# plot
comparison_city_plots <- lapply(cities, graph_live_music_cities) |>
  wrap_plots(plotlist = ., ncol = NUM_GRAPH_COLS) +
  plot_annotation(tag_levels = list(cities))

# quick three city plot
philly_plot <- graph_live_music_cities("Philadelphia")
nashville_plot <- graph_live_music_cities("Nashville")
nola_plot <- graph_live_music_cities("New Orleans")

small_comparison_plot <- (nola_plot | nashville_plot) / philly_plot +
  plot_annotation(tag_levels = list(c("New Orleans", 
                                      "Nashville", "Philadelphia")))

# save plots
ggsave("graphs/comparison_city.png", comparison_city_plots)
ggsave("graphs/small_comparison_plot.png", small_comparison_plot)
