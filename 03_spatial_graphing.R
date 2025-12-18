# this 
library(tigris)
library(sf)
library(stringr)
library(patchwork)

options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

# Globals -----------------------------------------------------------------
COLORS <- c("#0072B2", "#D55E00")  # Okabe–Ito: blue, vermillion
NUM_GRAPH_COLS <- 3
FONT_SIZE <- 15

STATES <- c("ID", "NV", "CA", "AZ",
            "MO", "IN", "PA",
            "TN", "LA", "FL")

CITIES <- c("Tampa", "Tucson", "Indianapolis",
            "Philadelphia", "New Orleans", "Reno",
            "Boise", "Nashville", "St Louis", "Santa Barbara")

# IMPORTANT: map each city to its state (in the same order as CITIES)
CITY_STATE <- c("FL", "AZ", "IN",
                "PA", "LA", "NV",
                "ID", "TN", "MO", "CA")

cities_and_states <- tibble(
  city       = CITIES,
  state_abbr = CITY_STATE
)

# Some Census place names differ from common usage
place_alias <- c(
  "Nashville"    = "Nashville-Davidson metropolitan government",
  "Indianapolis" = "Indianapolis city",
  "St Louis"     = "St. Louis"
)


# Output folders ----------------------------------------------------------
dir.create("graphs", showWarnings = FALSE, recursive = TRUE)
dir.create("graphs/cities", showWarnings = FALSE, recursive = TRUE)
dir.create("graphs/sized_cities", showWarnings = FALSE, recursive = TRUE)

# Helpers -----------------------------------------------------------------
clean_city <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("\\.", "") |>
    str_squish()
}

bind_sf <- function(x) {
  x <- x[!vapply(x, is.null, logical(1))]
  if (length(x) == 0) return(NULL)
  do.call(rbind, x)
}

# City boundaries + layers (tracts + water) -------------------------------
get_city_shape <- function(city_name, year = 2024) {
  state_abbr <- cities_and_states |>
    filter(city == city_name) |>
    pull(state_abbr)

  if (length(state_abbr) != 1) {
    stop("City '", city_name, "' not found (or not unique) in cities_and_states mapping.")
  }

  query_name <- if (city_name %in% names(place_alias)) place_alias[[city_name]] else city_name

  pl <- places(state = state_abbr, cb = TRUE, year = year)

  hits <- pl |>
    filter(str_to_lower(NAME) == str_to_lower(query_name))

  if (nrow(hits) == 0) {
    hits <- pl |>
      filter(str_detect(NAME, fixed(query_name, ignore_case = TRUE)))
  }

  if (nrow(hits) == 0) {
    stop("No Census 'places' match found for '", city_name, "' in state '", state_abbr, "'.")
  }

  hits |>
    slice_max(ALAND, n = 1)
}

get_city_layers <- function(city_name, year = 2024) {
  city_poly <- get_city_shape(city_name, year = year) |>
    st_make_valid()

  state_abbr <- cities_and_states |>
    filter(city == city_name) |>
    pull(state_abbr)
  if (length(state_abbr) != 1) stop("No unique state for city = ", city_name)

  cty <- counties(state = state_abbr, cb = TRUE, year = year) |>
    st_transform(st_crs(city_poly)) |>
    st_make_valid()

  hit <- lengths(st_intersects(cty, city_poly)) > 0
  cty_hit <- cty[hit, ]
  county_fips <- cty_hit$COUNTYFP

  tr_list <- map(county_fips, ~ tracts(state = state_abbr, county = .x, cb = TRUE, year = year))
  aw_list <- map(county_fips, ~ area_water(state = state_abbr, county = .x, year = year))
  lw_list <- map(county_fips, ~ linear_water(state = state_abbr, county = .x, year = year))

  tr <- bind_sf(tr_list)
  aw <- bind_sf(aw_list)
  lw <- bind_sf(lw_list)

  if (!is.null(tr)) tr <- tr |> st_transform(st_crs(city_poly)) |> st_make_valid() |> st_intersection(city_poly)
  if (!is.null(aw)) aw <- aw |> st_transform(st_crs(city_poly)) |> st_make_valid() |> st_intersection(city_poly)
  if (!is.null(lw)) lw <- lw |> st_transform(st_crs(city_poly)) |> st_make_valid() |> st_intersection(city_poly)

  list(city = city_poly, tracts = tr, water_poly = aw, water_line = lw)
}

# Points (SF) -------------------------------------------------------------
business_points <- filtered_business_data |>
  transmute(
    longitude, latitude,
    state,
    city = clean_city(city),
    review_count = as.numeric(review_count)
  ) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

restaurant_bar_points <- restaurants_and_bars |>
  transmute(
    longitude, latitude,
    state,
    city = clean_city(city),
    live_music,
    review_count = as.numeric(review_count)
  ) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# US map (states + counties + points) ------------------------------------
us_states <- states(cb = TRUE, resolution = "20m") |>
  shift_geometry()

us_counties <- counties(cb = TRUE, resolution = "20m") |>
  shift_geometry()

us_plot <- ggplot() +
  geom_sf(data = us_states, fill = NA, linewidth = 0.3) +
  geom_sf(data = us_counties, fill = NA, linewidth = 0.1, alpha = 0.4) +
  geom_sf(data = business_points, size = 0.2) +
  coord_sf() +
  theme_void()

ggsave(
  filename = "graphs/us/us_businesses.png",
  plot     = us_plot,
  device   = "png",
  width    = 10,
  height   = 6,
  dpi      = 300
)

# State plots (outline + county lines + points) ---------------------------
graph_state <- function(points_df, state_abbr, color = COLORS[1]) {
  st_outline <- states(cb = TRUE, year = 2024) |>
  filter(STUSPS == state_abbr)
  
  st_counties <- counties(state = state_abbr, cb = TRUE)

  pts <- points_df |>
    filter(state == state_abbr) |>
    st_transform(st_crs(st_counties))

  bb <- st_bbox(st_counties)

  ggplot() +
    geom_sf(data = st_outline, fill = NA, linewidth = 0.5) +
    geom_sf(data = st_counties, fill = NA, linewidth = 0.15, alpha = 0.6) +
    geom_sf(data = pts, color = color, size = 0.2) +
    coord_sf(xlim = c(bb["xmin"], bb["xmax"]),
             ylim = c(bb["ymin"], bb["ymax"])) +
    theme_void()
}

graph_state_business <- function(state_abbr) {
  graph_state(business_points, state_abbr, color = COLORS[1])
}

graph_state_restaurant_bar <- function(state_abbr) {
  graph_state(restaurant_bar_points, state_abbr, color = COLORS[2])
}

state_business_plots <- wrap_plots(
  lapply(STATES, graph_state_business),
  ncol = NUM_GRAPH_COLS
) + plot_annotation(tag_levels = list(STATES))

state_restaurant_bar_plots <- wrap_plots(
  lapply(STATES, graph_state_restaurant_bar),
  ncol = NUM_GRAPH_COLS) + 
  plot_annotation(
    tag_levels = list(STATES),
    theme = theme(plot.tag = element_text(size = FONT_SIZE))
    )

ggsave("graphs/states/state_business.png", plot = state_business_plots, width = 12, height = 8, dpi = 300)
ggsave("graphs/states/state_rb.png",       plot = state_restaurant_bar_plots, width = 12, height = 8, dpi = 300)

# City plots ------------------------------------------------
graph_city <- function(points_df, city_name, color = COLORS[1], year = 2024) {
  city_shape <- get_city_shape(city_name, year = year)
  city_key <- clean_city(city_name)

  pts <- points_df |>
    filter(city == city_key) |>
    st_transform(st_crs(city_shape))

  bb <- st_bbox(city_shape)

  ggplot() +
    geom_sf(data = city_shape) +
    geom_sf(data = pts, color = color, size = 0.2) +
    coord_sf(xlim = c(bb["xmin"], bb["xmax"]),
             ylim = c(bb["ymin"], bb["ymax"])) +
    theme_void()
}

graph_city_business <- function(city_name) {
  graph_city(business_points, city_name, color = COLORS[1])
}

graph_city_restaurant_bar <- function(city_name) {
  graph_city(restaurant_bar_points, city_name, color = COLORS[2])
}

city_business_plots <- wrap_plots(
  lapply(CITIES, graph_city_business),
  ncol = NUM_GRAPH_COLS) + 
  plot_annotation(
    tag_levels = list(CITIES),
    theme = theme(plot.tag = element_text(size = FONT_SIZE))
    )

city_restaurant_plots <- wrap_plots(
  lapply(CITIES, graph_city_restaurant_bar),
  ncol = NUM_GRAPH_COLS) + 
  plot_annotation(
    tag_levels = list(CITIES),
    theme = theme(plot.tag = element_text(size = FONT_SIZE))
    )

ggsave("graphs/city_businesses.png", plot = city_business_plots, width = 12, height = 8, dpi = 300)
ggsave("graphs/city_rb.png",         plot = city_restaurant_plots, width = 12, height = 8, dpi = 300)

# Live music in cities (points only) -------------------------------------
live_music_points <- restaurant_bar_points |>
  filter(live_music == 1)

graph_live_music_city <- function(city_name, year = 2024) {
  city_shape <- get_city_shape(city_name, year = year)
  city_key <- clean_city(city_name)

  rb_points <- restaurant_bar_points |>
    filter(city == city_key) |>
    st_transform(st_crs(city_shape))

  lm_points <- live_music_points |>
    filter(city == city_key) |>
    st_transform(st_crs(city_shape))

  bb <- st_bbox(city_shape)

  ggplot() +
    geom_sf(data = city_shape) +
    geom_sf(data = rb_points, color = COLORS[1], size = 0.2, alpha = 0.5) +
    geom_sf(data = lm_points, color = COLORS[2], size = 0.2, alpha = 0.8) +
    coord_sf(xlim = c(bb["xmin"], bb["xmax"]),
             ylim = c(bb["ymin"], bb["ymax"])) +
    theme_void()
}

comparison_city_plots <- wrap_plots(
  lapply(CITIES, graph_live_music_city),
  ncol = NUM_GRAPH_COLS) + 
  plot_annotation(
    tag_levels = list(CITIES),
    theme = theme(plot.tag = element_text(size = FONT_SIZE))
    )

philly_plot    <- graph_live_music_city("Philadelphia")
nashville_plot <- graph_live_music_city("Nashville")
nola_plot      <- graph_live_music_city("New Orleans")

small_comparison_plot <- (nola_plot | nashville_plot) / philly_plot +
  plot_annotation(tag_levels = list(c("New Orleans", "Nashville", "Philadelphia")))

ggsave("graphs/comparison_city.png",       plot = comparison_city_plots, width = 12, height = 8, dpi = 300)
ggsave("graphs/small_comparison_plot.png", plot = small_comparison_plot,  width = 10, height = 7, dpi = 300)

# Resized city points (RAW review_count) ---------------------------------
resize_city <- function(city_name, points_df = restaurant_bar_points, color = COLORS[1], year = 2024) {
  city_poly <- get_city_shape(city_name, year = year)
  city_key <- clean_city(city_name)

  pts <- points_df |>
    filter(city == city_key) |>
    filter(!is.na(review_count)) |>
    st_transform(st_crs(city_poly))

  bb <- st_bbox(city_poly)

  ggplot() +
    geom_sf(data = city_poly) +
    geom_sf(data = pts, aes(size = review_count), color = color, alpha = 0.6) +
    scale_size_area(max_size = 2, breaks = c(10, 50, 200, 1000), name = "Review count") +
    coord_sf(xlim = c(bb["xmin"], bb["xmax"]),
             ylim = c(bb["ymin"], bb["ymax"])) +
    theme_void()
}

resize_city_plots <- wrap_plots(
  lapply(CITIES, resize_city),
  ncol = NUM_GRAPH_COLS) + 
  plot_annotation(
    tag_levels = list(CITIES),
    theme = theme(plot.tag = element_text(size = FONT_SIZE))
    )

# Cities with tracts + water --------------------------
graph_city_rb_pretty <- function(city_name, year = 2024) {
  layers <- get_city_layers(city_name, year = year)
  city_poly <- layers$city
  city_key <- clean_city(city_name)

  pts <- restaurant_bar_points |>
    filter(city == city_key) |>
    st_transform(st_crs(city_poly))

  bb <- st_bbox(city_poly)

  ggplot() +
  geom_sf(data = layers$water_poly, color = NA, alpha = 0.5) +
  geom_sf(data = layers$water_line, alpha = 0.2) +
  geom_sf(data = layers$tracts, fill = NA, linewidth = 0.15, alpha = 0.7) +
  geom_sf(data = city_poly, fill = NA, linewidth = 0.4) +
  geom_sf(data = pts, color = COLORS[1], alpha = 0.45, size = 0.1) +
  scale_colour_manual(name   = "") +
  coord_sf(xlim = c(bb["xmin"], bb["xmax"]),
           ylim = c(bb["ymin"], bb["ymax"])) +
  theme_void()
}

purrr::walk(CITIES, \(city) {
  p <- graph_city_rb_pretty(city)

  file <- file.path(
    "graphs/cities",
    paste0("rb_", str_replace_all(tolower(city), "\\s+", "_"), ".png")
  )

  ggsave(filename = file, plot = p, width = 8, height = 6, dpi = 300)
})

business_city_plots <- wrap_plots(
  lapply(CITIES, graph_city_business_pretty), ncol = NUM_GRAPH_COLS) + 
  plot_annotation(tag_levels = list(CITIES)) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "right",
    plot.tag = element_text(size = FONT_SIZE, face = "plain", hjust = 0.5),
    plot.tag.position = c(0.5, 0.0),
    plot.margin = margin(6, 6, 14, 6)
  )

ggsave(filename = "graphs/cities/all_rb_city_plots.png", 
       business_city_plots, width = 14, height = 10, dpi = 300)


graph_live_music_city_pretty <- function(city_name, year = 2024) {
  layers <- get_city_layers(city_name, year = year)
  city_poly <- layers$city
  city_key <- clean_city(city_name)

  pts <- restaurant_bar_points |>
    filter(city == city_key) |>
    st_transform(st_crs(city_poly))

  rb_points <- pts |> mutate(pt_type = "No Live Music")
  lm_points <- pts |> filter(live_music == 1) |>  mutate(pt_type = "Live Music")

  bb <- st_bbox(city_poly)

  ggplot() +
  geom_sf(data = layers$water_poly, color = NA, alpha = 0.5) +
  geom_sf(data = layers$water_line, alpha = 0.2) +
  geom_sf(data = layers$tracts, fill = NA, linewidth = 0.15, alpha = 0.7) +
  geom_sf(data = city_poly, fill = NA, linewidth = 0.4) +
  geom_sf(data = rb_points, aes(colour = pt_type), alpha = 0.45, size = 0.2) +
  geom_sf(data = lm_points, aes(colour = pt_type), alpha = 0.75, size = 0.2) +
  scale_colour_manual(
    name   = "",
    values = c("No Live Music" = COLORS[1], "Live Music" = COLORS[2])
  ) +
  coord_sf(xlim = c(bb["xmin"], bb["xmax"]),
           ylim = c(bb["ymin"], bb["ymax"])) +
  theme_void() +
  guides(colour = guide_legend(override.aes = list(size = 2, alpha = 1))) +
  theme(legend.position = "bottom")
}

purrr::walk(CITIES, \(city) {
  p <- graph_live_music_city_pretty(city)

  file <- file.path(
    "graphs/cities",
    paste0("live_music_", str_replace_all(tolower(city), "\\s+", "_"), ".png")
  )

  ggsave(filename = file, plot = p, width = 8, height = 6, dpi = 300)
})

pretty_city_plots <- wrap_plots(
  lapply(CITIES, graph_live_music_city_pretty), ncol = NUM_GRAPH_COLS) + 
  plot_annotation(tag_levels = list(CITIES)) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "right",
    plot.tag = element_text(size = FONT_SIZE, face = "plain", hjust = 0.5),
    plot.tag.position = c(0.5, 0.0),
    plot.margin = margin(6, 6, 14, 6)
  )

ggsave(filename = "graphs/cities/all_pretty_city_plots.png", 
       pretty_city_plots, width = 14, height = 10, dpi = 300)


small_city_list <- c("Nashville", "New Orleans", "Philadelphia")
small_pretty_cities <- wrap_plots(
  lapply(small_city_list, graph_live_music_city_pretty), ncol = NUM_GRAPH_COLS) + 
  plot_annotation(tag_levels = list(small_city_list)) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "right",
    plot.tag = element_text(size = FONT_SIZE, face = "plain", hjust = 0.5),
    plot.tag.position = c(0.5, 0.0),
    plot.margin = margin(6, 6, 14, 6)
  )

ggsave(filename = "graphs/sized_cities/small_pretty_city_plots.png", small_pretty_cities)


# Sized points ------------------------------
graph_live_music_city_sized <- function(city_name, size_var = c("log1p", "raw"), year = 2024) {
  size_var <- match.arg(size_var)

  layers <- get_city_layers(city_name, year = year)
  city_poly <- layers$city
  city_key <- clean_city(city_name)

  pts <- restaurant_bar_points |>
    filter(city == city_key) |>
    filter(!is.na(review_count)) |>
    st_transform(st_crs(city_poly))

  rb_points <- pts |>
    filter(live_music == 0) |> 
    mutate(pt_type = "No Live Music")
  
  lm_points <- pts |> 
    filter(live_music == 1) |> 
    mutate(pt_type = "Live Music")

  bb <- st_bbox(city_poly)

  size_aes <- if (size_var == "log1p") rlang::expr(log1p(review_count)) else rlang::expr(review_count)

  ggplot() +
    geom_sf(data = layers$water_poly, color = NA, alpha = 0.3) +
    geom_sf(data = layers$water_line, alpha = 0.3) +
    geom_sf(data = layers$tracts, fill = NA, linewidth = 0.15, alpha = 0.7) +
    geom_sf(data = city_poly, fill = NA, linewidth = 0.4) +
    geom_sf(data = rb_points, aes(size = !!size_aes, colour = pt_type), alpha = 0.5) +
    geom_sf(data = lm_points, aes(size = !!size_aes, colour = pt_type), alpha = 0.5) +
    scale_size_area(max_size = 2, name = "Review count") +
    scale_colour_manual(
      name   = "",
      values = c("No Live Music" = COLORS[1], "Live Music" = COLORS[2]),
      drop   = FALSE
    ) +
    coord_sf(xlim = c(bb["xmin"], bb["xmax"]),
             ylim = c(bb["ymin"], bb["ymax"])) +
    theme_void() +
    guides(colour = guide_legend(override.aes = list(size = 2, alpha = 1))) +
    guides(size = "none") +
    theme(legend.position = "bottom")
}

sized_city_plots <- wrap_plots(
  lapply(CITIES, graph_live_music_city_sized), ncol = NUM_GRAPH_COLS) + 
  plot_annotation(tag_levels = list(CITIES)) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "right",
    plot.tag = element_text(size = FONT_SIZE, face = "plain", hjust = 0.5),
    plot.tag.position = c(0.5, 0.0),
    plot.margin = margin(6, 6, 14, 6)
  )
# these don't look great, but we'll use the regular pretty ones!

ggsave(filename = "graphs/sized_cities/all_sized_city_plots.png", 
       sized_city_plots, width = 14, height = 10, dpi = 300)


purrr::walk(CITIES, \(city) {
  p <- graph_live_music_city_sized(city, size_var = "log1p")  # change to "raw" if desired

  file <- file.path(
    "graphs/sized_cities",
    paste0("sized_", str_replace_all(tolower(city), "\\s+", "_"), ".png")
  )

  ggsave(filename = file, plot = p, width = 8, height = 6, dpi = 300)
})

graph_live_music_city_sized("Nashville", size_var = "log1p")
