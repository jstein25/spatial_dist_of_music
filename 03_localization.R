# this file is to use the Duranton-Overman metric for localization
# applied to our music businesses.

library(tidyverse)
library(sf)
library(spatstat.geom)
library(dbmss)

# globals ---------------------------------------------------------------
# assumes you already have:
#   - restaurants_and_bars with: city, latitude, longitude, live_music
#   - CITIES (pretty city names)
#   - get_city_shape(city_name) -> sf polygon for the city boundary

# helpers ---------------------------------------------------------------
canon_city <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("\\.", "") |>
    stringr::str_squish()
}

get_pts_sf <- function(df, city_name) {
  df |>
    mutate(city = canon_city(city)) |>
    filter(city == canon_city(city_name)) |>
    filter(!is.na(latitude), !is.na(longitude)) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
    st_transform(5070) # meters (NAD83 / Conus Albers)
}

get_xy <- function(df, city_name) {
  pts_sf <- get_pts_sf(df, city_name)
  st_coordinates(pts_sf) # matrix (x,y) in meters
}

get_median_distance <- function(df, city_name) {
  xy <- get_xy(df, city_name)
  if (nrow(xy) < 2) return(NA_real_)
  median(as.numeric(stats::dist(xy)))
}

# r_max (pooled median; DO-style) ---------------------------------------
median_restaurant_distances <- purrr::map_dbl(
  CITIES,
  ~ get_median_distance(restaurants_and_bars, .x)
)
names(median_restaurant_distances) <- CITIES

# round pooled median up to nearest km; keep units in meters
r_max <- ceiling(median(median_restaurant_distances, na.rm = TRUE) / 1000) * 1000

# envelope computation ---------------------------------------------------
get_envelope <- function(df, city_name, r_max_m = r_max, step_m = 100) {

  pts_sf <- get_pts_sf(df, city_name)
  xy <- st_coordinates(pts_sf)

  pp_df <- pts_sf |>
    st_drop_geometry() |>
    transmute(
      X = xy[, 1],
      Y = xy[, 2],
      PointType   = factor(if_else(live_music == 1, "LiveMusic", "NotLive")),
      PointWeight = 1
    )

  city_poly <- get_city_shape(city_name) |>
    st_make_valid() |>
    st_transform(5070)

  W <- spatstat.geom::as.owin(city_poly)

  r <- seq(0, r_max_m, by = step_m)

  X_pp <- wmppp(pp_df, window = W)

  KdEnvelope(
    X_pp,
    r = r,
    ReferenceType  = "LiveMusic",
    NeighborType   = "LiveMusic",
    SimulationType = "RandomLabeling",
    NumberOfSimulations = 100,
    Alpha = 0.05,
    Global = TRUE
  )
}

# compute all envelopes --------------------------------------------------
envelopes <- purrr::map(CITIES, ~ get_envelope(restaurants_and_bars, .x))
names(envelopes) <- CITIES

# localization measure (Gamma) ------------------------------------------
get_localization_from_env <- function(envelope) {
  fv <- as.data.frame(envelope$fv)

  # DO gamma(d): excess above upper global band
  gamma_d <- pmax(fv$obs - fv$hi, 0)

  # sum across distances (~ Riemann)
  dr <- c(diff(fv$r), tail(diff(fv$r), 1))
  sum(gamma_d * dr)
}

localization_tbl <- tibble(
  city  = names(envelopes),
  Gamma = purrr::map_dbl(envelopes, get_localization_from_env)
) |>
  arrange(desc(Gamma))

print(localization_tbl)

# optional debug ---------------------------------------------------------
# names(as.data.frame(envelopes[[1]]$fv))