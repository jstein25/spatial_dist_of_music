# this file is to use the Duranton-Overman metric for localization
# applied to our music businesses.
library(spatstat.geom)
library(dbmss)
library(ggrepel)
library(texreg)

# helpers ---------------------------------------------------------------
clean_city <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("\\.", "") |>
    str_squish()
}

get_pts_sf <- function(df, city_name) {
  df |>
    mutate(city = clean_city(city)) |>
    filter(city == clean_city(city_name)) |>
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
  median(as.numeric(dist(xy)))
}

# r_max (pooled median; DO-style) ---------------------------------------
median_restaurant_distances <- map_dbl(
  CITIES,
  ~ get_median_distance(restaurants_and_bars, .x)
)
names(median_restaurant_distances) <- CITIES

# round pooled median up to nearest km; keep units in meters
r_max <- ceiling(median(median_restaurant_distances, na.rm = TRUE) / 1000) * 1000
# ^7000. hence we compute all distances up to 7000

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

  W <- as.owin(city_poly)

  r <- seq(0, r_max_m, by = step_m)

  X_pp <- wmppp(pp_df, window = W)

  KdEnvelope(
    X_pp,
    r = r,
    ReferenceType  = "LiveMusic",
    NeighborType   = "LiveMusic",
    SimulationType = "RandomLabeling",
    NumberOfSimulations = 1000,
    Alpha = 0.05,
    Global = TRUE
  )
}

# compute all envelopes --------------------------------------------------
envelopes <- map(CITIES, ~ get_envelope(restaurants_and_bars, .x))
names(envelopes) <- CITIES

# localization measure (Gamma) ------------------------------------------
get_localization_from_env <- function(envelope) {
  fv <- as.data.frame(envelope)

  # DO gamma(d): excess above upper global band
  gamma_d <- pmax(fv$obs - fv$hi, 0)

  # sum across distances (~ Riemann)
  dr <- c(diff(fv$r), tail(diff(fv$r), 1))
  sum(gamma_d * dr)
}

localization_tbl <- tibble(
  city  = str_to_lower(names(envelopes)),
  Gamma = map_dbl(envelopes, get_localization_from_env)
) |>
  arrange(desc(Gamma))

# compute fraction music correlation ----------------------------------------
# we first take a look at the number of rb and music places
# can just add to localization_tbl
music_fracs <- restaurants_and_bars |> 
  group_by(city) |> 
  summarize(
    num_rb = n(),
    num_lm = sum(live_music == 1),
    frac_music = num_lm / num_rb
  )

localization_tbl <- localization_tbl |> 
  inner_join(music_fracs, by = "city")


## regress fractions on live music ----------
local_model <- lm(Gamma ~ frac_music, data = localization_tbl)

# export regression results:
texreg(file = "output/local_regression.tex", local_model)


# plot frac to localization -----------------------------------------------
dir.create("graphs/localization", showWarnings = FALSE, recursive = TRUE)

localization_tbl <- localization_tbl |> 
  mutate(
    city = str_to_title(city),
  )

city_to_plot <- "Nashville"

one_envelope_plot <- plot(
  envelopes[[city_to_plot]],
  main = paste("Duranton–Overman Kd for", city_to_plot),
  xlab = "Distance (meters)",
  ylab = expression(K[d](r))
)

localization_plot <-ggplot(localization_tbl,
       aes(x = frac_music, y = Gamma, label = city)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.6, linetype = "dashed") +
  geom_text_repel(size = 3, max.overlaps = 20) +
  labs(
    x = "Fraction of restaurants/bars with live-music reviews",
    y = expression(Gamma[d]),
    title = "Localization of Live-Music Businesses vs Share of Live-Music Places"
  ) +
  theme_minimal()

ggsave(filename = "graphs/localization/localization_table.png", localization_plot)


# plot envelopes ----------------------------------------------------------
# 1. Convert envelopes list → long df ------------------------------------
envelope_df <- imap(envelopes, ~{
  fv <- as.data.frame(.x)   # fv object from dbmss
  tibble(
    city = .y,
    r    = fv$r,
    obs  = fv$obs,
    lo   = fv$lo,
    hi   = fv$hi
  )
}) |>
  list_rbind() |>
  mutate(
    city = stringr::str_to_title(city),
    r_km = r / 1000   # since you’re in meters, convert to km for plotting
  )

# 2. Faceted plot of envelopes for all cities ----------------------------
envelope_plots <- ggplot(envelope_df, aes(x = r_km)) +
  # global envelope (shaded band)
  geom_ribbon(
    aes(ymin = lo, ymax = hi, fill = "Global envelope"),
    alpha = 0.2
  ) +
  # observed Kd
  geom_line(
    aes(y = obs, colour = "Observed Kd"),
    linewidth = 0.4
  ) +
  facet_wrap(~ city, scales = "free_y") +
  labs(
    x = "Distance (km)",
    y = expression(K[d](r)),
    title = "Duranton–Overman Kd Envelopes for Live-Music Businesses"
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Global envelope" = "grey70")
  ) +
  scale_colour_manual(
    name  = NULL,
    values = c("Observed Kd" = "black")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

ggsave(filename = "graphs/localization/envelope_plots.png", envelope_plots)
