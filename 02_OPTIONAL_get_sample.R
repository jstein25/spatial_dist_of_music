# this script is outlines the method for getting the distilBERT
# classification sample

sample_reviews <- restaurant_bar_reviews |>
  slice_sample(n = 200)


# Define Sample Terms --------------------------
review_sample_size = 10000

# FIRST QUARTER: Explicit music performance terms (highest signal)
music_dict <- c(
  "live music", "jazz band", "live band", "cover band", "tribute band",
  "acoustic set", "open mic", "live entertainment", "live performer",
  "house band", "resident DJ", "karaoke night", "music venue",
  "concert", "jam session", "live jazz", "live blues", "live country"
)

# SECOND QUARTER: General music-related terms (medium-high signal)
second_quarter_terms <- c(
  "jazz", "country", "blues", "music", "DJ", "band", "record",
  "singer", "musician", "guitar", "piano", "drums", "saxophone",
  "acoustic", "playlist", "jukebox", "sound system", "speakers",
  "rock", "folk", "reggae", "soul", "funk", "hip hop", "electronic"
)

# THIRD QUARTER: Ambiguous/potential false positives (medium-low signal)
third_quarter_terms <- c(
  "funky", "bass", "live", "hoping", "hope", "wish",
  "vibe", "atmosphere", "energy", "mood", "tone",
  "upbeat", "chill", "loud", "quiet", "background",
  "entertaining", "lively", "ambiance", "scene"
)

# FOURTH QUARTER: Random

# Sample ------------------------------------------------------------------
# helper for balanced slicing
balanced_slice <- function(data) {
  result <- data |> 
    group_by(city) |> 
    slice_sample(prop = ((review_sample_size * 0.25) / nrow(data))) |>
    ungroup()
  
  return(result)
}

# set seed
set.seed(123)

# slice samples
first_quarter <- restaurant_bar_reviews |>
  filter(str_detect(text, regex(paste(music_dict, collapse = "|"), ignore_case = TRUE))) |>
  balanced_slice()

second_quarter <- restaurant_bar_reviews |>
  filter(
    !review_id %in% first_quarter$review_id,
    str_detect(text, regex(paste(second_quarter_terms, collapse = "|"), ignore_case = TRUE))
  ) |>
  balanced_slice()

third_quarter <- restaurant_bar_reviews |>
  filter(
    !review_id %in% c(first_quarter$review_id, second_quarter$review_id),
    str_detect(text, regex(paste(third_quarter_terms, collapse = "|"), ignore_case = TRUE))
  ) |>
  balanced_slice()

fourth_quarter <- restaurant_bar_reviews |>
  filter(!review_id %in% c(first_quarter$review_id, 
                              second_quarter$review_id, 
                              third_quarter$review_id)) |>
  balanced_slice()

# Combine
classification_sample <- bind_rows(
  first_quarter |> mutate(sample_stratum = "explicit_music"),
  second_quarter |> mutate(sample_stratum = "general_music"),
  third_quarter |> mutate(sample_stratum = "ambiguous"),
  fourth_quarter |> mutate(sample_stratum = "random")
  )


# plot samples -------------------------------------------------------------
cool_sample_plot <- classification_sample |>
  count(sample_stratum, year, city) |>
  ggplot(aes(x = year, y = n, color = city)) +
  geom_line() +
  scale_colour_viridis_d(
    option = "magma",
    end = 0.9,
    name = "City"
  ) +
  facet_wrap(~sample_stratum) +
  theme_minimal() +
  labs(title = "Sample balance across quarters",
       y = "Number of Reviews")
# ^ wow what an interesting graph!
ggsave(filename = "graphs/reviews/cool_sample_plots.png")

test <- balanced_slice(restaurant_bar_reviews)
nrow(test) # just about a quarter

# write sample to excel for classification
write_xlsx(stratified_sample, "output/classification_sample.xlsx")