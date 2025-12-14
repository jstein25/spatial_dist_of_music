library(tidyverse)

# detect live music -------------------------------------------------------

## CURRENT METHOD:
# all reviews with "live music" mentions (takes a while):
live_music_reviews <- review_data |>
  filter(str_detect(text, regex("live music", ignore_case = TRUE)))

# number of music reviews -------------------------------------------------
## exploration -------------------------------------------------
num_music_reviews_by_id <- live_music_reviews |> 
  group_by(business_id) |>
  summarize(
    num_music_reviews = n()
    )

# distribution
music_review_dist <- num_music_reviews_by_id |> 
  ggplot(aes(x = num_music_reviews)) +
    geom_histogram()

# see outliers
music_review_outliers <- num_music_reviews_by_id |> 
  ggplot(aes(x = num_music_reviews)) +
  geom_histogram(binwidth = 1) + 
  coord_cartesian(ylim = c(0, 20))
# a number of outliers above 200

# otherwise, primary mode:
music_review_dist_cropped <- num_music_reviews_by_id |> 
  filter(num_music_reviews < 20) |> 
  ggplot(aes(x = num_music_reviews)) +
  geom_histogram(binwidth = 0.5)
# over 3000 business have one review

