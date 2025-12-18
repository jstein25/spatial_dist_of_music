# this script is meant to filter reviews based on the businesses
# categorized as restaurants and bars, then get a sample for human-encoding.

## filter reviews ------------------------
restaurant_bar_reviews <- review_data |> 
  filter(business_id %in% restaurant_and_bar_ids) |> # 3 million total reviews
  left_join(filtered_business_data |> select(business_id, city), by = "business_id") |> 
  mutate(year = year(date))
  # ^ for sample balancing

# explore data with random sample
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


# CLASSIFY SAMPLE ---------------------------------------------------------
# the above was submitted to chatGPT (5.1) for an initial classification.
# I then manually verified each entry. Given the errors in the AI 
# classification, the process was essntially the same as 
# manually classifying all observations

# after coding-------------------------------------------------------------
## Read sample -------------------------------------------------------------
classified_sample <- read_excel("data/classification_sample_with_live_music.xlsx") |> 
  select(!chat_live_music)

# the above was submitted to chatGPT
# check discrepancies
chat_pass_one <- read_excel("data/chat_pass_one.xlsx") |> 
  mutate(
    chat_one = live_music
  ) |> 
  select(review_id, chat_one)

first_check <- classified_sample |> 
  inner_join(chat_pass_one, by = "review_id") |> 
  relocate(live_music, chat_one)

discrep <- first_check |> 
  filter(
    chat_one != live_music
  )

write_xlsx(discrep, "output/discrep.xlsx")

## Export to BERT ------------------------------------------------------------
classified_sample <- read_excel("data/classification_sample_with_live_music.xlsx") |> 
  select(review_id, text, live_music)

# write sample for finetuning
write_csv(classified_sample, "output/finetuning_reviews.csv")

# write entire dataset to csv for classification:
write_csv(restaurant_bar_reviews, "output/all_restaurant_bar_reviews.csv")


## Read from BERT: -------------------------------------------------------------
classified_reviews <- read_csv("data/reviews_with_live_music.csv")


# add dummy for live music ----------------------------------------------------
# get live music reviews
live_ids <- classified_reviews |> 
  filter(live_music == 1) |> 
  pull(review_id)

live_music_reviews <- review_data |> 
  filter(review_id %in% live_ids)

num_all_music_businesses <-live_music_reviews |> 
  select(business_id) |> 
  nrow()

one_review_businesses <- live_music_reviews |> 
  group_by(business_id) |> 
  filter(n() == 1)

# how many businesses have one music review 
# compared to the total number of music restaurants/bars?
frac_one_review <- nrow(one_review_businesses) / num_all_music_businesses

# write for manual reclassification
write_xlsx(one_review_businesses, "output/one_review.xlsx")


# RECLASSIFIED ------------------------------------------------------------
# Businesses that only had one music review review are assumed to be
# those which BERT may have not correctly identified. I used the BERT
# model to help reclassify. I manually reclassified these reviews and
# used chatGPT to double check my work.

# read back in
reclassified_exclusions <- read_excel("data/one_music_reclassified.xlsx") |> 
  mutate(live_music = live_music...2) |> 
  select(business_id, live_music) |> 
  filter(live_music == 0)

# exclude these!
live_music_reviews <- live_music_reviews |> 
  filter(!(business_id %in% reclassified_exclusions$business_id))

# how many one-review restaurants/bars now?
new_one_review <- live_music_reviews |> 
  group_by(business_id) |> 
  filter(n() == 1)

# now add live music dummy
restaurants_and_bars <- restaurants_and_bars |> 
  mutate(
    live_music = if_else(business_id %in% live_music_reviews$business_id, 1, 0 )
  ) |> 
  relocate(live_music)

frac_music <- restaurants_and_bars |> 
  summarize(
    num_total = n(),
    num_live_music = sum(live_music == 1),
    frac = num_live_music / num_total
  ) |> 
  pull(frac)


# how well does the "live" categorization do? -----------------------------
rb_attributes <- restaurants_and_bars$attributes
yelp_music_category <- rb_attributes$Music

test_yelp_music_category <- restaurants_and_bars |> 
  mutate(
    yelp_music = yelp_music_category
  ) |> 
  select(live_music, yelp_music) |> 
  filter(str_detect(yelp_music, "'live': False")) |> 
  count(live_music == 1) # misses over 3000 restaurants and bars w/ live music

missing_yelp_category <- yelp_music_category |> 
  tibble() |> 
  summarize(n_missing = sum(is.na(yelp_music_category))) |> 
  pull(n_missing) # 23083





