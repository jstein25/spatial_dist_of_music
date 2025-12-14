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

# Sample Verification -----------------------------------------------------
classification_sample |>
  count(sample_stratum, city, year) |>
  group_by(sample_stratum) |>
  summarize(
    min_n = min(n),
    max_n = max(n),
    mean_n = mean(n),
    total = sum(n)
  )

# Visualize
classification_sample |>
  count(sample_stratum, city, year) |>
  ggplot(aes(x = year, y = n, color = city)) +
  geom_line() +
  facet_wrap(~sample_stratum) +
  theme_minimal() +
  labs(title = "Sample balance across quarters")
# ^ wow what an interesting graph!

test <- balanced_slice(restaurant_bar_reviews)
nrow(test) # just about a quarter

# write sample to excel for classification
write_xlsx(stratified_sample, "data_output/classification_sample.xlsx")


# CLASSIFY SAMPLE ---------------------------------------------------------
# the above was submitted to chatGPT (5.1) for an initial classification.
# I then manually verified each entry. Given the errors in the AI 
# classification, the process amounted to me manually classifying all
# observations.

# after coding:

# Read sample -------------------------------------------------------------
# ... maybe:
classified_sample <- read_excel("data_output/classification_sample_with_live_music.xlsx") |> 
  select(!chat_live_music) |> 
  rename(live_music = verified)

# the above was submitted to chatGPT
# check discrepancies
chat_pass_one <- read_excel("data_output/chat_pass_one.xlsx") |> 
  mutate(
    chat_one = live_music
  ) |> 
  select(review_id, chat_one)

first_check <- coded_sample |> 
  inner_join(chat_pass_one, by = "review_id") |> 
  relocate(chat_live_music, verified, chat_one)

discrep <- first_check |> 
  filter(
    chat_one != verified
  )

write_xlsx(discrep, "data_output/discrep.xlsx")



# Prep for llm ------------------------------------------------------------
classified_sample <- read_excel("data_output/classification_sample_with_live_music.xlsx") |> 
  select(review_id, text, live_music)


# write for finetuning
write_csv(classified_sample, "data_output/finetuning_reviews.csv")

# write entire dataset to csv:
write_csv(restaurant_bar_reviews, "data_output/all_restaurant_bar_reviews.csv")
