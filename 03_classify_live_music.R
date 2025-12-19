# this script loads distilBERT classified reviews and adds
# the live music dummy to the restaurant/bar dataset

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





