# this file is meant to classify which cuisines/restaurant categories
# tend to offer live music the most.

# let's first get a sense of what categories there are in total!
restaurant_categories <- restaurants_and_bars |>
  separate_rows(categories, sep = ",") |> 
  mutate(
    categories = trimws(categories)
  )

cuisine_fracs <- restaurant_categories |> 
  group_by(categories) |> 
  summarize(
    num_music = sum(music_treatment),
    num_total = n(),
    frac_music = num_music / num_total
  ) |>
  filter(num_total > 20) |> 
  arrange(desc(frac_music))

# hold on, jazz and blues has 18 restaurants which aren't treated...
# let's look at these. then we eat.
bad_jazz <- restaurant_categories |> 
  filter(categories == 'Jazz & Blues', music_treatment == 0)
