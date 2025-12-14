# what kinds of places are there?
num_categories <- restaurants_and_bars |>
  separate_longer_delim(
    categories,
    delim = ",",
  ) |>
  mutate(
    category = str_to_lower(trimws(categories))
  ) |> 
  group_by(category) |> 
  summarize(num_occurances = n()) |> 
  arrange(desc(num_occurances))
# there's some weird categories but that's okay

# what kinds of places offer live music?
all_categories <- restaurants_and_bars |>
  separate_longer_delim(
    categories,
    delim = ",",
  ) |>
  mutate(
    category = categories |> 
      trimws() |> 
      str_to_lower() |> 
      fct()
  ) |> 
  relocate(category)

# let's look at the distribution
num_categories |> 
  ggplot(aes(x = num_occurances)) +
  ylim(0, 10) +
  xlim(0, 20000) +
  geom_histogram()



# can we run a regression of live music adoption on category?

# this may or may not work:
# music_on_category = glm(music_treatment ~ category,
#                         family = binomial(link = "probit"),
#                         data = all_categories)
# summary(music_on_category)















