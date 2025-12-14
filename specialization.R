# from slide notes:
# location quotient Li = si/xi shows a location's level of specialization
# where si is a location's share of music places
# and xi is share of restaurants and bars
# we can compare across cities.
TOTAL_RB <- restaurants_and_bars |>
  nrow()

TOTAL_MUSIC <- restaurants_and_bars |> 
  filter(music_treatment == 1) |> 
  nrow()

x <- restaurants_and_bars |> 
  group_by(city) |> 
  summarize(
    num_rb = n()
    ) |> 
  mutate(
    x = num_rb / TOTAL_RB
  )

s <- restaurants_and_bars |>
  filter(music_treatment == 1) |> 
  group_by(city) |> 
  summarize(
    num_music = n()
  ) |> 
  mutate(
    s = num_music / TOTAL_MUSIC
  )

location_quotients <- x |> 
  inner_join(s, by = "city") |> 
  relocate(city, num_music, num_rb, s, x) |> 
  mutate(
    l = s / x
  )


# can use locational Gini coefficient to measure inequality of specialization
# Gini := 1 - sum_i(x_i(s_i + 2(sum_(j = i+1)sj)))

location_quotients <- location_quotients |> 
  mutate(
    s_j = sum(s)
  )

compute_gini_row <- location_quotients |>
  rowwise() |> 
  mutate(
      row_gini = x * (s + 2 * (sum(s)))
    )

# naive concentration:
# sum across i(s_i - s_i)^2


# Ellison-Glaeser index:
#           G - (1 - sum_i((x_i)^2)H
# gamma = -----------------------------
#         (1 - sum_i((x_i))^2)(1 - H)
# 
# where
# G := S_i((s_i - x_i)^2)
# H := S_j((z_j)^2)

































