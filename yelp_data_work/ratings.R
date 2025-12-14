# install.packages("texreg", "fixest")
library(texreg)
library(fixest)

# only look at states and cities we want:
STATES <- c("ID", "NV", "CA", "AZ",
            "MO", "IN", "PA",
            "TN", "LA", "FL")

CITIES <- c("Boise","Reno","Santa Barbara","Tucson","St. Louis",
            "Indianapolis","Philadelphia",
            "Nashville","New Orleans","Tampa")

# get restaurants and bars for regression data
restaurants_and_bars <- cleaned_business_data |> 
  filter(lm_treatment == 1) |> 
  select(business_id, city, state,
       postal_code, stars, review_count, 
       music_treatment, music_control)
  
restaurants_and_bars_state <-  cleaned_business_data |>
  filter(state %in% states)

restaurants_and_bars_city <- restaurants_and_bars_state |> 
  filter(city %in% cities)


# Regressions -------------------------------------------------------------

# Let's do some regressions of rating on live music offering.
simple_regression <- lm(stars ~ music_treatment, 
                        data = restaurants_and_bars)
# summary(simple_regression)

# all cities under observation
all_cities_fe <- feols(stars ~ music_treatment | city,
                  data = restaurants_and_bars_state)
summary(all_cities_fe)

# only the cities we like
city_fe <- feols(stars ~ music_treatment | city,
                 data = restaurants_and_bars_city)
# summary(city_fe)

# potal codes
postal_fe <- feols(stars ~ music_treatment | postal_code,
                   data = restaurants_and_bars)
# summary(postal_fe)

regressions <- list(simple_regression, city_fe, all_cities_fe, postal_fe)

reg_output <- texreg(regressions,
       booktabs = TRUE,
       dcolumn = TRUE,
       use.packages = FALSE,
       label = "tab:regs",
       caption = "Regression results",
       custom.model.names = c("OLS","City FE","Cities in State FE","Postal FE"))
