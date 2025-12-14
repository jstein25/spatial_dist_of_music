library(jsonlite)
setwd("/Users/jeff/Projects/spatial_dist_of_music")
# WARNING: THIS MAY TAKE A LONG TIME TO RUN.

## stream in business data
con <- file("Yelp_JSON/yelp_dataset/yelp_academic_dataset_business.json", open = "r")
business_data <- stream_in(con)
close(con)
head(business_data)

# stream in review data
con <- file("Yelp_JSON/yelp_dataset/yelp_academic_dataset_review.json", open = "r")
review_data <- stream_in(con)
close(con)