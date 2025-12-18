library(jsonlite)
# SET WD:
setwd("")
# WARNING: THIS MAY TAKE SOME TIME TO RUN.

# note that the JSON for the Yelp Open Dataset should be
# downloaded at https://business.yelp.com/data/resources/open-dataset/

# the titles of each of the files for the data are below. Include the path
# to your downloaded Yelp data and run each file in chronological order.

## stream in business data
con <- file("yelp_academic_dataset_business.json", open = "r")
business_data <- stream_in(con)
close(con)
head(business_data)

# stream in review data
con <- file("yelp_academic_dataset_review.json", open = "r")
review_data <- stream_in(con)
close(con)