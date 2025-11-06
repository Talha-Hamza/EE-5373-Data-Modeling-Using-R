library(dplyr)

# Part 1
house_data <- read.csv("kc_house_data.csv")
head(house_data)



# Part 2
# data_by_zipcode <- house_data %>%
#   group_by(zipcode) %>%
#   summarize(
#     count = n(),
#     med_price = median(price),
#     med_yr_built = median(yr_built),
#   )