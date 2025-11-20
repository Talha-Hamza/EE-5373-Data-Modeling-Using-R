library(dplyr)

raw_house_data <- read.csv("kc_house_data.csv")
house_data <- na.omit(raw_house_data)

# Define new function
price_prediction_error <- function(price, bedrooms, bathrooms, sqft_living, 
                                   sqft_lot, grade, yr_built) {
  
  house_info <- data.frame(price, bedrooms, bathrooms, sqft_living, sqft_lot, grade, yr_built)
  
  rows <- nrow(house_info)
  f <- 0.6
  
  perm <- house_info[sample(rows), ]
  train.dat <- perm[1:floor(f * rows), ]
  test.dat  <- perm[(floor(f * rows) + 1):rows, ]
  
  # Linear model
  house.lm <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + grade 
                 + yr_built, data = train.dat)
  
  # Predictions & RMSE
  pred <- predict(house.lm, newdata = test.dat)
  rmse <- sqrt(mean((test.dat$price - pred)^2))
  
  return(rmse)
}

# Group by zipcode
data_by_zipcode <- house_data %>%
  group_by(zipcode) %>%
  summarize(
    count = n(),
    med_price = median(price),
    med_yr_built = median(yr_built),
    error = price_prediction_error(price, bedrooms, bathrooms, sqft_living,
                                   sqft_lot, grade, yr_built)
  )

plot( x= data_by_zipcode$zipcode, 
      y= data_by_zipcode$error, 
      xlab = "Zipcodes",
      ylab = "RMSE",
      main = "Error vs Zipcode" 
)

boxplot(data_by_zipcode$error, 
        ylab = "RMSE", 
        main = "Boxplot of Errors")

# Why do some zipcodes have higher RMSE than other?
cor(data_by_zipcode$med_price, data_by_zipcode$error)

plot(data_by_zipcode$med_price, data_by_zipcode$error,
     pch = 19,
     xlab = "Median Price",
     ylab = "RMSE",
     main = "Higher Priced Zip Codes = Higher RMSE?")

cor(data_by_zipcode$count, data_by_zipcode$error, use = "complete.obs")

plot(data_by_zipcode$count, data_by_zipcode$error,
     pch = 19,
     xlab = "Number of Houses in Zipcode",
     ylab = "RMSE",
     main = "Does Sample Size Affect RMSE?")


