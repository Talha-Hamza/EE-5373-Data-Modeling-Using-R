library(ggplot2)
library(gganimate)
library(gifski)
library(dplyr)

raw_house_data <- read.csv(file.choose())
house_data <- na.omit(raw_house_data)

# ---------------- PROBLEM 1 ---------------- 

# Convert date and extract useful time features
house_data$date <- as.Date(house_data$date, format = "%Y%m%dT%H%M%S")
house_data$year <- as.numeric(format(house_data$date , "%Y" ) )
house_data$month <- as.numeric(format(house_data$date , "%m" ) )
# drop id and date
house_data <- house_data %>% 
  select ( -id , -date )
# Convert categorical features
house_data$zipcode <- as.factor( house_data$zipcode )
house_data$waterfront <- as.factor(house_data$waterfront )
house_data$view <- as.factor( house_data$view )
house_data$condition <- as.factor( house_data$condition)
# house_data $ floors <- as.factor(house_data$floors 

f <- 0.6
rows <- nrow(house_data)
perm <- house_data[sample(rows),]
train.dat <- perm [1:floor (f*rows ),]
test.dat <- perm [(floor(f*rows) + 1):rows,]
# Linear model using
house.lm <- lm (( price ) ~ bedrooms + bathrooms + sqft_living +
                  floors + waterfront + view + condition + grade +
                  sqft_above + yr_built + yr_renovated +
                  zipcode + lat +
                  sqft_living15 + sqft_lot15 +
                  year + month ,
                data = train.dat)
pred.log <- predict(house.lm , newdata = test.dat)

# Percent Error
percent_error <- (abs(test.dat$price-pred.log)/test.dat$price)*100

# Add percent error and absolute percent error to test.dat
test.dat$percent_error <- percent_error
test.dat$abs_percent_error <- abs(percent_error)

which_state <- "washington"
county_info <- map_data("county", region = "washington")

# Filter King County
king_county_map <- county_info %>% 
  filter(subregion == "king")

base_map <- ggplot(
  data = county_info,
  mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", fill = "white") +
  coord_quickmap() +
  theme_void()

# Function to create plots for different sampling fractions
create_error_map <- function(q, test_data) {
  # Sample the data based on fraction q
  n_samples <- ceiling(nrow(test_data) * q)
  sampled_data <- test_data %>% sample_n(size = n_samples)
  
  ggplot() +
    geom_polygon(data = king_county_map, 
                 aes(x = long, y = lat, group = group),
                 color = "black", fill = "white") +
    geom_point(data = sampled_data,
               aes(x = long, y = lat, 
                   color = abs_percent_error, 
                   size = price),
               alpha = 0.7) +
    scale_color_gradient(low = "green", high = "red", 
                         name = "Absolute % Error") +
    scale_size_continuous(name = "Price", 
                          range = c(1, 6),
                          breaks = quantile(sampled_data$price, 
                                            probs = c(0.25, 0.5, 0.75))) +
    coord_quickmap() +
    theme_void() +
    ggtitle(paste("House Price Prediction Errors (q =", q, ")")) +
    theme(plot.title = element_text(hjust = 0.5))
}

# different sampling fractions
q_values <- c(0.01, 0.05, 0.10, 1.0)

for (q in q_values) {
  plot <- create_error_map(q, test.dat)
  print(plot)
}

# correlation between price and prediction quality
correlation <- cor(test.dat$price, test.dat$abs_percent_error, use = "complete.obs")

# scatter plot of price vs error
ggplot(test.dat, aes(x = price, y = abs_percent_error)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relationship between House Price and Prediction Error",
       x = "House Price",
       y = "Absolute Percent Error") +
  theme_minimal()

# Summary statistics by price quartiles
price_quartiles <- quantile(test.dat$price, probs = c(0, 0.25, 0.5, 0.75, 1))
test.dat$price_group <- cut(test.dat$price, 
                            breaks = price_quartiles, 
                            labels = c("Q1 (Low)", "Q2", "Q3", "Q4 (High)"),
                            include.lowest = TRUE)

error_by_price_group <- test.dat %>%
  group_by(price_group) %>%
  summarise(
    mean_error = mean(abs_percent_error),
    median_error = median(abs_percent_error),
    n = n()
  )

print("Error statistics by price quartile:")
print(error_by_price_group)

# The correlation is -0.1086361, so no string relationship between erros and house prices

# ---------------- PROBLEM 2 ---------------- 
# Animation for q = 1.0 (all data)
animated_plot <- ggplot() +
  geom_polygon(data = king_county_map,
               aes(x = long, y = lat, group = group),
               color = "black", fill = "white") +
  geom_point(data = test.dat,
             aes(x = long, y = lat,
                 color = abs_percent_error,
                 size = price,
                 frame = yr_built),
             alpha = 0.7) +
  scale_color_gradient(low = "green", high = "red",
                       name = "Absolute % Error") +
  scale_size_continuous(name = "Price",
                        range = c(1, 6)) +
  coord_quickmap() +
  theme_void() +
  labs(title = "Prediction Errors in King County — Year Built: {frame_time}") +
  transition_time(yr_built) +
  ease_aes("linear")

animation <- animate(animated_plot,
                     fps = 3)

# ---------------- PROBLEM 3 ---------------- 

# old function from lab 6
price_prediction_error <- function(price, bedrooms, bathrooms, sqft_living, 
                                   sqft_lot, grade, yr_built) {
  
  house_info <- data.frame(price, bedrooms, bathrooms, sqft_living, sqft_lot, grade, yr_built)
  rows <- nrow(house_info)
  f <- 0.6
  
  perm <- house_info[sample(rows), ]
  train.dat <- perm[1:floor(f * rows), ]
  test.dat  <- perm[(floor(f * rows) + 1):rows, ]
  
  model <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + grade + yr_built,
              data = train.dat)
  
  pred <- predict(model, newdata = test.dat)
  rmse <- sqrt(mean((test.dat$price - pred)^2))
  
  return(rmse)
}

# grouping by zipcode
data_by_zipcode <- house_data %>%
  group_by(zipcode) %>%
  summarize(
    count = n(),
    med_price = median(price),
    med_yr_built = median(yr_built),
    mean_lat = mean(lat),
    mean_long = mean(long),
    error = price_prediction_error(price, bedrooms, bathrooms, sqft_living,
    sqft_lot, grade, yr_built)
  )

# Compute percent error representation:
data_by_zipcode$percent_error <- data_by_zipcode$error


# filtering county
county_info <- map_data("county", region = "washington")
king_county_map <- county_info %>% filter(subregion == "king")

# static plot
static_plot <- ggplot() +
  geom_polygon(data = king_county_map,
               aes(x = long, y = lat, group = group),
               fill = "white", color = "black") +
  geom_point(data = data_by_zipcode,
             aes(x = mean_long, y = mean_lat,
                 color = percent_error,
                 size = med_price),
             alpha = 0.8) +
  scale_color_gradient(low = "green", high = "red",
                       name = "Percent Error") +
  scale_size_continuous(name = "Median Price") +
  coord_quickmap() +
  theme_void() +
  ggtitle("Prediction Error by Zipcode (King County)")


animated_plot <- ggplot() +
  geom_polygon(data = king_county_map,
               aes(x = long, y = lat, group = group),
               fill = "white", color = "black") +
  geom_point(data = data_by_zipcode,
             aes(x = mean_long, y = mean_lat,
                 color = percent_error,
                 size = med_price,
                 frame = med_yr_built),
             alpha = 0.8) +
  scale_color_gradient(low = "green", high = "red") +
  scale_size_continuous(range = c(2, 10)) +
  coord_quickmap() +
  theme_void() +
  labs(title = "Zipcode Errors — Median Year Built: {frame_time}",
       color = "Percent Error",
       size = "Median Price") +
  transition_time(med_yr_built) +
  ease_aes("linear")


animation <- animate(animated_plot, fps = 2, width = 900, height = 700)
anim_save("zipcode_error_animation.gif", animation)



correlation <- cor(data_by_zipcode$med_price,
                   data_by_zipcode$percent_error)

# Very strong correlation between the median price of the neighborhood and the error




