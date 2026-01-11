library(ggplot2)

# Reading the data
raw_data <- read.csv("C:/Users/Hamza/Downloads/HAB-FlatFile-Weekly-2025-P1-P11.csv", header = TRUE)

# Remove rows with NA values
dat <- na.omit(raw_data)

# Check dimensions
cat("Original rows:", nrow(raw_data), "\n")
cat("Clean rows:", nrow(dat), "\n")

# Inspect structure
str(dat)
summary(dat)

# Visualization prior to modeling 
numeric_vars <- sapply(dat, is.numeric)
numeric_data <- dat[, numeric_vars]

# Basic scatterplot matrix for numeric variables
# pairs(dat[, numeric_vars],
#       main = "Pairwise Scatterplot Matrix of Numeric Variables",
#       pch = 19, col = "blue", cex = 0.5)

cor_matrix <- cor(dat[, numeric_vars])
round(cor_matrix, 2)

# Visual heatmap
# heatmap(cor_matrix, symm = TRUE, col=terrain.colors(10), main="Correlation Heatmap")

table(dat$Report)     # Only 1 unique value - REMOVE
table(dat$Timeframe)  # Only 1 unique value - REMOVE
table(dat$Type)       # Only 2 unique value - REMOVE
table(dat$Geography)
table(dat$Current.Year.Week.Ending) # Remove for simplicity

# Prepare Categorical variable for modelling
dat$Geography <- as.factor(dat$Geography)

# test - train split
set.seed(1234)
rows <- nrow(dat)
f <- 0.65
perm <- dat[sample(rows), ]
train.dat <- perm[1:floor(f * rows), ]
test.dat  <- perm[(floor(f * rows) + 1):rows, ]

# Linear model
base.lm <- lm(ASP.Current.Year ~ Period + Geography + Total.Bulk.and.Bags + X4046  + X4225 + X4770 
               + TotalBagged + Sml.Bagged + Lrg.Bagged + X.Lrg.Bagged + Unknown.Bag.Size, data = train.dat)
summary(base.lm)
pred <- predict(base.lm, newdata = test.dat)
# par(mfrow=c(2,2))
# plot(base.lm)
# par(mfrow=c(1,1))

# Evaluate model
rmse <- sqrt(mean((test.dat$ASP.Current.Year - pred)^2))
SST <- sum((test.dat$ASP.Current.Year - mean(test.dat$ASP.Current.Year))^2)
SSE <- sum((test.dat$ASP.Current.Year - pred)^2)
R2_test <- 1 - SSE/SST
cat("Test RMSE:", rmse, "\n")
cat("Test R-squared:", R2_test, "\n")

# Linear model after backward elimination
model <- lm(ASP.Current.Year ~ Period + Total.Bulk.and.Bags + X4046  + X4225 
              + Sml.Bagged + Lrg.Bagged , data = train.dat)
summary(model)
pred <- predict(model, newdata = test.dat)

par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))

# Evaluate model
rmse <- sqrt(mean((test.dat$ASP.Current.Year - pred)^2))
SST <- sum((test.dat$ASP.Current.Year - mean(test.dat$ASP.Current.Year))^2)
SSE <- sum((test.dat$ASP.Current.Year - pred)^2)
R2_test <- 1 - SSE/SST

cat("Test RMSE:", rmse, "\n")
cat("Test R-squared:", R2_test, "\n")

# Build a unique model for each geographical region
geographies <- unique(train.dat$Geography)
models_list <- list()
predictions_list <- list()
performance_metrics <- data.frame(
  Geography = character(),
  R2_train = numeric(),
  RMSE_train = numeric(),
  R2_test = numeric(),
  RMSE_test = numeric(),
  n_train = integer(),
  n_test = integer(),
  stringsAsFactors = FALSE
)

for(geo in geographies) {
  # Subset data for this region
  train_subset <- train.dat[train.dat$Geography == geo, ]
  test_subset <- test.dat[test.dat$Geography == geo, ]

  # Skip if too few observations
  if(nrow(train_subset) < 10 || nrow(test_subset) < 5) {
    cat("Skipping", geo, "- insufficient data\n")
    next
  } # No such region skipped

  # Build model for this region using variable identifed in earlier exercise
  model_geo <- lm(ASP.Current.Year ~ Period + Total.Bulk.and.Bags +
                    X4046 + X4225 + Sml.Bagged + Lrg.Bagged,
                  data = train_subset)

  # Store model
  models_list[[geo]] <- model_geo

  # Store predictions
  train_pred <- predict(model_geo, newdata = train_subset)
  test_pred <- predict(model_geo, newdata = test_subset)

  # Calculate metrics
  # Training
  train_rmse <- sqrt(mean((train_subset$ASP.Current.Year - train_pred)^2))
  SST_train <- sum((train_subset$ASP.Current.Year - mean(train_subset$ASP.Current.Year))^2)
  SSE_train <- sum((train_subset$ASP.Current.Year - train_pred)^2)
  R2_train <- 1 - SSE_train/SST_train

  # Testing
  test_rmse <- sqrt(mean((test_subset$ASP.Current.Year - test_pred)^2))
  SST_test <- sum((test_subset$ASP.Current.Year - mean(test_subset$ASP.Current.Year))^2)
  SSE_test <- sum((test_subset$ASP.Current.Year - test_pred)^2)
  R2_test <- 1 - SSE_test/SST_test

  # Store performance
  performance_metrics <- rbind(performance_metrics, data.frame(
    Geography = geo,
    R2_train = R2_train,
    RMSE_train = train_rmse,
    R2_test = R2_test,
    RMSE_test = test_rmse,
    n_train = nrow(train_subset),
    n_test = nrow(test_subset)
  ))

  cat("Geography:", geo,
      "Train R2:", round(R2_train, 3),
      "Test R2:", round(R2_test, 3),
      "\n")
}

# Sort by test R2 to see best performing geographies
performance_metrics_sorted <- performance_metrics[order(-performance_metrics$R2_test), ]
print(performance_metrics_sorted)

# Make Geography an ordered factor by R2_test
performance_metrics_sorted$Geography <- factor(performance_metrics_sorted$Geography,
                                               levels = performance_metrics_sorted$Geography[order(performance_metrics_sorted$R2_test)]
)

cat("\n=== Performance Summary ===\n")
cat("Average Test R-squared:", round(mean(performance_metrics$R2_test), 3), "\n")
cat("Average Test RMSE:", round(mean(performance_metrics$RMSE_test), 3), "\n")
cat("Range of Test R-squared:", range(performance_metrics$R2_test), "\n")
cat("Range of Test R-RMSE:", range(performance_metrics$RMSE_test), "\n")


# Train VS Test Comparison of R2 and RMSE, to check for overfitting
ggplot(performance_metrics_sorted, aes(x = R2_train, y = R2_test, label = Geography)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Train vs Test R-squared per Geography",
       x = "Training R²", y = "Test R²") +
  theme_minimal(base_size = 13)

ggplot(performance_metrics_sorted, aes(x = RMSE_train, y = RMSE_test, label = Geography)) +
  geom_point(color = "orange", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "red") +
  labs(title = "Train vs Test RMSE per Geography",
       x = "Training RMSE", y = "Test RMSE") +
  theme_minimal(base_size = 13)

# R2 and RMSE test histograms to see general prediction spread
# par(mfrow = c(1, 2))
hist(performance_metrics$R2_test, breaks = 15, col = "lightblue",
     main = "Test R² Distribution", xlab = "Test R²")
hist(performance_metrics$RMSE_test, breaks = 15, col = "lightgreen",
     main = "Test RMSE Distribution", xlab = "Test RMSE")

