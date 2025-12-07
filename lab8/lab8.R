library(ggplot2)

# Reading the data
raw_data <- read.csv("C:/Users/Hamza/Documents/UMN-EE-coursework/Fall 2025/EE 5373/lab8/financial_risk_assessment.csv")

# Remove all rows with any NA values
dat <- na.omit(raw_data)

# Check the dimensions to see how many rows were removed
cat("Original rows:", nrow(raw_data), "\n")                  # 15000
cat("Clean rows:", nrow(dat), "\n")               # 5716
cat("Rows removed:", nrow(dat) - nrow(raw_data), "\n")

# Check structure of cleaned data
str(dat)
summary(dat)

# Sanity checking: from the summary I can tell: 
#  1. Age min = 18, max = 69 - reasonable
#  2. Income is between 20k and 120K
#  3. Credit score is between 600 and 799
#  4. Loan amount is between 5K to 50K
#  5. No negative years at job, dependents, previous default, Assets value
#  6. All categorical variable are consitent, no ungrouped categories or entries.
#  7. No duplicated data
#  8. 56 cases removed reported having started working at an age of 3 or younger

sum(duplicated(dat))

sum(dat$Years.at.Current.Job > dat$Age)

# impossible cases where years at job is greater than the person's age
impossible_cases <- dat[dat$Years.at.Current.Job > (dat$Age - 3), ]
print(impossible_cases)
nrow(impossible_cases) # 56 such cases

# Removing these cases
dat <- dat[dat$Years.at.Current.Job <= (dat$Age - 3), ]

# Confirming the change
impossible_cases <- dat[dat$Years.at.Current.Job > (dat$Age - 3), ]
print(impossible_cases)
nrow(impossible_cases)


# Identify numeric and categorical variables
numeric_vars <- names(dat)[sapply(dat, is.numeric)]
cat_vars     <- names(dat)[sapply(dat, function(x) is.factor(x) | is.character(x))]

for (var in numeric_vars) {
  print(
    ggplot(dat, aes_string(var)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "black") +
      theme_minimal() +
      labs(title = paste("Histogram of", var))
  )
}

# Concerns after Numeric visualization:
# 1. Marital.Status.Change has only 3 unique values
# 2. Number.of.Dependents & Previous.Defaults has only 4 unique values

library(ggplot2)

# Reading the data
raw_data <- read.csv("C:/Users/Hamza/Documents/UMN-EE-coursework/Fall 2025/EE 5373/lab8/financial_risk_assessment.csv")

# Remove all rows with any NA values
dat <- na.omit(raw_data)

# Check the dimensions to see how many rows were removed
cat("Original rows:", nrow(raw_data), "\n")                  # 15000
cat("Clean rows:", nrow(dat), "\n")               # 5716
cat("Rows removed:", nrow(dat) - nrow(raw_data), "\n")

# Check structure of cleaned data
str(dat)
summary(dat)

# Sanity checking: from the summary I can tell: 
#  1. Age min = 18, max = 69 - reasonable
#  2. Income is between 20k and 120K
#  3. Credit score is between 600 and 799
#  4. Loan amount is between 5K to 50K
#  5. No negative years at job, dependents, previous default, Assets value
#  6. All categorical variable are consitent, no ungrouped categories or entries.
#  7. No duplicated data
#  8. 56 cases removed reported having started working at an age of 3 or younger

sum(duplicated(dat))

sum(dat$Years.at.Current.Job > dat$Age)

# impossible cases where years at job is greater than the person's age
impossible_cases <- dat[dat$Years.at.Current.Job > (dat$Age - 3), ]
print(impossible_cases)
nrow(impossible_cases) # 56 such cases

# Removing these cases
dat <- dat[dat$Years.at.Current.Job <= (dat$Age - 3), ]

# Confirming the change
impossible_cases <- dat[dat$Years.at.Current.Job > (dat$Age - 3), ]
print(impossible_cases)
nrow(impossible_cases)


# Identify numeric and categorical variables
numeric_vars <- names(dat)[sapply(dat, is.numeric)]
cat_vars     <- names(dat)[sapply(dat, function(x) is.factor(x) | is.character(x))]

# Set up 3x4 grid (12 slots)
par(mfrow = c(3, 4), mar = c(4, 4, 2, 1))

for (var in numeric_vars) {
  hist(dat[[var]], 
       main = var,
       xlab = "",
       col = "steelblue",
       border = "black",
       breaks = 30,
       cex.main = 0.9)
}

# Reset to default
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

# Concerns after Numeric visualization:
# 1. Marital.Status.Change has only 3 unique values
# 2. Number.of.Dependents & Previous.Defaults has only 4 unique values

# CORRELATION MATRIX FOR NUMERIC VARIABLES 
numeric_data <- dat[, numeric_vars]
cor_matrix <- cor(numeric_data)
print(round(cor_matrix, 3))
# Comments: Very low abs(correlation) (< 0.3) between all numerical variables. 

# CORRELATION FOR CATEGORCIAL VARIABLES 

cat_data <- dat[, cat_vars]

# Cramer's V function
cramers_v <- function(x, y) {
  tbl <- table(x, y)
  chi <- suppressWarnings(chisq.test(tbl, correct = FALSE)$statistic)
  n <- sum(tbl)
  r <- nrow(tbl)
  k <- ncol(tbl)
  sqrt(chi / (n * (min(r - 1, k - 1))))
}

# Compute Cramer's V matrix
m <- length(cat_vars)
cram_matrix <- matrix(NA, nrow = m, ncol = m)
rownames(cram_matrix) <- cat_vars
colnames(cram_matrix) <- cat_vars

for (i in 1:m) {
  for (j in 1:m) {
    cram_matrix[i, j] <- cramers_v(cat_data[[i]], cat_data[[j]])
  }
}

# Convert to long format for ggplot
cram_df <- data.frame(
  Var1 = rep(cat_vars, times = m),
  Var2 = rep(cat_vars, each = m),
  Value = as.vector(cram_matrix)
)

# Plot heatmap for Categorical
ggplot(cram_df, aes(x = Var1, y = Var2, fill = Value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Value, 2)), size = 3) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Cramer's V - Categorical", fill = "V") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Comments: 
# Very high multi-collinearity between City-State-Country
# City had high correlation with every variable. MUST remove City in lm
