# BUS 336 - Assignment 3 (Part 1B)
# Steven Duong
# Darian Sidhu
# Brady Van Unen

# -----------------------------------------------------------------------------
# Load data and perform initial checks
# -----------------------------------------------------------------------------
honda <- read.csv("data/Honda_Sales.csv")
head(honda); str(honda); names(honda)

# Convert Year and Month to integers
honda <- honda %>% mutate(
  Year  = as.integer(Year),
  Month = as.integer(Month)
)

# Fix 2-digit year formatting
honda <- honda %>%
  mutate(Year = ifelse(Year < 100, 2000 + Year, Year))

# Confirm year range
unique(honda$Year)

# -----------------------------------------------------------------------------
# Split into training (≤2012) and testing (2013–2014) sets
# -----------------------------------------------------------------------------
train <- honda %>% filter(Year <= 2012)
test  <- honda %>% filter(Year >= 2013 & Year <= 2014)

# Compute percentages for reporting
n_total   <- nrow(honda)
pct_train <- nrow(train) / n_total * 100
pct_test  <- nrow(test)  / n_total * 100

# Print percentages and year ranges
pct_train; pct_test
range(train$Year); range(test$Year)

# -----------------------------------------------------------------------------
# Define error metric functions: RMSE, MAE, MAPE
# -----------------------------------------------------------------------------
rmse <- function(actual, pred) sqrt(mean((actual - pred)^2))
mae  <- function(actual, pred) mean(abs(actual - pred))
mape <- function(actual, pred) mean(abs((actual - pred) / actual)) * 100

# -----------------------------------------------------------------------------
# Model 1: Baseline model using economic indicators + Google Queries
# -----------------------------------------------------------------------------
model1 <- lm(Sales ~ Unemployment + CPI_All + CPI_Energy + Queries, data = train)
summary(model1)

# Count number of significant predictors at 90% confidence (p < 0.10)
sig90_m1 <- sum(coef(summary(model1))[-1, 4] < 0.10)

# Extract R-squared for model comparison
r2_m1 <- summary(model1)$r.squared

sig90_m1
r2_m1

# -----------------------------------------------------------------------------
# Model 2: Add time variables (Year and Month)
# -----------------------------------------------------------------------------
model2 <- lm(Sales ~ Unemployment + CPI_All + CPI_Energy + Queries + Year + Month,
             data = train)
summary(model2)

# -----------------------------------------------------------------------------
# Model 3: Treat Month as a categorical variable (factor) for seasonality
# -----------------------------------------------------------------------------
train <- train %>%
  mutate(Month_Factor = as.factor(Month))

model3 <- lm(Sales ~ Unemployment + CPI_All + CPI_Energy + Queries +
               Year + Month_Factor,
             data = train)

summary(model3)

# Count significant predictors and get R²
sig90_m3 <- sum(coef(summary(model3))[-1, 4] < 0.10)
r2_m3    <- summary(model3)$r.squared

sig90_m3
r2_m3

# -----------------------------------------------------------------------------
# Stepwise model selection (AIC) to choose the best subset of predictors
# -----------------------------------------------------------------------------
bestmodel <- step(model3, direction = "both", trace = FALSE)
summary(bestmodel)

# Count significant predictors + R² of best model
sig90_best <- sum(coef(summary(bestmodel))[-1, 4] < 0.10)
r2_best    <- summary(bestmodel)$r.squared

sig90_best
r2_best

summary(bestmodel)

# -----------------------------------------------------------------------------
# Predict on training data and compute error metrics
# -----------------------------------------------------------------------------
train_pred <- predict(bestmodel, newdata = train)

rmse_train <- rmse(train$Sales, train_pred)
mae_train  <- mae(train$Sales, train_pred)
mape_train <- mape(train$Sales, train_pred)

rmse_train
mae_train
mape_train

# -----------------------------------------------------------------------------
# Predict on test data
# Month_Factor needs same levels as training set
# -----------------------------------------------------------------------------
test$Month_Factor <- factor(test$Month, levels = levels(train$Month_Factor))

test_pred <- predict(bestmodel, newdata = test)

# Compute test performance metrics
rmse_test <- rmse(test$Sales, test_pred)
mae_test  <- mae(test$Sales, test_pred)
mape_test <- mape(test$Sales, test_pred)

rmse_test
mae_test
mape_test

# -----------------------------------------------------------------------------
# Compute Test R-squared manually
# -----------------------------------------------------------------------------
SSE_test <- sum((test$Sales - test_pred)^2)
SST_test <- sum((test$Sales - mean(test$Sales))^2)
r2_test  <- 1 - SSE_test / SST_test

r2_test
