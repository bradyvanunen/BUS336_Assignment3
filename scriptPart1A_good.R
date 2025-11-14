# BUS 336 - Assignment 3 (Part 1A)
# Steven Duong
# Darian Sidhu
# Brady Van Unen

library(tidyverse)

# -----------------------------------------------------------------------------
# Load the Honda sales dataset
# -----------------------------------------------------------------------------
honda <- read.csv("data/Honda_Sales.csv")

# Quick exploratory checks: first rows, structure, and variable names
head(honda); str(honda); names(honda)

# -----------------------------------------------------------------------------
# Convert Year and Month columns to integers to ensure correct numeric formatting
# -----------------------------------------------------------------------------
honda <- honda %>%
  mutate(
    Year  = as.integer(Year),
    Month = as.integer(Month)
  )

# -----------------------------------------------------------------------------
# Fix 2-digit year formatting
# Some datasets store years as "10" instead of "2010".
# This converts any year < 100 to "2000 + Year"
# -----------------------------------------------------------------------------
honda <- honda %>%
  mutate(Year = ifelse(Year < 100, 2000 + Year, Year))

# Sanity check: ensures years are now 2010–2014
unique(honda$Year)

# -----------------------------------------------------------------------------
# 3) Create Train and Test Splits
# Train = years ≤ 2012
# Test  = years 2013–2014
# -----------------------------------------------------------------------------
train <- honda %>% filter(Year <= 2012)
test  <- honda %>% filter(Year >= 2013 & Year <= 2014)

# -----------------------------------------------------------------------------
# Calculate the percentage of observations in Train vs Test
# -----------------------------------------------------------------------------
n_total   <- nrow(honda)
pct_train <- nrow(train) / n_total * 100
pct_test  <- nrow(test)  / n_total * 100

# Display rounded percentages
round(pct_train, 1); round(pct_test, 1)

# Check year ranges in each set
range(train$Year); range(test$Year)

# -----------------------------------------------------------------------------
# Prepare train dataset for Tableau export:
# Create a YYYY-MM column for time-series plots
# -----------------------------------------------------------------------------
train_plot <- train %>%
  arrange(Year, Month) %>%
  select(Year, Month, Sales) %>%
  mutate(YearMonth = paste(Year, sprintf("%02d", Month), sep = "-"))

# -----------------------------------------------------------------------------
# Create export folder if it does not exist
# -----------------------------------------------------------------------------
dir.create("tableau_exports", showWarnings = FALSE)

# Export prepared training data for Tableau visualization
write.csv(train_plot,
          "tableau_exports/train_sales_for_tableau.csv",
          row.names = FALSE)

# -----------------------------------------------------------------------------
# Combine actual vs predicted values for Tableau (train set)
# -----------------------------------------------------------------------------
train_preds_df <- data.frame(
  Year = train$Year,
  Month = train$Month,
  Actual = train$Sales,
  Predicted = train_pred
)

# Export comparison table
write.csv(train_preds_df,
          "tableau_exports/train_actual_vs_predicted.csv",
          row.names = FALSE)
