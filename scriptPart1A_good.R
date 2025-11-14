# BUS 336 - Assignment 3 (Part 1A)
# Steven Duong
# Darian Sidhu
# Brady Van Unen

library(tidyverse)


honda <- read.csv("data/Honda_Sales.csv")


head(honda); str(honda); names(honda)


honda <- honda %>%
  mutate(
    Year  = as.integer(Year),
    Month = as.integer(Month)
  )


honda <- honda %>%
  mutate(Year = ifelse(Year < 100, 2000 + Year, Year))

# Sanity check
unique(honda$Year)     # should show 2010..2014

# 3) Split: Train ≤ 2012; Test = 2013–2014 
train <- honda %>% filter(Year <= 2012)
test  <- honda %>% filter(Year >= 2013 & Year <= 2014)


n_total   <- nrow(honda)
pct_train <- nrow(train) / n_total * 100
pct_test  <- nrow(test)  / n_total * 100

round(pct_train, 1); round(pct_test, 1)  
range(train$Year); range(test$Year)     


train_plot <- train %>%
  arrange(Year, Month) %>%
  select(Year, Month, Sales) %>%
  mutate(YearMonth = paste(Year, sprintf("%02d", Month), sep = "-"))


dir.create("tableau_exports", showWarnings = FALSE)

write.csv(train_plot,
          "tableau_exports/train_sales_for_tableau.csv",
          row.names = FALSE)
getwd()
list.files(recursive = TRUE)

list.files(recursive = TRUE)
list.dirs()
write.csv(train_plot,
          "tableau_exports/train_sales_for_tableau.csv",
          row.names = FALSE)
train_preds_df <- data.frame(
  Year = train$Year,
  Month = train$Month,
  Actual = train$Sales,
  Predicted = train_pred
)

write.csv(train_preds_df,
          "tableau_exports/train_actual_vs_predicted.csv",
          row.names = FALSE)
