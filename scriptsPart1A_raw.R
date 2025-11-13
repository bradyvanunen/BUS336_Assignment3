# =====================================================
# BUS 336 - Assignment 
Brady Van Unen, Steven Duong, Darian Sidhu
# Part 1A: Data Loading and Splitting (Honda Civic Sales)
library(tidyverse)
honda <- read.csv("data/Honda_Sales.csv")
head(honda)
str(honda)
names(honda)


  names(honda)
str(honda)
head(honda, 3)

honda <- honda %>%
  mutate(
    Year  = as.integer(Year),
    Month = as.integer(Month)
  )
train <- honda %>% filter(Year <= 2012)
test  <- honda %>% filter(Year >= 2013 & Year <= 2014)

n_total   <- nrow(honda)
pct_train <- nrow(train) / n_total * 100
pct_test  <- nrow(test)  / n_total * 100

unique(honda$Year)

honda <- honda %>%
  mutate(Year = ifelse(Year < 100, 2000 + Year, Year)
         train <- honda %>% dplyr::filter(Year <= 2012)
         test  <- honda %>% dplyr::filter(Year >= 2013 & Year <= 2014)

         n_total   <- nrow(honda)
         pct_train <- nrow(train) / n_total * 100
         pct_test  <- nrow(test)  / n_total * 100         

         pct_train; pct_test
         range(train$Year); range(test$Year)         
         # Split using two-digit year values
         train <- honda %>% dplyr::filter(Year <= 12)          # 10–12 = 2010–2012
         test  <- honda %>% dplyr::filter(Year %in% c(13, 14)) # 13–14 = 2013–2014
         
         # Recompute percentages
         n_total   <- nrow(honda)
         pct_train <- nrow(train) / n_total * 100
         pct_test  <- nrow(test)  / n_total * 100
         
         pct_train; pct_test
         
         range(train$Year); range(test$Year)
         
         train_plot <- train %>%
           arrange(Year, Month) %>%
           select(Year, Month, Sales) %>%
           mutate(YearMonth = paste(Year, sprintf("%02d", Month), sep = "-"))
         
         write.csv(train_plot, "tableau_exports/train_sales_for_tableau.csv", row.names = FALSE)
         
         getwd()
         list.dirs()         
         dir.create("tableau_exports")         
         write.csv(train_plot, "tableau_exports/train_sales_for_tableau.csv", row.names = FALSE)         
         