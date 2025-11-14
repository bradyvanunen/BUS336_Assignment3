honda <- read.csv("data/Honda_Sales.csv")
head(honda); str(honda); names(honda)

honda <- honda %>% mutate(
  Year  = as.integer(Year),
  Month = as.integer(Month)
)


honda <- honda %>%
  mutate(Year = ifelse(Year < 100, 2000 + Year, Year))

unique(honda$Year)  

train <- honda %>% filter(Year <= 2012)
test  <- honda %>% filter(Year >= 2013 & Year <= 2014)

n_total   <- nrow(honda)
pct_train <- nrow(train) / n_total * 100
pct_test  <- nrow(test)  / n_total * 100

pct_train; pct_test
range(train$Year); range(test$Year)

rmse <- function(actual, pred) sqrt(mean((actual - pred)^2))
mae  <- function(actual, pred) mean(abs(actual - pred))
mape <- function(actual, pred) mean(abs((actual - pred) / actual)) * 100

model1 <- lm(Sales ~ Unemployment + CPI_All + CPI_Energy + Queries, data = train)
summary(model1)

sig90_m1 <- sum(coef(summary(model1))[-1, 4] < 0.10)  
r2_m1    <- summary(model1)$r.squared

sig90_m1
r2_m1

model2 <- lm(Sales ~ Unemployment + CPI_All + CPI_Energy + Queries + Year + Month,
             data = train)

summary(model2)

train <- train %>%
  mutate(Month_Factor = as.factor(Month))

model3 <- lm(Sales ~ Unemployment + CPI_All + CPI_Energy + Queries +
               Year + Month_Factor,
             data = train)

summary(model3)

sig90_m3 <- sum(coef(summary(model3))[-1, 4] < 0.10)
r2_m3    <- summary(model3)$r.squared

sig90_m3
r2_m3

bestmodel <- step(model3, direction = "both", trace = FALSE)
summary(bestmodel)

sig90_best <- sum(coef(summary(bestmodel))[-1, 4] < 0.10)
r2_best    <- summary(bestmodel)$r.squared

sig90_best
r2_best

summary(bestmodel)

train_pred <- predict(bestmodel, newdata = train)

rmse_train <- rmse(train$Sales, train_pred)
mae_train  <- mae(train$Sales, train_pred)
mape_train <- mape(train$Sales, train_pred)

rmse_train
mae_train
mape_train
