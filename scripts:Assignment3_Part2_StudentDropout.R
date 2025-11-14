# BUS 336 - Assignment 3
# Part 2: Logistic Regression - Student Dropout
# Names: Darian, Steven, and Brady
# Date: November 13th, 2025

library(caTools)
library(ROCR)

# Load in data
data <- read.csv("Student_Dropout.csv")

# Q1: How many stayed vs dropped out
prop.table(table(data$Dropout))
nrow(data)                 
table(data$Dropout)

# Q2: Split 75/25 + build model by given variables
set.seed(123)
split <- sample.split(data$Dropout, SplitRatio = 0.75)
train <- subset(data, split == TRUE)
test  <- subset(data, split == FALSE)

model <- glm(Dropout ~ Gender + BirthYear + Major + GPA +
               CreditsCompleted + PartTime + Scholarship,
             data = train, family = binomial)

# -------------------------------------------------------------------------


summary(model)   # Q3 significant/insignificant

# Q4: GPA coefficient & odds ratio 
coef(model)["GPA"]
exp(coef(model)["GPA"])

# Q5: Probability for given student in question 
new_student <- data.frame(
  Gender="Male",
  BirthYear=2002,
  Major="BUS",
  GPA=3.1,
  CreditsCompleted=100,
  PartTime="No",
  Scholarship="Yes"
)

predict(model, newdata=new_student, type="response")

#  PART 2B: THE TRAINING SET 

# Q1: Confusion matrix = 0.5 
pred_train <- predict(model, type="response")
class_0.5 <- ifelse(pred_train > 0.5, 1, 0)
cm_0.5 <- table(train$Dropout, class_0.5)
cm_0.5

acc_0.5  <- sum(diag(cm_0.5)) / sum(cm_0.5)
sens_0.5 <- cm_0.5[2,2] / (cm_0.5[2,1] + cm_0.5[2,2])
spec_0.5 <- cm_0.5[1,1] / (cm_0.5[1,1] + cm_0.5[1,2])
acc_0.5; sens_0.5; spec_0.5

# Q2: ROC + AUC Curves
ROCRpred <- prediction(pred_train, train$Dropout)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, main="ROC - Training")
auc_train <- performance(ROCRpred, "auc")
auc_train
auc_train@y.values

# Choose threshold 
t_best <- 0.30

# Q3: Metrics at best threshold 
class_best <- ifelse(pred_train > t_best, 1, 0)
cm_best <- table(train$Dropout, class_best)
cm_best

acc_best  <- sum(diag(cm_best)) / sum(cm_best)
sens_best <- cm_best[2,2] / (cm_best[2,1] + cm_best[2,2])
spec_best <- cm_best[1,1] / (cm_best[1,1] + cm_best[1,2])
acc_best; sens_best; spec_best

#       PART 2C: TEST SET 
# Q1: Test-set confusion matrix 
pred_test <- predict(model, newdata=test, type="response")
class_test <- ifelse(pred_test > t_best, 1, 0)
cm_test <- table(test$Dropout, class_test)
cm_test

acc_test  <- sum(diag(cm_test)) / sum(cm_test)
sens_test <- cm_test[2,2] / (cm_test[2,1] + cm_test[2,2])
spec_test <- cm_test[1,1] / (cm_test[1,1] + cm_test[1,2])
acc_test; sens_test; spec_test

# Q2: Baseline comparison 
baseline_pred <- rep(0, nrow(test))
acc_baseline <- sum(test$Dropout == baseline_pred) / nrow(test)
acc_baseline

# Q3: Test-set ROC + AUC Curves
ROCRpred_test <- prediction(pred_test, test$Dropout)
ROCRperf_test <- performance(ROCRpred_test, "tpr", "fpr")
plot(ROCRperf_test, main="ROC - Test")
auc_test <- performance(ROCRpred_test, "auc")
auc_test
auc_test@y.values


