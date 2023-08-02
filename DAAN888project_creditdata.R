library(readxl)
creditdata_clean <- read_excel("C:/Users/jeepu/OneDrive/Desktop/Data analytics penn state/DAAN 888 - Design and Implementation of Analytics Systems/Project/creditdata_clean.xlsx")
View(creditdata_clean)

library(caret)
library(rpart)
library(e1071)
library(pROC)


##Eliminating few variables
df <- data.frame(creditdata_clean)
#The following code is our column deletion, it contains all the variables we agreed as a group that we do not need. Some of these are duplicates or derivatives of other variables. Some only have one value for the entire column.
df <- subset(df, select = -c(time_since_bankrupt,num_ccj,time_since_ccj,ccj_amount,arrears_months,region,
                             origination_date,maturity_date,arrears_status,arrears_event,term_expiry_event,
                             worst_arrears_status,months_since_2mia,avg_mia_6m, max_arrears_bal_6m,max_mia_6m))
df <- data.frame(df)
ncol(df)

## Convert target variable type
##For this model, the target variable should be a factor. For predictors, this model handles
##non-numeric data of some types (such as character, factor and ordered data).

df$default_event <- factor(df$default_event)


##Train / test split
sampling_vector <- createDataPartition(df$default_event, p = 0.80, list = FALSE)
train_reg <- df[sampling_vector,]
View(train_reg)

test_reg <- df[-sampling_vector,]
View(test_reg)

##Classification Tree with rpart
CARTTree <- rpart(default_event ~., method = "class", data = train_reg)
summary(CARTTree)
CARTTree$variable.importance
CARTTree$cptable

##Plot Tree
plot(CARTTree, uniform = TRUE, main = "Classification of creditdata")
text(CARTTree, use.n = TRUE, all = TRUE, cex = .6)

##Assess performance

train_predicted <- predict(CARTTree, newdata = train_reg, type = "class")
mean(train_reg$default_event == train_predicted)
table(actural = train_reg$default_event, predict = train_predicted)

test_predicted <- predict(CARTTree, newdata = test_reg, type = "class")
mean(test_reg$default_event == test_predicted)
table(actural = test_reg$default_event, predict = test_predicted)

# Training - Calculate AUC and ROC
# Create ROC curve
train_roc <- roc(response = train_reg$default_event, predictor = factor(train_predicted, ordered = TRUE), plot=TRUE)
plot(train_roc, col="red", lwd=3, main="Training - ROC curve")
auc(train_reg$default_event,predictor = factor(train_predicted, ordered = TRUE))

# Testing - Calculate AUC and ROC
# Create ROC Curve
test_roc <- roc(response = test_reg$default_event, predictor = factor(test_predicted, ordered = TRUE), plot=TRUE)
plot(test_roc, col="red", lwd=3, main="Testing - ROC curve")
auc(test_reg$default_event,predictor = factor(test_predicted, ordered = TRUE))

