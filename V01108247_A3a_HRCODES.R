# Load necessary libraries
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)
library(e1071)
library(dplyr)
library(ggplot2)

#setting the wd
setwd('C:\\Users\\SPURGE\\Desktop\\SCMA')
getwd()

# Load the dataset
data <- read.csv("HR_DataSet.csv")

# Encode categorical variables
data$salary <- as.factor(data$salary)
data$Department <- as.factor(data$Department)

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data$left, p = 0.8, list = FALSE)
trainData <- data[trainIndex,]
testData <- data[-trainIndex,]


# Logistic Regression
logit_model <- glm(left ~ ., data = trainData, family = binomial)

# Predict on the test data
logit_pred <- predict(logit_model, newdata = testData, type = "response")
logit_pred_class <- ifelse(logit_pred > 0.5, 1, 0)

# Confusion Matrix
confusionMatrix(as.factor(logit_pred_class), as.factor(testData$left))


# ROC Curve
roc_curve <- roc(testData$left, logit_pred)
plot(roc_curve)
auc(roc_curve)


# Decision Tree
tree_model <- rpart(left ~ ., data = trainData, method = "class")
rpart.plot(tree_model)


# Predict on the test data
tree_pred <- predict(tree_model, newdata = testData, type = "class")

# Confusion Matrix
confusionMatrix(tree_pred, as.factor(testData$left))


# Compare models
logit_roc <- roc(testData$left, as.numeric(logit_pred_class))
tree_roc <- roc(testData$left, as.numeric(as.character(tree_pred)))


plot(logit_roc, col = "blue")
plot(tree_roc, add = TRUE, col = "red")
legend("bottomright", legend = c("Logistic Regression", "Decision Tree"), col = c("blue", "red"), lwd = 2)

# Create a correlation matrix
correlation_matrix <- cor(select(data, -left), use = "complete.obs")

# Create a heatmap
ggcorrplot(correlation_matrix, 
           method = "circle", 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           colors = c("red", "white", "blue"), 
           title = "Correlation Matrix Heatmap", 
           ggtheme = theme_minimal())
