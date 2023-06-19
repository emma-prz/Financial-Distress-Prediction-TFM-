# Load necessary libraries
library(randomForest)
library(pROC)
library(tidyverse)
library(caret)

load("../Datos/imputed_data.RData") 

# Initialize variables to store the best model and its ROC AUC
best_model <- NULL
best_avg_roc_auc <- 0
best_mtry <- NULL
best_min_node_size <- NULL
best_ntree <- NULL

# Values for mtry, min.node.size and ntree to perform grid search
mtry_values <- c(2, 4, 6)
min_node_size_values <- c(1, 3, 5)
ntree_values <- c(200, 300, 500)

# Perform temporal cross-validation and hyperparameter tuning
for(mtry_val in mtry_values) {
  for(min_node_size_val in min_node_size_values) {
    for(ntree_val in ntree_values) {
      
      # Initialize a vector to store the ROC AUC for each year
      yearly_roc_auc <- numeric()
      
      for(year in 2015:2018) {
        
        # Split data into training and testing subsets
        train_data <- model_sample[model_sample$year < year, ] %>% select(-year)
        test_data <- model_sample[model_sample$year == year, ] %>% select(-year)
        
        # Train the model (random forest) using tuning
        model <- randomForest(
          formula = distressed ~ .,
          mtry = mtry_val,
          ntree = ntree_val,
          min.node.size= min_node_size_val,
          data = train_data)
        
        # Make predictions on the test data
        predicted_probabilities <- predict(model, newdata = test_data, type = "prob")[, 2]
        
        # Calculate the ROC AUC of the model and store it
        yearly_roc_auc <- c(yearly_roc_auc, roc(test_data$distressed, predicted_probabilities)$auc)
      }
      
      # Calculate the average ROC AUC across all years
      avg_roc_auc <- mean(yearly_roc_auc)
      
      # Update the best model if the current model has a higher average ROC AUC
      if (avg_roc_auc > best_avg_roc_auc) {
        best_model <- model
        best_avg_roc_auc <- avg_roc_auc
        best_mtry <- mtry_val
        best_min_node_size <- min_node_size_val
        best_ntree <- ntree_val
      }
    }
  }
}

# Print the best model and its average ROC AUC
print(best_model)
print(best_avg_roc_auc)
print(best_mtry)
print(best_min_node_size)
print(best_ntree)

# Validation set 
rfProb = predict(best_model, test_data, type="prob")
roc.rf=roc( test_data$distressed, rfProb[,2])

plot.roc(roc.rf, col="darkblue", print.auc = TRUE,  auc.polygon=TRUE, grid=c(0.1, 0.2),
         grid.col=c("green", "red"), max.auc.polygon=TRUE,
         auc.polygon.col="lightblue", print.thres=TRUE, main = "RF - Matched 2018")

prediction <- as.factor(ifelse(rfProb[,2] > 0.163, "yes", "no"))
levels(test_data$distressed) <- c("no", "yes")

# Create the confusion matrix
confusionMatrix(prediction, test_data$distressed, positive = "yes")


# Testing set 
test_2019 <- model_sample %>% filter(year == 2019) %>% select(-year)

rfProb = predict(best_model, test_2019, type="prob")
roc.rf=roc( test_2019$distressed, rfProb[,2])

prediction <- as.factor(ifelse(rfProb[,2] > 0.163, "yes", "no"))
levels(test_2019$distressed) <- c("no", "yes")

# Create the confusion matrix
confusionMatrix(prediction, test_2019$distressed, positive = "yes")

metrics_rf_match <- confusionMatrix(prediction, test_2019$distressed, positive = "yes")$byClass[1:4] %>% enframe()

plot.roc(roc.rf, col="darkblue", print.auc = TRUE,  auc.polygon=TRUE, grid=c(0.1, 0.2),
         grid.col=c("green", "red"), max.auc.polygon=TRUE,
         auc.polygon.col="lightblue", print.thres=TRUE, main = "RF - Matched 2019")

# Importance of variables
varImpPlot(best_model, type = 2, main = "Variable importance Random Forest")

# All 2019 
rfProb = predict(best_model, full_2019_imputed, type="prob")

prediction <- as.factor(ifelse(rfProb[,2] > 0.163, "yes", "no"))

#full_2019_imputed$distressed <- as.factor(full_2019_imputed$distressed)
levels(full_2019_imputed$distressed) <- c("no", "yes")

confusionMatrix(prediction, full_2019_imputed$distressed, positive = "yes")

metrics_rf_full <- confusionMatrix(prediction, full_2019_imputed$distressed, positive = "yes")$byClass[1:4] %>% enframe()

roc.rf=roc(full_2019_imputed$distressed, rfProb[,2])
plot.roc(roc.rf, col="darkblue", print.auc = TRUE,  auc.polygon=TRUE, grid=c(0.1, 0.2),
         grid.col=c("green", "red"), max.auc.polygon=TRUE,
         auc.polygon.col="lightblue", print.thres=TRUE, main = "RF - Full 2019")



