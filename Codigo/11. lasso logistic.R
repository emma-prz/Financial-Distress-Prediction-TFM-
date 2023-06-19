# Penalized Logistic Regression

################################################################################
# DO NOT RUN THIS CHUNK IF PREVIOUS CODE (10.xgb.R) has been run
# Relevel binary factors

#model_sample$distressed <- as.numeric(model_sample$distressed)-1
#model_sample$distress2_p <- as.numeric(model_sample$distress2_p)-1
#model_sample$distress3_p <- as.numeric(model_sample$distress3_p)-1
#model_sample$distress4_p <- as.numeric(model_sample$distress4_p)-1

# One hot encoding for factor variables 
#model_sample <- fastDummies::dummy_columns(model_sample, select_columns = "sector", 
#                                           remove_selected_columns = T, remove_first_dummy = T)
#model_sample <- fastDummies::dummy_columns(model_sample, select_columns = "size", 
#                                          remove_selected_columns = T, remove_first_dummy = T)
#################################################################################


# Initialize variables to store the best model and its ROC AUC
best_model <- NULL
best_avg_roc_auc <- 0
best_lambda <- NULL

# Values for lambda to perform grid search
lambda_values <- 10^seq(-3, 3, length = 100)

# Perform temporal cross-validation and hyperparameter tuning
for(lambda_val in lambda_values) {
  
  # Initialize a vector to store the ROC AUC for each year
  yearly_roc_auc <- numeric()
  
  for(year in 2015:2018) {
    
    # Split data into training and testing subsets
    train_data <- model_sample[model_sample$year < year, ] %>% select(-year)
    test_data <- model_sample[model_sample$year == year, ] %>% select(-year)
    
    # Convert data to matrix format, required by glmnet
    x_train <- as.matrix(train_data[,-which(names(train_data) == "distressed")])
    y_train <- train_data$distressed
    x_test <- as.matrix(test_data[,-which(names(test_data) == "distressed")])
    y_test <- test_data$distressed
    
    # Train the model (Lasso Logistic Regression) using tuning
    model <- glmnet(x_train, y_train, 
                    alpha = 1, 
                    standardize = T,
                    lambda = lambda_val, 
                    family = "binomial",
                    link = "logit")
    
    # Make predictions on the test data
    predicted_probabilities <- predict(model, newx = x_test, type = "response", s = lambda_val)
    
    # Calculate the ROC AUC of the model and store it
    yearly_roc_auc <- c(yearly_roc_auc, roc(y_test, predicted_probabilities)$auc)
  }
  
  # Calculate the average ROC AUC across all years
  avg_roc_auc <- mean(yearly_roc_auc)
  
  # Update the best model if the current model has a higher average ROC AUC
  if (avg_roc_auc > best_avg_roc_auc) {
    best_model <- model
    best_avg_roc_auc <- avg_roc_auc
    best_lambda <- lambda_val
  }
}

# Print the best model and its average ROC AUC
print(best_model)
print(best_avg_roc_auc)
print(best_lambda)

coef(best_model, s = best_lambda)

# Validation set (2018)
plProb = predict(best_model, newx = x_test, type="response", s = best_lambda)
roc.rf=roc(test_data$distressed, plProb)

plot.roc(roc.rf, col="darkblue", print.auc = TRUE,  auc.polygon=TRUE, grid=c(0.1, 0.2),
         grid.col=c("green", "red"), max.auc.polygon=TRUE,
         auc.polygon.col="lightblue", print.thres=TRUE, main = "PLR - Matched 2018")

prediction <- ifelse(plProb > 0.155,1,0)
levels(test_data$distressed) <- NULL

# Create the confusion matrix
confusionMatrix(factor(prediction), factor(test_data$distressed), positive = "1")

# Testing set (2019 matched)
test_2019 <- model_sample %>% filter(year == 2019) %>% select(-year)
x_test <- as.matrix(test_2019[,-which(names(test_2019) == "distressed")])

plProb = predict(best_model, newx = x_test, type="response", s = best_lambda)
roc.rf=roc(test_2019$distressed, plProb)

plot.roc(roc.rf, col="darkblue", print.auc = TRUE,  auc.polygon=TRUE, grid=c(0.1, 0.2),
         grid.col=c("green", "red"), max.auc.polygon=TRUE,
         auc.polygon.col="lightblue", print.thres=TRUE, main = "PLR - Matched 2019")

prediction <- ifelse(plProb > 0.155,1,0)
levels(test_2019$distressed) <- NULL

# Create the confusion matrix
confusionMatrix(factor(prediction), factor(test_2019$distressed), positive = "1")

metrics_plr_match <- confusionMatrix(factor(prediction), factor(test_2019$distressed), positive = "1")$byClass[1:4] %>% enframe()


# All 2019 
x_2019_full <- as.matrix(full_2019_imputed[,-which(names(full_2019_imputed) == "distressed")])

plProb = predict(best_model, newx = x_2019_full, type="response", s = best_lambda)
roc.rf=roc(full_2019_imputed$distressed, plProb)

plot.roc(roc.rf, col="darkblue", print.auc = TRUE,  auc.polygon=TRUE, grid=c(0.1, 0.2),
         grid.col=c("green", "red"), max.auc.polygon=TRUE,
         auc.polygon.col="lightblue", print.thres=TRUE, main = "PLR - Full 2019") 

prediction <- ifelse(plProb > 0.155,1,0)

# Create the confusion matrix
confusionMatrix(factor(prediction), factor(full_2019_imputed$distressed), positive = "1")

metrics_plr_full <- confusionMatrix(factor(prediction), factor(full_2019_imputed$distressed), positive = "1")$byClass[1:4] %>% enframe()

# Join performance metrics ----

# full 2019
metrics_rf_full <- metrics_rf_full %>% pivot_wider(names_from = "name") 
row.names(metrics_rf_full) <- "RF"

metrics_xgb_full <- metrics_xgb_full %>% pivot_wider(names_from = "name") 
row.names(metrics_xgb_full) <- "XGB"

metrics_plr_full <- metrics_plr_full %>% pivot_wider(names_from = "name") 
row.names(metrics_plr_full) <- "PLR"

metrics_full_2019_imputed <- bind_rows(metrics_plr_full, metrics_rf_full, metrics_xgb_full)

write.xlsx(metrics_full_2019_imputed, "../Salidas/performance metrics.xlsx", sheetName = "full 2019")

#matched 2019
metrics_rf_match <- metrics_rf_match %>% 
  pivot_wider(names_from = "name") 
row.names(metrics_rf_match) <- "RF"

metrics_xgb_match <- metrics_xgb_match %>% pivot_wider(names_from = "name") 
row.names(metrics_xgb_match) <- "XGB"

metrics_plr_match <- metrics_plr_match %>% pivot_wider(names_from = "name") 
row.names(metrics_plr_match) <- "PLR"

metrics_match_2019 <- bind_rows(metrics_plr_match, metrics_rf_match, metrics_xgb_match)

write.xlsx(metrics_match_2019, "../Salidas/performance metrics.xlsx", sheetName = "match 2019", append = T)

