# XGB Model

#load("../Datos/imputed_data.RData")

# One hot encoding for factor variables 
model_sample$distressed <- as.numeric(model_sample$distressed)-1

model_sample$distress2_p <- as.numeric(model_sample$distress2_p)-1
model_sample$distress3_p <- as.numeric(model_sample$distress3_p)-1
model_sample$distress4_p <- as.numeric(model_sample$distress4_p)-1


model_sample <- fastDummies::dummy_columns(model_sample, 
                                           select_columns = "sector", remove_selected_columns = T, remove_first_dummy = T)

model_sample <- fastDummies::dummy_columns(model_sample, 
                                           select_columns = "size", remove_selected_columns = T, remove_first_dummy = T)


# Initialize variables to store the best model and its ROC AUC
best_model <- NULL
best_avg_roc_auc <- 0
best_eta <- NULL
best_max_depth <- NULL
best_nrounds <- NULL

# Values for eta, max_depth, and nrounds to perform grid search
eta_values <- c(0.01,0.05,0.1, 0.15,0.2, 0.3)
max_depth_values <- c(4,5,6,7,8)
nrounds_values <- c(150, 200, 250, 300, 350)

# Perform temporal cross-validation and hyperparameter tuning
for(eta_val in eta_values) {
  for(max_depth_val in max_depth_values) {
    for(nrounds_val in nrounds_values) {
      
      # Initialize a vector to store the ROC AUC for each year
      yearly_roc_auc <- numeric()
      
      for(year in 2015:2018) {
        
        # Split data into training and testing subsets
        train_data <- model_sample[model_sample$year < year, ] %>% select(-year)
        test_data <- model_sample[model_sample$year == year, ] %>% select(-year)
        
      
        # Convert data to matrix format, required by xgboost
        dtrain <- xgb.DMatrix(data = as.matrix(train_data[,-which(names(train_data) == "distressed")]),
                              label = train_data$distressed)
        dtest <- xgb.DMatrix(data = as.matrix(test_data[,-which(names(test_data) == "distressed")]),
                             label = test_data$distressed)
        
        # Train the model (XGBoost) using tuning
        model <- xgboost(
          data = dtrain,
          eta = eta_val,
          max_depth = max_depth_val,
          nrounds = nrounds_val,
          objective = "binary:logistic")
        
        # Make predictions on the test data
        predicted_probabilities <- predict(model, newdata = dtest)
        
        # Calculate the ROC AUC of the model and store it
        yearly_roc_auc <- c(yearly_roc_auc, roc(test_data$distressed, predicted_probabilities)$auc)
      }
      
      # Calculate the average ROC AUC across all years
      avg_roc_auc <- mean(yearly_roc_auc)
      
      # Update the best model if the current model has a higher average ROC AUC
      if (avg_roc_auc > best_avg_roc_auc) {
        best_model <- model
        best_avg_roc_auc <- avg_roc_auc
        best_eta <- eta_val
        best_max_depth <- max_depth_val
        best_nrounds <- nrounds_val
      }
    }
  }
}

# Print the best model and its average ROC AUC
print(best_model)
print(best_avg_roc_auc)
print(best_eta)
print(best_max_depth)
print(best_nrounds)

# Validation set 
xgbProb = predict(best_model, as.matrix(test_data[,-which(names(test_data) == "distressed")]), type="prob")
roc.xgb=roc(test_data$distressed, xgbProb)

plot.roc(roc.xgb, col="darkblue", print.auc = TRUE,  auc.polygon=TRUE, grid=c(0.1, 0.2),
         grid.col=c("green", "red"), max.auc.polygon=TRUE,
         auc.polygon.col="lightblue", print.thres=TRUE, main = "XGB - Matched 2018")

prediction <- ifelse(xgbProb > 0.139, 1, 0)

# Create the confusion matrix
confusionMatrix(as.factor(prediction), as.factor(test_data$distressed))

# Importance of variables
new_feature_names <- c("Equity to assets",
                       "Liquidity ratio",
                       "ROE",
                       "Profit to assets",
                       "Age",
                       "Employment change",
                       "Current ratio",
                       "Debt to equity",
                       "Interest charge",
                       "Debt quality ratio")

gg_imp <- xgb.ggplot.importance(importance_matrix = xgb.importance(colnames(train_data[,-1]), model = best_model), 
                      rel_to_first = T, top_n = 10, n_clusters = 1) + 
  theme_light() + 
  theme(text = element_text(family = "serif", size = 12),
        plot.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 12),
        legend.position = "none")+
  scale_x_discrete(labels = rev(new_feature_names)) +
  scale_fill_manual(values = "#F57217")

ggsave(plot = gg_imp, "../Graficos/var_importance.png", height = 2, width = 6)

# Testing test

test_2019 <- model_sample %>% 
  filter(year == 2019) %>% 
  select(-year)

xgbProb = predict(best_model, as.matrix(test_2019[,-which(names(test_2019) == "distressed")]), type="prob")
roc.xgb=roc(test_2019$distressed, xgbProb)

plot.roc(roc.xgb, col="darkblue", print.auc = TRUE,  auc.polygon=TRUE, grid=c(0.1, 0.2),
         grid.col=c("green", "red"), max.auc.polygon=TRUE,
         auc.polygon.col="lightblue", print.thres=TRUE, main = "XGB - Matched 2019")

prediction <- ifelse(xgbProb > 0.139, 1, 0)

# Create the confusion matrix
confusionMatrix(as.factor(prediction), as.factor(test_2019$distressed), positive = "1")

metrics_xgb_match <- confusionMatrix(as.factor(prediction), as.factor(test_2019$distressed), positive = "1")$byClass[1:4] %>% enframe()


# All 2019 
load("../Datos/full_2019_imputed.RData")
full_2019_imputed$distress2_p <- as.numeric(full_2019_imputed$distress2_p)-1
full_2019_imputed$distress3_p <- as.numeric(full_2019_imputed$distress3_p)-1
full_2019_imputed$distress4_p <- as.numeric(full_2019_imputed$distress4_p)-1


full_2019_imputed <- fastDummies::dummy_columns(full_2019_imputed, 
                                           select_columns = "sector", remove_selected_columns = T, remove_first_dummy = T)

full_2019_imputed <- fastDummies::dummy_columns(full_2019_imputed, 
                                           select_columns = "size", remove_selected_columns = T, remove_first_dummy = T)


matrix_2019 <- xgb.DMatrix(data = as.matrix(full_2019_imputed[,-which(names(train_data) == "distressed")]),
                        label = full_2019_imputed$distressed)


xgbProb = predict(best_model, matrix_2019, type="prob")
roc.xgb=roc(full_2019_imputed$distressed, xgbProb)

plot.roc(roc.xgb, col="darkblue", print.auc = TRUE,  auc.polygon=TRUE, grid=c(0.1, 0.2),
         grid.col=c("green", "red"), max.auc.polygon=TRUE,
         auc.polygon.col="lightblue", print.thres=TRUE, main = "XGB - Full 2019")

prediction <- ifelse(xgbProb > 0.139, 1, 0)

# Create the confusion matrix
confusionMatrix(as.factor(prediction), as.factor(full_2019_imputed$distressed), positive = "1")

metrics_xgb_full <- confusionMatrix(as.factor(prediction), as.factor(full_2019_imputed$distressed), positive = "1")$byClass[1:4] %>% enframe()


