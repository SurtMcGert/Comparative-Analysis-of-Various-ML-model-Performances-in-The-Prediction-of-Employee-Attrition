# File containing all models - each contributor has their own model

# Function to create a classifier and evaluate
# inputs:
# training_data - data frame - the data to train the model on
# testing_data - data frame - the data to evaluate the model on
# formula - R formula object - formula for model to use
ModelHarry<-function(training_data, testing_data, formula){
  predictions <- list()
  return(predictions)
}

ModelChris<-function(training_data, testing_data, formula){
  predictions <- list()
  return(predictions)
}

# Function parameters:
# - training_data: Training dataset
# - testing_data: Testing dataset
# - ENSEMBLE_SIZE: Number of random forests in the ensemble
# - FOREST_SIZE: Number of trees in each random forest
# - OUTPUT_FIELD: Name of the output field (target variable) in the dataset
# - plot: A logical value indicating whether to plot feature importance (default is TRUE)
# ModelAnna <- function(training_data, testing_data, OUTPUT_FIELD, plot = TRUE) {
#   # Identify the position of the output field in the dataset
#   positionClassOutput <- which(names(training_data) == OUTPUT_FIELD)
#   
#   # Ensure the target variable is a factor with valid levels
#   training_data[, "Attrition"] <- factor(training_data[, "Attrition"])
#   levels(training_data$Attrition) <- make.names(levels(training_data$Attrition))
#   
#   testing_data[, "Attrition"] <- factor(testing_data[, "Attrition"])
#   levels(testing_data$Attrition) <- make.names(levels(testing_data$Attrition))
#   
#   # Define the hyperparameter grid for the random forest
#   param_grid <- expand.grid(
#     .mtry = c(5 : 7)
#   )
#   
#   # Specify the training control parameters for cross-validation
#   ctrl <- trainControl(
#     method = "cv",      # Cross-validation method
#     number = 5,          # Number of folds
#     summaryFunction = twoClassSummary,
#     classProbs = TRUE,
#     verboseIter = TRUE
#   )
#   
#   # Train the random forest model with grid search
#   rf_model <- train(
#     Attrition ~ ., data = training_data,
#     method = "rf",            # Random Forest method
#     metric = "logLoss",       # Scoring metric
#     trControl = ctrl,
#     tuneGrid = param_grid
#   )
#   
#   best_mtry <- rf_model$bestTune$mtry
#   
#   print("best mtry")
#   print(best_mtry)
#   
#   final_rf_model <- randomForest(
#     Attrition ~ ., data = training_data,
#     mtry = best_mtry,
#     ntree = 150
#   )
#   
#   # Make predictions on the testing data of the positive class
#   # When evaluating model performance it is common practice to 
#   # extract predicted probabilities for the positive class 
#   test_predictedProbs <- predict(final_rf_model, newdata = testing_data, type = "prob")[, 2]
#   
#   # Plot feature importance if specified
#   if (plot) {
#     # Access feature importance from the random forest model
#     feature_importance <- rf_model$finalModel$importance
#     
#     # Plot the feature importance
#     varImpPlot(rf_model$finalModel, main = "Feature Importance")
#   }
#   
#   # Return the final model predictions
#   return(test_predictedProbs)
# }

# ModelAnna <- function(training_data, testing_data, OUTPUT_FIELD, plot = TRUE) {
#   # Identify the position of the output field in the dataset
#   positionClassOutput <- which(names(training_data) == OUTPUT_FIELD)
#   
#   # Ensure the target variable is a factor with valid levels
#   training_data[, "Attrition"] <- factor(training_data[, "Attrition"])
#   levels(training_data$Attrition) <- make.names(levels(training_data$Attrition))
#   
#   testing_data[, "Attrition"] <- factor(testing_data[, "Attrition"])
#   levels(testing_data$Attrition) <- make.names(levels(testing_data$Attrition))
#   
#   customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
#   customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
#   customRF$grid <- function(x, y, len = NULL, search = "grid") {}
#   customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
#     randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
#   }
#   customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
#     predict(modelFit, newdata)
#   customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
#     predict(modelFit, newdata, type = "prob")
#   customRF$sort <- function(x) x[order(x[,1]),]
#   customRF$levels <- function(x) x$classes
#   
#   # train
#   metric <- "Accuracy"
#   control <- trainControl(method="repeatedcv", number=2, repeats=2)
#   tunegrid <- expand.grid(.mtry=c(1:2), .ntree=c(500, 1000))
#   set.seed(123)
#   custom <- train(Attrition~., data = training_data, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
#   summary(custom)
#   plot(custom)
#   
#   best_params <- custom$bestTune
#   
#   best_model <- randomForest(Attrition ~ ., data = training_data, mtry = best_params$mtry, ntree = best_params$ntree)
#   print("best model")
#   print(best_model)
#   # Get prediction probabilities on the testing data
#   predictions <- predict(best_model, newdata = testing_data, type = "prob")[,2]
#   
#   
#   # Make predictions on the testing data of the positive class
#   # When evaluating model performance it is common practice to 
#   # extract predicted probabilities for the positive class 
#   # Plot feature importance if specified
#   if (plot) {
#     # Access feature importance from the random forest model
#     feature_importance <- best_model$importance
# 
#     # Plot the feature importance
#     varImpPlot(best_model, main = "Feature Importance")
#   }
#   
#   # Return the final model predictions
#   return(predictions)
# }

ModelAnna <- function(training_data, testing_data, OUTPUT_FIELD, plot = TRUE) {
  # Identify the position of the output field in the dataset
  positionClassOutput <- which(names(training_data) == OUTPUT_FIELD)
  
  # Ensure the target variable is a factor with valid levels
  training_data[, "Attrition"] <- factor(training_data[, "Attrition"])
  levels(training_data$Attrition) <- make.names(levels(training_data$Attrition))
  
  testing_data[, "Attrition"] <- factor(testing_data[, "Attrition"])
  levels(testing_data$Attrition) <- make.names(levels(testing_data$Attrition))
  
  rf <- RFTrainer$new()
  gst <-GridSearchCV$new(trainer = rf,
                         parameters = list(n_estimators = c(500, 1000, 1500),
                                           max_depth = c(1,2,5,10)),
                         n_folds = 3,
                         scoring = c('accuracy','auc'))
  
  gst$fit(training_data, "Attrition")
  best_params <- gst$best_iteration()
  
  print("best params")
  print(best_params)
  
  n_estimators = best_params$n_estimators
  max_depth = best_params$max_depth
  
  best_model <- randomForest(Attrition ~ ., data = training_data, n_estimators = n_estimators, max_depth = max_depth)
  print("best model")
  print(best_model)
  # Get prediction probabilities on the testing data
  predictions <- predict(best_model, newdata = testing_data, type = "prob")[,2]


  # Make predictions on the testing data of the positive class
  # When evaluating model performance it is common practice to
  # extract predicted probabilities for the positive class
  # Plot feature importance if specified
  if (plot) {
    # Access feature importance from the random forest model
    feature_importance <- best_model$importance

    # Plot the feature importance
    varImpPlot(best_model, main = "Feature Importance")
  }

  # Return the final model predictions
  return(predictions)
}

ModelMelric<-function(training_data, testing_data, formula){
  predictions <- list()
  return(predictions)
}

ModelZion<-function(training_data, testing_data, formula){
  predictions <- list()
  return(predictions)
}


getTreeClassifications<-function(myTree,
                                 testDataset,
                                 title,
                                 classLabel=1,
                                 plot=TRUE,
                                 OUTPUT_FIELD){
  
  positionClassOutput=which(names(testDataset)==OUTPUT_FIELD)
  
  #test data: dataframe with with just input fields
  test_inputs<-testDataset[-positionClassOutput]
  
  # Generate class membership probabilities
  # Column 1 is for class 0 (bad loan) and column 2 is for class 1 (good loan)
  
  testPredictedClassProbs<-predict(myTree,test_inputs, type="prob")
  
  # Get the column index with the class label
  classIndex<-which(as.numeric(colnames(testPredictedClassProbs))==classLabel)
  
  # Get the probabilities for classifying attrition
  test_predictedProbs<-testPredictedClassProbs[,classIndex]
  
  #test data: vector with just the expected output class
  test_expected<-testDataset[,positionClassOutput]
  
  # measures<-NdetermineThreshold(test_expected=test_expected,
  #                               test_predicted=test_predictedProbs,
  #                               plot=plot,
  #                               title=title)
  
  # if (plot==TRUE)
  #   NprintMeasures(results=measures,title=title)
  # 
  # return(measures)
  return(test_predictedProbs)
} #endof getTreeClassifications()
debugSource("dataPrep.R")

