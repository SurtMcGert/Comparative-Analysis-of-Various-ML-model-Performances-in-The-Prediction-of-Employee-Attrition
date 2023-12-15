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
# - OUTPUT_FIELD: Name of the output field (target variable) in the dataset
# - plot: A logical value indicating whether to plot feature importance (default is TRUE)
ModelAnna <- function(training_data, testing_data, OUTPUT_FIELD, plot = TRUE) {
  # Position of the output field in the dataset
  positionClassOutput <- which(names(training_data) == OUTPUT_FIELD)
  
  # Make sure the target variable is a factor with valid levels
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
  
  print("Best Params")
  print(best_params)
  
  n_estimators = best_params$n_estimators
  max_depth = best_params$max_depth
  
  best_model <- randomForest(Attrition ~ ., data = training_data, n_estimators = n_estimators, max_depth = max_depth)
  print("best model")
  print(best_model)
  
  # Make predictions on the testing data of the positive class
  # When evaluating model performance it is common practice to
  # extract predicted probabilities for the positive class
  predictions <- predict(best_model, newdata = testing_data, type = "prob")[,2]

  # Plot feature importance if specified
  if (plot) {
    # Access feature importance from model
    feature_importance <- best_model$importance

    # Plot feature importance
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

