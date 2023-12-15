# File containing all models - each contributor has their own model

# Function to create a MLP classifier and SVM and evaluate both
# inputs:
# training_data - data frame - the data to train the model on
# testing_data - data frame - the data to evaluate the model on
# formula - R formula object - formula for model to use
ModelHarry<-function(training_data, testing_data, formula){
  # neural network
  numOfInputs = length(all.vars(update(formula, z ~.))) - 1
  print(paste("number of Inputs: ", numOfInputs))
  layers = c(60, 10)
  print(paste(layers))
  print("training")
  set.seed(123)
  nn=neuralnet(formula,data=training_data, stepmax = 8000, lifesign.step = 500, hidden=layers, act.fct="logistic", err.fct="ce", algorithm="backprop", learningrate = 0.01, threshold=5, rep=1, linear.output = FALSE, lifesign = "full")
  predictions<-predict(nn, testing_data, type="response")
  
  # SVM
  supportVectorMachine = svm(formula, training_data, cost=0.1, kernel="linear", gamma=0.1, probability=TRUE)
  svmPredictions<-predict(supportVectorMachine, testing_data, type="response")
  return(list(predictions, svmPredictions))
}

ModelChris<-function(training_data, testing_data, formula){
  predictions <- list()
  svmPredictions <- list()
  return(list(predictions, svmPredictions))
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
  svmPredictions <- list()
  return(list(predictions, svmPredictions))
}

debugSource("dataprep.R")

