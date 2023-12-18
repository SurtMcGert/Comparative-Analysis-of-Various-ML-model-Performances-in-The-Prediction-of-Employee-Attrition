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
  print("Running Chris model")
  
  # Cross-validation model
  cv_model <- cv.glmnet(x = as.matrix(training_data), y = training_data$Attrition)
  # Select best lambda
  best_lambda <- cv_model$lambda.min
  # Re-train using best lambda (alpha = 0 means ridge penalty, alpha = 1 means lasso penalty)
  logisticModel <- glmnet(x = as.matrix(training_data), y = training_data$Attrition, family = "binomial", lambda = best_lambda, alpha = 1)
  
  predictions <- predict(object = logisticModel, newx = as.matrix(testing_data), type="response", s = best_lambda, alpha = 1)
  
  predictions <- list()
  svmPredictions <- list()
  return(list(predictions, svmPredictions))
}

# Function parameters:
# - training_data: Training dataset
# - testing_data: Testing dataset
# - OUTPUT_FIELD: Name of the output field (target variable) in the dataset
# - plot: A logical value indicating whether to plot feature importance (default is TRUE)
ModelAnna <- function(training_data, testing_data, formula, plot = TRUE) {
  set.seed(123)
  # RF
  rf <- RFTrainer$new()
  gst <-GridSearchCV$new(trainer = rf,
                         parameters = list(n_estimators = c(500, 1000, 1500),
                                           max_depth = c(1,2,5,10)),
                         n_folds = 3,
                         scoring = c('accuracy','auc'))
  gst$fit(training_data, "Attrition")
  best_params <- gst$best_iteration()
  n_estimators = best_params$n_estimators
  max_depth = best_params$max_depth
  best_model <- randomForest(Attrition ~ ., data = training_data, n_estimators = n_estimators, max_depth = max_depth)
  predictions <- predict(best_model, newdata = testing_data, type = "response")

  # Plot feature importance if specified
  if (plot) {
    # Access feature importance from model
    feature_importance <- best_model$importance
    # Plot feature importance
    varImpPlot(best_model, main = "Feature Importance")
  }
  
  # SVM
  supportVectorMachine = svm(formula, training_data, kernel="linear", probability=TRUE)
  svmPredictions<-predict(supportVectorMachine, testing_data, type="response")
  
  return(list(predictions, svmPredictions))
}

ModelMelric<-function(training_data, testing_data, formula){
  predictions <- list()
  predictions2 <- list()
  return(list(predictions, predictions2))
}

ModelZion<-function(training_data, testing_data, formula){
  predictions <- list()
  svmPredictions <- list()
  return(list(predictions, svmPredictions))
}

debugSource("dataprep.R")

