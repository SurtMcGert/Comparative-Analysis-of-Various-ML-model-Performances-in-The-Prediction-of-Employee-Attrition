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
# - formula: specific formula for dataset
# - plot: A logical value indicating whether to plot feature importance (default is TRUE)
ModelAnna <- function(training_data, testing_data, formula, plot = TRUE) {
    set.seed(123)
  
    # RF
    rf <- RFTrainer$new(classification=1,
                        seed=42,
                        verbose=TRUE)
    
    parameters = list(
      n_estimators = c(500, 1000, 1500, 2000, 3000),
      max_depth = c(1, 10, 50)
      )
    
    # Tune ntrees and max_nodes
    gst <-GridSearchCV$new(trainer = rf,
                           parameters = parameters,
                           n_folds = 3,
                           scoring = c('accuracy','auc'))
    gst$fit(training_data, "Attrition")
    best_params <- gst$best_iteration()
    n_estimators = best_params$n_estimators
    max_depth = best_params$max_depth
    results <- data.frame(gst$results)
    print("Summary of ntrees and max-node tuning:")
    print(gst$evaluation_scores)
    print("Best tree number")
    print(n_estimators)
    print("Best node depth")
    print(max_depth)
    
    
    
    # Tune mtry
    sqrt_ncols <- sqrt(ncol(training_data))
    metric <- 'auc'
    
    param_grid <- expand.grid(
      .mtry = floor(sqrt_ncols) + c(-1, 0, 1)
    )

    ctrl <- trainControl(
      method = "repeatedcv",
      number = 5,
      summaryFunction = twoClassSummary,
      classProbs = TRUE,
      search='grid'
    )
    
    # Inputs must be factors
    training_data[, "Attrition"] <- factor(training_data[, "Attrition"])
    levels(training_data$Attrition) <- make.names(levels(training_data$Attrition))

    testing_data[, "Attrition"] <- factor(testing_data[, "Attrition"])
    levels(testing_data$Attrition) <- make.names(levels(testing_data$Attrition))
    
    rf_model <- train(
      Attrition ~ ., data = training_data,
      method = "rf",
      metric = metric,
      trControl = ctrl,
      tuneGrid = param_grid
    )
    
    print("Tune mtry")
    print(rf_model$results)
    
    best_mtry <- rf_model$bestTune$mtry

    final_rf_model <- randomForest(
      Attrition ~ ., data = training_data,
      mtry = best_mtry,
      ntree = n_estimators,
      max_depth = max_depth
    )
    test_rfpredictedProbs <- predict(final_rf_model, newdata = testing_data, type = "prob")[, 2]

  
  # Plot feature importance if specified
  if (plot) {
    # Access feature importance from model
    feature_importance <- final_rf_model$importance
    # Plot feature importance
    varImpPlot(final_rf_model, main = "Feature Importance")
  }

  # SVM
  svm_model <- svm(
    formula,
    data=training_data,
    kernel = "radial",
    probability = TRUE
  )

  test_svmpredictedProbs<-predict(svm_model, testing_data, type="prob")

  return(list(test_rfpredictedProbs, test_svmpredictedProbs))
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

