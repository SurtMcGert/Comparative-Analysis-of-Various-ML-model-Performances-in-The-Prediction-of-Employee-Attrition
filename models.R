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
# - formula: specific formula for dataset
# - plot: A logical value indicating whether to plot feature importance (default is TRUE)
ModelAnna <- function(training_data, testing_data, formula, plot = TRUE) {
    set.seed(123)

  # SVM
  param_grid <- expand.grid(
    C = c(0.1, 1, 10),
    gamma = c(0.01, 0.1, 1)
    )
  
  svm_tune <- tune(
    svm,
    formula,
    type="nu-regression",
    scale=TRUE,
    data = training_data,
    kernel = "radial",
    probability = TRUE,
    nu=0.25,
    ranges = param_grid,
    tol=0.001,
    tunecontrol = tune.control(sampling = "cross", cross = 5)
  )
  best_params <- svm_tune$best.parameters
  svm_tune$best.model
  tune.results <- svm_tune$results

  best_model <- svm_tune$best.model
  
  final_model <- svm(
    formula,
    type="nu-regression",
    scale=TRUE,
    data = training_data,
    kernel = "radial",
    probability = TRUE,
    cost = best_params$C,
    gamma = best_params$gamma,
    nu=0.25
  )
  test_svmpredictedProbs<-predict(final_model, testing_data, type="prob")
  
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
      .mtry = floor(sqrt_ncols) + c(-2, 0, 2)
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

  return(list(test_rfpredictedProbs, test_svmpredictedProbs))
}

# Function to create a decision tree of 30 trials with rules applied
#inputs:
# training_data - data frame - the data to train the model on
# testing_data - data frame - the data to evaluate the model on
#outputs:
# returns the predictions for the column of class 1
ModelMelric<-function(training_data, testing_data){
  # Rebalancing dataset (appears that it is not needed for the model):
  #training_data <- rebalance(training_data, "both", "Attrition")
  #testing_data <- rebalance(testing_data, "under", "Attrition")
  
  predictedFieldLocationTrain <- which(names(training_data)=="Attrition")
  trainingFields <- training_data[, -predictedFieldLocationTrain]
  
  groundTruth <- factor(training_data[, predictedFieldLocationTrain])
  
  # cost_matrix <- matrix(c(0, 1, 1, 15), nrow = 2, dimnames = list(c("0", "1"), c("0", "1")))
  
  #basicTree <- C5.0(x = trainingFields, y = groundTruth, trials = 30, rules= TRUE, costs = cost_matrix)
  basicTree <- C5.0(x = trainingFields, y = groundTruth, trials = 30, rules= TRUE)
  
  # print(summary(basicTree))
  
  # Performing Evaluation
  
  predictedFieldLocationTest <- which(names(testing_data) == "Attrition")
  testingFields <- testing_data[,-predictedFieldLocationTest]
  
  # Predicts using the tree and returns probability for each class
  # predictionsAsProbability <- testPredictedClassProbs <- predict(basicTree, testingFields, type = "prob")
  
  predictionsAsProbability <- predict(basicTree, testingFields, type = "prob")
  # predictionsAsProbability <- predict(basicTree, testingFields)
  
  classLabel <- 1
  
  # Pulls out a single column from the two lists of probabilities for each class
  classIndex<-which(as.numeric(colnames(predictionsAsProbability))==classLabel)
  
  # Gets the predictions for the other column and returns back to the caller
  test_predictedProbs <-predictionsAsProbability[,classIndex]
  
  # class_probs <- table(predictionsAsProbability) / length(predictionsAsProbability)
  # 
  # test_predictedProbs <- class_probs["1"]
  # 
  # 
  # conf_matrix <- confusionMatrix(as.factor(predictionsAsProbability), as.factor(testing_data$Attrition))
  
  # Print the confusion matrix
  # print(conf_matrix)
  
  #return(predictedLabels)
  
  svm_pred <- list()
  
  
  return(list(test_predictedProbs, svm_pred))
}

ModelZion<-function(training_data, testing_data, formula){
  predictions <- list()
  svmPredictions <- list()
  return(list(predictions, svmPredictions))
}

debugSource("dataprep.R")

