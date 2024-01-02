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
  nn=neuralnet(formula,data=training_data, 
               stepmax = 8000, 
               lifesign.step = 500, 
               hidden=layers, 
               act.fct="logistic", 
               err.fct="ce", 
               algorithm="backprop", 
               learningrate = 0.01, 
               threshold=5, 
               rep=1, 
               linear.output = FALSE, 
               lifesign = "full")
  predictions<-predict(nn, testing_data, type="response")
  
  # SVM
  supportVectorMachine = svm(formula, 
                             training_data, 
                             type="nu-regression", 
                             nu=0.3, 
                             cost=0.1, 
                             kernel="polynomial", 
                             degree="5", 
                             gamma=0.5, 
                             tolerance = 0.001, 
                             probability=TRUE)
  svmPredictions<-predict(supportVectorMachine, testing_data, type="response")
  
  return(list(predictions, svmPredictions))
}

ModelChris<-function(training_data, testing_data, formula){
  print("Running Chris model")
  
  # Copy 'Attrition' field as a factor before it is removed
  y_train = as.factor(training_data$Attrition)
  
  # Remove 'Attrition' field
  # Put into a separate variable since the second model also has to use training_data
  X_training_data = training_data[, -which(names(training_data) == "Attrition")]
  X_testing_data <- as.matrix(testing_data[, -which(names(testing_data) == "Attrition")])
  
  # Cross-validation model
  cv_model <- cv.glmnet(as.matrix(X_training_data), y = y_train, family = "binomial", nfolds = 10)
  
  # Extract best lambda
  best_lambda <- cv_model$lambda.min
  
  # Re-train using best lambda (alpha = 0 means ridge penalty, alpha = 1 means lasso penalty)
  logisticModel <- glmnet(x = X_training_data,
                          y = y_train,
                          family = "binomial",
                          alpha = 1,
                          lambda = best_lambda)
  
  predictions <- predict(logisticModel, newx = X_testing_data, type = "response")
  
  # SVM
  supportVectorMachine = svm(formula,
                             training_data,
                             type = "nu-regression",
                             cost = 0.05,
                             kernel = "sigmoid",
                             gamma = 0.075,
                             cross = 10,
                             probability = TRUE)
  svmPredictions<-predict(supportVectorMachine, testing_data, type = "response")
  
  return(list(predictions, svmPredictions))
}

# Function parameters:
# - training_data: Training dataset
# - testing_data: Testing dataset
# - formula: specific formula for dataset
# - plot: A logical value indicating whether to plot feature importance (default is TRUE)
ModelAnna <- function(training_data, testing_data, formula, plot = TRUE) {
  set.seed(123)
  
  
  svm_model = svm(formula, 
                  training_data, 
                  type="nu-regression", 
                  nu=0.3, 
                  cost=0.1, 
                  kernel="polynomial", 
                  degree="2", 
                  gamma=0.06, 
                  probability=TRUE)
  test_svmpredictedProbs<-predict(svm_model, testing_data, type="response")
  
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
# Function to create a decision tree of 30 trials with rules applied
# inputs:
# training_data - data frame - the data to train the model on
# testing_data - data frame - the data to evaluate the model on
# outputs:
# returns the predictions for the column of class 1 for decision tree and SVM
ModelMelric<-function(training_data, testing_data, formula){
  
  newtraining_data <- training_data
  newtesting_data <- testing_data
  
  # Split training set into training and 'validation' using subset
  
  validationIndices <- sample(1:nrow(newtraining_data), 0.7 * nrow(newtraining_data))
  
  newtraining_data <- newtraining_data[validationIndices, ]
  newvalidation_data <- newtraining_data[setdiff(1:nrow(newtraining_data), validationIndices), ]
  
  
  predictedFieldLocationTrain <- which(names(newtraining_data)=="Attrition")
  trainingFields <- newtraining_data[, -predictedFieldLocationTrain]
  
  groundTruth <- factor(newtraining_data[, predictedFieldLocationTrain])
  
  basicTree <- C5.0(x = trainingFields, y = groundTruth, trials = 25, rules= TRUE)
  
  
  # # Inputs must be factors
  # newtraining_data[, "Attrition"] <- factor(newtraining_data[, "Attrition"])
  # levels(newtraining_data$Attrition) <- make.names(levels(newtraining_data$Attrition))
  # 
  # newtesting_data[, "Attrition"] <- factor(newtesting_data[, "Attrition"])
  # levels(newtesting_data$Attrition) <- make.names(levels(newtesting_data$Attrition))
  
  # print(summary(basicTree))
  
  # Perform evaluation on validation set and print confusion metrics to see statistics
  validationFieldLocation <- which(names(newvalidation_data) == "Attrition")
  validationFields <- newvalidation_data[,-validationFieldLocation]
  
  validationProbability <- predict(basicTree, validationFields, type = "prob")
  
  # Choose the class with the highest probability
  valPredicted <- colnames(validationProbability)[apply(validationProbability, 1, which.max)]
  
  cm <- confusionMatrix(as.factor(valPredicted), as.factor(newvalidation_data$Attrition))
  
  print(cm)
  
  # Issue with using a validation set is that there isnt enough data? Accuracy on validation set is 100% ?
  
  # Performing Evaluation
  
  predictedFieldLocationTest <- which(names(newtesting_data) == "Attrition")
  testingFields <- newtesting_data[,-predictedFieldLocationTest]
  
  # Predicts using the tree and returns probability for each class
  predictionsAsProbability <- predict(basicTree, testingFields, type = "prob")
  
  classLabel <- 1
  
  # Pulls out a single column from the two lists of probabilities for each class
  classIndex<-which(as.numeric(colnames(predictionsAsProbability))==classLabel)
  
  # Gets the predictions for the other column and returns back to the caller
  test_predictedProbs <-predictionsAsProbability[,classIndex]
  
  
  # SVM
  supportVectorMachine = svm(formula,
                             training_data,
                             cost=10,
                             kernel="radial",
                             gamma=0.3,
                             nu=0.5,
                             type="nu-regression",
                             scale=TRUE,
                             probability=TRUE)
  
  svmPredictions<-predict(supportVectorMachine, newtesting_data, type="response")
  
  return(list(test_predictedProbs, svmPredictions))
}

#' Naive Bayes and SVM classifiers
#'
#' @param training_data Dataframe containing training data
#' @param testing_data Dataframe containing testing data
#' @param formula Formula for model, currently using Attrition as target
#'
#' @return List with probabilities for the Naive Bayes and SVM classifiers
#' @export
#'
#' @examples
#' ModelZion(training_data, testing_data, Attrition ~ .)
#'
ModelZion <- function(training_data, testing_data, formula) {
  set.seed(123)
  
  trainingNB <- training_data
  trainingSVM <- training_data
  testingNB <- testing_data
  testingSVM <- testing_data
  
  print(str(trainingNB))
  
  trainingNB[] <- lapply(trainingNB, factor)
  testingNB[] <- lapply(testingNB, factor)
  
  print(str(trainingNB))
  
  # Create Naive Bayes model
  nbModel <-
    naiveBayes(
      formula,
      data = trainingNB,
      laplace = 1
    )
  # Predict using created model
  predictionsRaw <-
    predict(nbModel,
            newdata = testingNB,
            type = "raw")
  
  print(head(predictionsRaw))
  
  classIdx <- which(as.numeric(colnames(predictionsRaw)) == 1)
  
  predictions <- predictionsRaw[, classIdx]
  
  # Predict using SVM model
  supportVectorMachine = svm(
    formula,
    trainingSVM,
    cost = 1,
    kernel = "linear",
    probability = TRUE,
    cross = 5
  )
  svmPredictions <-
    predict(supportVectorMachine, testingSVM, type = "response")
  
  print(head(svmPredictions))
  
  # Return Naive Bayes predictions and SVM predictions
  return(list(predictions, svmPredictions))
}

debugSource("dataprep.R")

