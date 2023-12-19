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


ModelAnna<-function(training_data, testing_data, ENSEMBLE_SIZE, FOREST_SIZE, OUTPUT_FIELD, plot=TRUE){
  # Try multiple models and then aggregate their predictions.
  # I train multiple models with different subsets of data and different hyperparameters
  myTitle<-paste("Ensamble of Random Forests (Size=",ENSEMBLE_SIZE, "Trees=", FOREST_SIZE,")")
  print(myTitle)
  positionClassOutput<-which(names(training_data)==OUTPUT_FIELD)
  
  forest_list <- list()
  
  # Train random forest models
  for (i in 1:ENSEMBLE_SIZE) {
    sampled_indices <- sample(1:nrow(training_data), replace = TRUE)
    sampled_data <- training_data[sampled_indices, ]
    
    train_inputs <- sampled_data[-positionClassOutput]
    print(ncol(train_inputs))
    
    train_expected <- sampled_data[, positionClassOutput]
    rf <- randomForest::randomForest(train_inputs,
                                     factor(train_expected),
                                     ntree = FOREST_SIZE,
                                     importance = TRUE,
                                     mtry = sqrt(ncol(train_inputs)),
                                     nodesize=5)
    
    # Add each trained model to the list
    forest_list[[i]] <- rf
  }
  
  # train data: dataframe with the input fields
  # train_inputs<-training_data[-positionClassOutput]
  
  # train data: vector with the expedcted output
  # train_expected<-training_data[,positionClassOutput]
  
  # rf<-randomForest::randomForest(train_inputs,
  #                                factor(train_expected),
  #                                ntree=FOREST_SIZE ,
  #                                importance=TRUE,
  #                                mtry=sqrt(ncol(train_inputs)))
  
  
  # ************************************************
  # Use the created decision tree with the test dataset
  # measures<-getTreeClassifications(myTree = rf,
  #                                  testDataset = testing_data,
  #                                  title=myTitle,
  #                                  plot=plot,
  #                                  OUTPUT_FIELD=OUTPUT_FIELD)
  
  test_predictedProbs <- matrix(0, nrow = nrow(testing_data), ncol = ENSEMBLE_SIZE)
  for (i in 1:ENSEMBLE_SIZE) {
    rf <- forest_list[[i]]
    test_predictedProbs[, i] <- predict(rf, testing_data, type = "prob")[, 2]
  }
  
  # normalize predictions
  test_predictedProbs <- rowMeans(test_predictedProbs)
  
  if (plot==TRUE){
    # Get importance of the input fields
    importance<-randomForest::importance(rf,scale=TRUE,type=1)
    importance<-importance[order(importance,decreasing=TRUE),,drop=FALSE]
    
    colnames(importance)<-"Strength"
    
    barplot(t(importance),las=2, border = 0,
            cex.names =0.7,
            main=myTitle)
    
    print(formattable::formattable(data.frame(importance)))
  }
  
  # return(measures)
  svmPredictions <- list()
  return(list(test_predictedProbs, svmPredictions))
}

# Function to create a decision tree of 30 trials with rules applied
# inputs:
# training_data - data frame - the data to train the model on
# testing_data - data frame - the data to evaluate the model on
# outputs:
# returns the predictions for the column of class 1 for decision tree and SVM
ModelMelric<-function(training_data, testing_data, formula){
  
  # Split training set into training and 'validation' using subset
  
  validationIndices <- sample(1:nrow(training_data), 0.7 * nrow(training_data))
  
  training_data <- training_data[validationIndices, ]
  validation_data <- training_data[setdiff(1:nrow(training_data), validationIndices), ]
  
  
  predictedFieldLocationTrain <- which(names(training_data)=="Attrition")
  trainingFields <- training_data[, -predictedFieldLocationTrain]

  groundTruth <- factor(training_data[, predictedFieldLocationTrain])

  basicTree <- C5.0(x = trainingFields, y = groundTruth, trials = 25, rules= TRUE)

  # print(summary(basicTree))
  
  # Perform evaluation on validation set and print confusion metrics to see statistics
  validationFieldLocation <- which(names(validation_data) == "Attrition")
  validationFields <- validation_data[,-validationFieldLocation]
  
  validationProbability <- predict(basicTree, validationFields, type = "prob")
  
  # Choose the class with the highest probability
  valPredicted <- colnames(validationProbability)[apply(validationProbability, 1, which.max)]
  
  cm <- confusionMatrix(as.factor(valPredicted), as.factor(validation_data$Attrition))
  
  print(cm)
  
  # Issue with using a validation set is that there isnt enough data? Accuracy on validation set is 100% ?

  # Performing Evaluation

  predictedFieldLocationTest <- which(names(testing_data) == "Attrition")
  testingFields <- testing_data[,-predictedFieldLocationTest]

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
  
  svmPredictions<-predict(supportVectorMachine, testing_data, type="response")
  
  return(list(test_predictedProbs, svmPredictions))
}


ModelZion<-function(training_data, testing_data, formula){
  predictions <- list()
  svmPredictions <- list()
  return(list(predictions, svmPredictions))
}
debugSource("dataprep.R")

