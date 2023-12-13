# File containing all models - each contributor has their own model

# Function to create a classifier and evaluate
# inputs:
# training_data - data frame - the data to train the model on
# testing_data - data frame - the data to evaluate the model on
# formula - R formula object - formula for model to use
ModelHarry<-function(training_data, testing_data, formula){
  # neural network
  numOfInputs = length(all.vars(update(formula, z ~.))) - 1
  layers = c(400, 150, 10)
  print(paste(layers))
  nn=neuralnet(formula,data=training_data, hidden=layers,act.fct = "logistic", linear.output = FALSE)
  predictions<-predict(nn, testing_data, type="response")
  
  # SVM
  # supportVectorMachine = svm(formula, training_data, cost=0.1, kernel="linear", gamma=0.1, probability=TRUE)
  # predictions<-predict(supportVectorMachine, testing_data, type="response")
  return(predictions)
}

ModelChris<-function(training_data, testing_data, formula){
  predictions <- list()
  return(predictions)
}

ModelAnna<-function(training_data, testing_data, formula){
  predictions <- list()
  return(predictions)
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
  
  
  return(test_predictedProbs)
}



ModelZion<-function(training_data, testing_data, formula){
  predictions <- list()
  return(predictions)
}

debugSource("dataprep.R")
library("C50")


