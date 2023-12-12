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
  logisticModel<-stats::glm(formula,data=training_data,family=quasibinomial)
  predictions<-predict(logisticModel, testing_data, type="response")
  
  return(predictions)
}

ModelAnna<-function(training_data, testing_data, formula){
  predictions <- list()
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

