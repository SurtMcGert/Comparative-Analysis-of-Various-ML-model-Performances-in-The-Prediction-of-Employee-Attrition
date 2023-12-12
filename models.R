# File containing all models - each contributor has their own model

# Function to create a classifier and evaluate
# inputs:
# training_data - data frame - the data to train the model on
# testing_data - data frame - the data to evaluate the model on
# formula - R formula object - formula for model to use
ModelHarry<-function(training_data, testing_data, formula){
  # neural network
  numOfInputs = length(all.vars(update(formula, z ~.))) - 1
  # layers = c(ceiling(numOfInputs * 0.9), ceiling(numOfInputs * 0.6), ceiling(numOfInputs * 0.2), 4)
  # layers = c(150, 5, 5)
  layers = c(400, 150, 10)
  print(paste(layers))
  nn=neuralnet(formula,data=training_data, hidden=layers,act.fct = "logistic", linear.output = FALSE)
  predictions<-predict(nn, testing_data, type="response")
  
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

ModelMelric<-function(training_data, testing_data, formula){
  predictions <- list()
  return(predictions)
}

ModelZion<-function(training_data, testing_data, formula){
  predictions <- list()
  return(predictions)
}

