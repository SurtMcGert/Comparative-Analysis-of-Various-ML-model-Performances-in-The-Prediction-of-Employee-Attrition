# File containing all models - each contributor has their own model

# Function to create a classifier and evaluate
# inputs:
# training_data - data frame - the data to train the model on
# testing_data - data frame - the data to evaluate the model on
# formula - R formular object - formula for model to use
ModelHarry<-function(training_data, testing_data, formula){
  # Example model
  logisticModel<-stats::glm(formula,data=training_data,family=quasibinomial)
  predictions<-predict(logisticModel, testing_data,type="response")
  
  return(predictions)
}

ModelChris<-function(training_data, testing_data, formula){
  
  return(predictions)
}

ModelAnna<-function(training_data, testing_data, formula){
  # Test
  return(predictions)
}

ModelMelric<-function(training_data, testing_data, formula){
  
  return(predictions)
}

ModelZion<-function(training_data, testing_data, formula){
  
  return(predictions)
}