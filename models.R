# File containing all models - each contributor has their own model

# Function to create a classifier and evaluate
# inputs:
# training_data - data frame - the data to train the model on
# testing_data - data frame - the data to evaluate the model on
# formula - R formula object - formula for model to use
ModelHarry<-function(training_data, testing_data, formula){
  # Example model
  logisticModel<-stats::glm(formula,data=training_data,family=quasibinomial)
  predictions<-predict(logisticModel, testing_data, type="response")
  
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
  h2o.init()
  training_data$Attrition <- as.factor(training_data$Attrition)
  inputs <- colnames(training_data[,-2])
  # print(training_data$attrition)
  target <- "Attrition"
  
  nbModel <- h2o.naiveBayes(x = inputs, y = target, training_frame = as.h2o(training_data), laplace = 0, nfolds = 5, seed = 64)
  
  nbPerformance <- h2o.performance(nbModel)
  
  print(nbPerformance)
  
  predictions <- h2o.predict(nbModel, newdata = as.h2o(testing_data))
  
  print(predictions)
  # return(predictions)
}

