# File containing all models - each contributor has their own model

# Function to create a classifier and evaluate
# inputs:
# training_data - data frame - the data to train the model on
# testing_data - data frame - the data to evaluate the model on
# formula - R formula object - formula for model to use
ModelHarry<-function(training_data, testing_data, formula){
  predictions <- list()
  return(predictions)
}

ModelChris<-function(training_data, testing_data, formula){
  predictions <- list()
  return(predictions)
}

ModelAnna<-function(training_data, testing_data, FOREST_SIZE, OUTPUT_FIELD, plot=TRUE){
  
  myTitle<-(paste("Preprocessed Dataset. Random Forest=",FOREST_SIZE,"trees"))
  print(myTitle)
  
  positionClassOutput<-which(names(training_data)==OUTPUT_FIELD)
  
  # train data: dataframe with the input fields
  train_inputs<-training_data[-positionClassOutput]
  
  # train data: vector with the expedcted output
  train_expected<-training_data[,positionClassOutput]
  
  rf<-randomForest::randomForest(train_inputs,
                                 factor(train_expected),
                                 ntree=FOREST_SIZE ,
                                 importance=TRUE,
                                 mtry=sqrt(ncol(train_inputs)))
  
  
  # ************************************************
  # Use the created decision tree with the test dataset
  measures<-getTreeClassifications(myTree = rf,
                                   testDataset = testing_data,
                                   title=myTitle,
                                   plot=plot,
                                   OUTPUT_FIELD=OUTPUT_FIELD)
  
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
  
  return(measures)
}

ModelMelric<-function(training_data, testing_data, formula){
  predictions <- list()
  return(predictions)
}

ModelZion<-function(training_data, testing_data, formula){
  predictions <- list()
  return(predictions)
}


getTreeClassifications<-function(myTree,
                                 testDataset,
                                 title,
                                 classLabel=1,
                                 plot=TRUE,
                                 OUTPUT_FIELD){
  
  positionClassOutput=which(names(testDataset)==OUTPUT_FIELD)
  print(positionClassOutput)
  
  #test data: dataframe with with just input fields
  test_inputs<-testDataset[-positionClassOutput]
  
  # Generate class membership probabilities
  # Column 1 is for class 0 (bad loan) and column 2 is for class 1 (good loan)
  
  testPredictedClassProbs<-predict(myTree,test_inputs, type="prob")
  
  # Get the column index with the class label
  classIndex<-which(as.numeric(colnames(testPredictedClassProbs))==classLabel)
  
  # Get the probabilities for classifying attrition
  # test_predictedProbs<-testPredictedClassProbs[,classIndex]
  
  #test data: vector with just the expected output class
  test_expected<-testDataset[,positionClassOutput]
  
  predictedLabels <- colnames(testPredictedClassProbs)[apply(testPredictedClassProbs,1,which.max)]
  
  # measures<-NdetermineThreshold(test_expected=test_expected,
  #                               test_predicted=test_predictedProbs,
  #                               plot=plot,
  #                               title=title)
  
  # if (plot==TRUE)
  #   NprintMeasures(results=measures,title=title)
  # 
  # return(measures)
  
  confusionMatrix <- confusionMatrix(as.factor(test_expected), as.factor(predictedLabels))
  
  print(confusionMatrix)
  
  return(test_predictedProbs)
} #endof getTreeClassifications()