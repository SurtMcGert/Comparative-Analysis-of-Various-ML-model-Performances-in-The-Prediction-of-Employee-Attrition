# R Script For Group Coursework

#  clear all objects in "global environment"
rm(list=ls())

# ************************************************
# Global Environment variables

DATASET_FILENAME  <- "dataset/HR_Analytics.csv"          # Name of input dataset file
OUTPUT_FIELD      <- "Attrition"             # Field name of the output class to predict

HOLDOUT           <- 70                   # % split to create TRAIN dataset

SCALE_DATASET     <- TRUE                 # Set to true to scale dataset before ML stage
OUTLIER_CONF      <- 0.95                 # Confidence p-value for outlier detection

TYPE_DISCRETE     <- "DISCRETE"           # field is discrete (numeric)
TYPE_ORDINAL      <- "ORDINAL"            # field is continuous numeric
TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric
TYPE_IGNORE       <- "IGNORE"             # field is not encoded

DISCRETE_BINS     <- 6                    # Number of empty bins to determine discrete
MAX_LITERALS      <- 55                   # Maximum number of 1-hot encoding new fields

# ************************************************
# Define and then load the libraries used in this project

# Library from CRAN     Version
# ************************************************
# pacman	               0.5.1
# outliers	             0.14
# corrplot	             0.84
# MASS	                 7.3.53
# formattable 	         0.2.0.1
# stats                  4.0.3
# PerformanceAnalytics   2.0.4

LIBRARIES<-c("outliers",
               "corrplot",
               "MASS",
               "formattable",
               "stats",
               "PerformanceAnalytics",
               "tidyverse")







# function to generate a plot from a models predictions
# using a dataset
# inputs:
# probs - vector double - probability of being class 1
# testing_data - data frame - dataset to evaluate
# name - string - the name of the plot
displayPerformance<-function(probs,testing_data, name){
  
  print("plotting performance")
  
  toPlot<-data.frame()
  bestAccuracy = 0
  bestResults <- list()
  
  #Vary the threshold
  for(threshold in seq(0,1,by=0.01)){
    results<-evaluate(probs=probs,testing_data=testing_data,threshold=threshold)
    if(as.numeric(results["accuracy"]) > as.numeric(bestAccuracy)){
      bestResults = results
      bestAccuracy = results["accuracy"]
    }
    toPlot<-rbind(toPlot,data.frame(x=threshold,fpr=results$FPR,tpr=results$TPR))
  }
  
  toPlot$youdan<-toPlot$tpr+(1-toPlot$fpr)-1
  
  maxYoudan<-toPlot$x[which.max(toPlot$youdan)]
  
  toPlot$distance<-sqrt(((100-toPlot$tpr)^2)+(toPlot$fpr^2))
  
  minEuclidean<-toPlot$x[which.min(toPlot$distance)]
  
  printMeasures(bestResults, name)
  
  plot(x=toPlot$x,y=toPlot$tpr, type="l",col="blue",
       xlab="Threshold",
       ylab="%Rate",
       main=name)
  
  # Plot the Euclidean distance to "perfect" classifier (smallest the best)
  lines(toPlot$x,toPlot$distance,type="l",col="green")
  abline(v=minEuclidean,col="green",lty=3,lwd=2)
  
  # Plot the specificity (1-FPR)
  lines(x=toPlot$x,y=100-toPlot$fpr,type="l",col="red",lwd=3,lty=1)
  
  # The point where specificity and sensitivity are the same
  crosspoint<-toPlot$x[which(toPlot$tpr<(100-toPlot$fpr))[1]]
  abline(v=crosspoint,col="red",lty=3,lwd=2)
  
  lines(toPlot$x,toPlot$youdan,type="l",col="purple",lwd=2,lty=3)
  abline(v=maxYoudan,col="purple",lty=3,lwd=2)
  
  legend("bottom",c("TPR","1-FPR","Distance","Youdan"),col=c("blue","red","green","purple"),lty=1:2,lwd=2)
  text(x=0,y=50, adj = c(-0.2,2),cex=1,col="black",paste("THRESHOLDS:\nEuclidean=",minEuclidean,"\nYoudan=",maxYoudan))
}



# function to evaluate the accuracy of the model
# inputs:
# probs - vector double - probability of being class 1 output from the model
# testing_data - data frame - the dataset to evaluate against
# threshold - double - cutoff (probability for the classifier)
# 
# returns: list of named evaluation measures and predicted class probabilities
evaluate<-function(probs,testing_data,threshold) {
  
  predictedClass<-ifelse(as.numeric(probs)<threshold,0,1)
  expectedClass<-testing_data[,OUTPUT_FIELD]
  
  results<-calculateConfusionMatrix(expectedClass=expectedClass,
                          predictedClass=predictedClass)
  
  return(results)
}


# function to create a formula for column names and given output
# inputs:
# dataset - data frame - the dataset
# fieldNameOutput - String - the name of the output field, effectivly the field the model is going to try and predict
# 
# returns: an R forumlar object
modelFormula<-function(dataset,fieldNameOutput){
  
  inputs<-paste(names(dataset)[which(names(dataset)!=fieldNameOutput)],collapse = "+")
  
  output<-paste(fieldNameOutput,"~")
  
  formular=as.formula(paste(output,inputs))
  
  return(formular)
  
}


# Function to create a classifier and evaluate
# inputs:
# training_data - data frame - the data to train the model on
# testing_data - data frame - the data to evaluate the model on
Model<-function(training_data,testing_data){
  # call the formula function
  formular<-modelFormula(dataset=training_data,fieldNameOutput=OUTPUT_FIELD)
  
  
  # Placeholder - change in testing and final implementation
  predictionNames <- c("harryPredictions", "chrisPredictions", "annaPredictions", "melricPredictions", "zionPredictions")
  
  predictions <- list(
    ModelHarry(training_data, testing_data, formular),
    ModelChris(training_data, testing_data, formular),
    ModelAnna(training_data, testing_data, formular),
    ModelMelric(training_data, testing_data, formular),
    ModelZion(training_data, testing_data, formular)
  )
  
  # Evaluate the models
  threshold<-0.7
  for (i in seq(1,length(predictionNames))){
    name <- predictionNames[i]
    probabilities <- predictions[[i]]
    print(paste(name))
    if(length(probabilities) > 0){
      results<-evaluate(probs=probabilities, testing_data=testing_data, threshold=threshold)
      results<-displayPerformance(probs=probabilities,testing_data=testing_data, name=predictionNames[i])
    }
  }
}



# main method
main<-function(){
  
  print(paste("using dataset: ", DATASET_FILENAME))

  
  # read the dataset
  dataset<-readDataset(DATASET_FILENAME)
  columnsToRemove <- list("MaritalStatus", "EmployeeNumber", "JobInvolvement", "PerformanceRating", "RelationshipSatisfaction", "YearsWithCurrManager")
  dataset <- cleanData(dataset, remove = columnsToRemove)
  #determine each field type
  field_types<-getFieldTypes(dataset)
  
  #plot our data
  plotData(dataset, OUTPUT_FIELD)
  
  

  
  
 
  
  #numeric_fields<-names(dataset)[field_types=="NUMERIC"]
  #symbolic_fields<-names(dataset)[field_types=="SYMBOLIC"]
  
 # number_of_numeric<-length(numeric_fields)
 # number_of_symbolic<-length(symbolic_fields)
  
  #print(paste("NUMERIC FIELDS=",number_of_numeric))
 # print(numeric_fields)
 # print(paste("SYMBOLIC FIELDS=",number_of_symbolic))
 # print(symbolic_fields)
  
  
  results<-data.frame(field=names(dataset),initial=field_types,types1=field_types)
  print(formattable::formattable(results))
  
  
  ordinals<-dataset[,which(field_types==TYPE_ORDINAL)]
  ordinals<-removeOutliers(ordinals=ordinals,confidence=OUTLIER_CONF)
  
  # z-scale
  zscaled<-as.data.frame(scale(ordinals,center=TRUE, scale=TRUE))
  
  # n the chosen classifier, the input values need to be scaled to [0.0,1.0]
  ordinalReadyforML<-rescaleDataFrame(zscaled)
  
  # Process the catagorical (symbolic/discrete) fields using 1-hot-encoding
  catagoricalReadyforML<-oneHotEncode(dataset=dataset,field_types=field_types)
  
  print(formattable::formattable(data.frame(fields=names(catagoricalReadyforML))))
  
  # number of non-numeric fields before transformation
  #nonNumericbefore<-length(which(field_types!=TYPE_ORDINAL))
  
  #nonNumerictranformed<-ncol(catagoricalReadyforML)
  #print(paste("Symbolic fields. Before encoding=",nonNumericbefore,"After",nonNumerictranformed))
  
  
  # Combine the two sets of data that are read for ML
  combinedML<-cbind(ordinalReadyforML,catagoricalReadyforML)
  
  # The dataset for ML information
  # print(paste("Fields=",ncol(combinedML)))
  # combinedML<-removeRedundantFields(dataset=combinedML,cutoff=OUTLIER_CONF)
  
  #The dataset for ML information
  print(paste("Fields=",ncol(combinedML)))
  
  # Create a TRAINING dataset using HOLDOUT% (e.g. 70) of the records
  
  # Randomise the entire data set
  combinedML<-combinedML[sample(nrow(combinedML)),]
  
  # Create a TRAINING dataset using first HOLDOUT% of the records
  # and the remaining 30% is used as TEST
  # use ALL fields (columns)
  training_records<-round(nrow(combinedML)*(HOLDOUT/100))
  training_data <- combinedML[1:training_records,]
  testing_data = combinedML[-(1:training_records),]
  
  
  
  Model(training_data = training_data, testing_data = testing_data)
  
  
}


# clear the console area
cat("\014")

# Loads the libraries
library(pacman)
pacman::p_load(char=LIBRARIES,install=TRUE,character.only=TRUE)


# Load additional R script files
debugSource("dataPrep.R")
debugSource("models.R")

set.seed(123)

main()

print("finished")

