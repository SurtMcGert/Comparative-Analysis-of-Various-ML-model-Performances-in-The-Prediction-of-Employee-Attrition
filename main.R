# R Script For Group Coursework

#  clear all objects in "global environment"
rm(list=ls())

# ************************************************
# Global Environment variables

DATASET_FILENAME        <- "dataset/HR_Analytics.csv"          # Name of input dataset file
OUTPUT_FIELD            <- "Attrition"             # Field name of the output class to predict
FIELDS_FOR_REMOVAL      <- list("DailyRate", 
                                "MaritalStatus", 
                                "EmployeeNumber", 
                                "JobInvolvement", 
                                "RelationshipSatisfaction",
                                "PerformanceRating",
                                "MonthlyIncome", 
                                "MonthlyRate", 
                                "PercentSalaryHike", 
                                "StandardHours") # the list of fields that need manually removing
ORDERED_FIELDS          <- list("Education", 
                                "EnvironmentSatisfaction", 
                                "JobLevel", 
                                "JobSatisfaction", 
                                "WorkLifeBalance", 
                                "BusinessTravel") # the list of fields that need marking as ordered symbolic

CONTINUOUS_FIELDS       <- list("XUFEFFAge", 
                                "DistanceFromHome",
                                "PerformanceWithCurrentManager",
                                "Age",
                                "TotalWorkingYears",
                                "TrainingTimesLastYear",
                                "YearsAtCompany",
                                "YearsInCurrentRole",
                                "YearsSinceLastPromotion",
                                "AgeJoined") # the list of fields that should be overriden as continuous

HOLDOUT                 <- 70                   # % split to create TRAIN dataset

SCALE_DATASET           <- TRUE                 # Set to true to scale dataset before ML stage
OUTLIER_CONF            <- 0.85                 # Confidence p-value for outlier detection

TYPE_DISCRETE           <- "DISCRETE"           # field is discrete (numeric)
TYPE_CONTINUOUS         <- "CONTINUOUS"         # field is continuous (numeric)
TYPE_SYMBOLIC           <- "SYMBOLIC"           # field is a string
TYPE_ORDERED_CATEGORICAL<- "ORDERED_CATEGORICAL"# field is a string where order is of some importance
TYPE_NUMERIC            <- "NUMERIC"            # field is initially a numeric
TYPE_IGNORE             <- "IGNORE"             # field is not encoded

DISCRETE_BINS           <- 1                    # Number of empty bins to determine discrete
MAX_LITERALS            <- 55                   # Maximum number of 1-hot encoding new fields

FOREST_SIZE       <- 1000                 # Number of trees in the forest


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
               "tidyverse",
               "reshape2",
               "car",
               "caret",
               "neuralnet",
               "dplyr",
               "glmnet",
               "e1071",
               "ROSE",
               "C50",
               "randomForest",
               "mlbench",
               "superml",
            "ranger")







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
      bestResults["threshold"] = threshold
      bestAccuracy = results["accuracy"]
    }
    toPlot<-rbind(toPlot,data.frame(x=threshold,precision=results$pgood,recall=results$TPR,accuracy=results$accuracy,fpr=results$FPR))
  }
  
  # toPlot$youdan<-toPlot$recall+(1-toPlot$fpr)-1
  toPlot$youdan<-toPlot$recall+(100 - toPlot$fpr)-100
  
  maxYoudan<-toPlot$x[which.max(toPlot$youdan)]
  
  toPlot$distance<-sqrt(((100-toPlot$recall)^2)+(toPlot$fpr^2))
  
  minEuclidean<-toPlot$x[which.min(toPlot$distance)]
  
  printMeasures(bestResults, paste(name, " (best results)"))
  
  # melt the data into long data
  toPlot <- melt(toPlot, id.var=1)
  
  print(toPlot %>% # our dataset
    group_by(variable) %>%  # group the data by variable
    mutate(min = toPlot$x[which.min(value)]) %>% # get the min of each metric
    mutate(max = toPlot$x[which.max(value)]) %>%  # get the max of each metric
    ungroup %>% #ungroup the data
    ggplot(aes(x, value, color = variable))+ # plot x on the x axis and % on the y axis, and give each variable a different colour
    xlim(0, 1)+ # set limits on the x axis
    ylim(0, 100)+
    geom_point(size = 2, alpha = 0.5)+ # set the point size to 5 and the transparancy of the fill colour to 0.5
    geom_smooth()+ # draw a smooth line over our points
    geom_vline(aes(xintercept = min), color="red")+ # draw a red vertical line at the minimum of each metric
    geom_vline(aes(xintercept = max), color="green")+ # draw a green vertical line at the maximum of each metric
    facet_wrap(~variable)+ # put the data for each variable in its own box
    theme_bw()+ # the black and white theme
    labs(x = "threshold", title = name) # set the title of the plot
  )
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
# fieldNameOutput - String - the name of the output field, effectively the field the model is going to try and predict
# fields - list - and optional list of field names that you want to use to make up the forumal instead of all the fields
# 
# returns: an R forumlar object
modelFormula<-function(dataset, fieldNameOutput, fields=list()){
  
  # if there is no list of fields
  if(length(fields) == 0) {
    # use all the variables to make the formula
    inputs <- paste(names(dataset)[which(names(dataset)!=fieldNameOutput)],collapse = "+")
  } else {
    # use the specified list of fields to make the formula
    # first check that all the listed fields are even in the dataset
    for(i in 1:length(fields)){
      print(fields[i])
      if(fields[i] %in% names(dataset)){
        # do nothing
      }
      else{
        stop("THE SOME OR ALL OF THE LIST OF FIELDS ARE NOT IN THE DATASET")
      }
    }
    inputs <- paste(fields, collapse = "+")
  }
  
  
  output<-paste(fieldNameOutput,"~")
  
  formular = as.formula(paste(output,inputs))
  
  return(formular)
  
}


# Function to create a classifier and evaluate
# inputs:
# training_data - data frame - the data to train the model on
# testing_data - data frame - the data to evaluate the model on
Model<-function(training_data,testing_data, plot_heading){
  # call the formula function
  formular<-modelFormula(dataset=training_data,fieldNameOutput=OUTPUT_FIELD)
  
  # Placeholder - change in testing and final implementation
  predictionNames <- c("harryMlpPredictions", "harrySvmPredictions", "chrisPredictions", "chrisSvmPredictions", "annaPredictions", "annaSvmPredictions", "melricPredictions", "melricSvmPredictions", "zionPredictions", "zionSvmPredictions")
  
  predictions <- c(
    ModelHarry(training_data, testing_data, formular),
    ModelChris(training_data, testing_data, formular),
    ModelAnna(training_data, testing_data, formular),
    ModelMelric(training_data, testing_data),
    ModelZion(training_data, testing_data, formular)
  )
  
  print(length(predictions))
  
  # Evaluate the models
  threshold<-0.7
  for (i in seq(1,length(predictionNames))){
    name <- predictionNames[i]
    probabilities <- predictions[[i]]
    print(paste(name))
    if(length(probabilities) > 0){
      name = paste(predictionNames[i], "/")
      name = paste(name, plot_heading)
      results<-evaluate(probs=probabilities, testing_data=testing_data, threshold=threshold)
      results<-displayPerformance(probs=probabilities,testing_data=testing_data, name=name)
    }
  }
}



# main method
main<-function(){
  
  print(paste("using dataset: ", DATASET_FILENAME))

  # read the dataset
  dataset<-readDataset(DATASET_FILENAME)
  
  # all ages of people who have left
  ages <- dataset$Age[dataset$Attrition == "Yes"]
  meanAge <- mean(ages)
  print("Mean age")
  print(meanAge)
  
  # formula to divide two columns and obtain a ratio
  # to understand the average time between promotions relative to the time spent with the current manager.
  # this ratio could provide insights into the frequency of promotions in relation to the duration of the 
  # current managerial relationship. For example, a higher ratio might suggest that employees tend to receive 
  # promotions more frequently in comparison to the time they spend with their current manager.
  divide <- function(val1, val2, threshold = 1) {
    epsilon <- 1e-10
    
    # Check if the value in val2 is below the threshold
    if (val2 < threshold) {
      # Handle cases where there is limited time with the current manager
      return(NA)  # You can choose a special value or handle it differently
    }
    
    # Calculate the performance ratio
    result <- (val1 + epsilon) / (val2 + epsilon)
    
    return(result)
  }
  
  # Standard subtraction formula used in combineOrDeriveFields
  # Used in creating AgeJoined created from the employee Age and the years they have worked at the company
  # Provides insights on how the age that they joined the company may affect their loyalty and work ethic toward the company
  # Also may provide an interesting trend on attrition
  subtract <- function(colName1, colName2, dataframe) {
    results <- colName1 - colName2
    return(results)
  }
  
  # clean data
  dataset <- cleanData(dataset, remove = FIELDS_FOR_REMOVAL)
  
  #View(dataset)
  
  # combine fields before removing any
  dataset <- combineOrDeriveFields(dataset, "YearsSinceLastPromotion", "YearsWithCurrManager", divide, "PerformanceWithCurrentManager", TRUE, threshold = 1)
  dataset <- combineOrDeriveFields(dataset, "Age", "YearsAtCompany", subtract, "AgeJoined", FALSE)
  
  # replace the NA values in this field PerformanceWithCurrentManager with its mean
  dataset$PerformanceWithCurrentManager[is.na(dataset$PerformanceWithCurrentManager)] <- mean(dataset$PerformanceWithCurrentManager, na.rm = TRUE)
  
  
  #determine each field type
  field_types<-getFieldTypes(dataset, continuousFields=CONTINUOUS_FIELDS, orderedFields=ORDERED_FIELDS)
  print(field_types)
 
  # plot our data
  plotData(dataset, OUTPUT_FIELD, field_types)
  prettyDataset(dataset)
  
  results<-data.frame(field=names(dataset),type=field_types)
  print(formattable::formattable(results))
  print("Results")
  print(results)
  
  
  # pre processing first dataset
  print("preprocessing first dataset")
  print("encoding continuous data")
  continuous<-as.data.frame(dataset[which(field_types==TYPE_CONTINUOUS)])
  print("removing outliers")
  continuousWithoutOutliers<-removeOutliers(continuous=continuous,confidence=OUTLIER_CONF)
  
  # z-scale
  zscaledWithoutOutliers<-as.data.frame(scale(continuousWithoutOutliers,center=TRUE, scale=TRUE))
  zscaled<-as.data.frame(scale(continuous,center=TRUE, scale=TRUE))
  
  # n the chosen classifier, the input values need to be scaled to [0.0,1.0]
  continuousWithoutOutliersReadyforML<-rescaleDataFrame(zscaledWithoutOutliers)
  continuousReadyforML<-rescaleDataFrame(zscaled)
  print(paste("without outliers", continuousWithoutOutliersReadyforML))
  print(paste("with outliers",continuousReadyforML))

  print("encoding non ordered categorical data")
  categoricalReadyforML<-oneHotEncode(dataset=dataset,field_types=field_types)
  
  
  # Combine the two sets of data that are read for ML
  combinedML<-cbind(continuousWithoutOutliersReadyforML,categoricalReadyforML)

  # process the ordered categorical fields
  print("encoding ordered categorical data")
  orderedCategoricalReadyforML<-encodeOrderedCategorical(dataset=dataset, field_types=field_types)
  
  # View(orderedCategoricalReadyforML)
  
  # combine the ordered categorical fields that are ready for ML
  combinedML<-cbind(combinedML, orderedCategoricalReadyforML)
  
  # View(combinedML)
  
  # the dataset for ML information
  print(paste("Fields=",ncol(combinedML)))
  
  # Create a TRAINING dataset using HOLDOUT% (e.g. 70) of the records
  
  # Randomise the entire data set
  combinedML<-combinedML[sample(nrow(combinedML)),]
  
  # Puts the two training and testing splits into a list
  splitList <- splitDataset(combinedML)
  
  # balance data
  splitList$train <- rebalance(splitList$train, "both", "Attrition")
  
  # Calling models
  Model(training_data = splitList$train, testing_data = splitList$test, plot_heading = "first dataset outliers removed")
  
  
  #pre processing second dataset
  combinedML<-cbind(continuousReadyforML,categoricalReadyforML)
  # combine the ordered categorical fields that are ready for ML
  combinedML<-cbind(combinedML, orderedCategoricalReadyforML)
  # Puts the two training and testing splits into a list
  splitList <- splitDataset(combinedML)
  
  # balance data
  splitList$train <- rebalance(splitList$train, "both", "Attrition")
  
  # Calling models
  Model(training_data = splitList$train, testing_data = splitList$test, plot_heading = "second dataset outliers kept")
  
  View(splitList$test)
  
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

