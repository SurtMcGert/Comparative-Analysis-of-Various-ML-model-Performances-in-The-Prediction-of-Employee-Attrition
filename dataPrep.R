# The majority of these functions are from the labs, they have been renamed, some bits slightly re-written and re-commented.
# some new functions have been added as well







# Pre-Processing a Dataset functions
# This will store $name=field name, $type=field type
manualTypes <- data.frame()

# function to scale a vector between 0 and 1
# input:
# input - vector - values to be scaled
# 
# returns: a scaled vector
rescale<-function(input){

  minv<-min(input)
  maxv<-max(input)
  return((input-minv)/(maxv-minv))
}


# function to rescale an entire dataframe between 0 and 1
# input:
# dataset - data frame - the data frame to scale
# 
# returns: a scaled data frame
rescaleDataFrame<-function(dataset){

  scaled<-sapply(as.data.frame(dataset),rescale)
  return(scaled)
}


# function to remove punctuation from a string
# input: 
# input - string - the string to remove the punctuation from
# 
# returns: a string with no punctuation
removePunctuation<-function(input){
  return(gsub("[[:punct:][:blank:]]+", "", input))
}

# Function to read a dataset from the working directory
# input:
# csvFilename - the file name of the dataset to read
#
# returns: data frame - the contents of the CSV file
readDataset<-function(csvFilename){
  
  # read the dataset
  dataset<-read.csv(csvFilename,encoding="UTF-8",stringsAsFactors = FALSE)
  
  # read the names of all the variables in the dataset and remove any punctuation
  names(dataset)<-removePunctuation(names(dataset))
  
  # print a confirmation
  print(paste("CSV dataset",csvFilename,"has been read. Records=",nrow(dataset)))
  
  # return the data frame
  return(dataset)
}


# function to set each field for NUMERIC or SYNBOLIC
# inputs:
# name - String - name of the field to manually set
# type - String - the type of this field to set
setInitialFieldType<-function(name,type){

  #Sets in the global environment
  manualTypes<<-rbind(manualTypes,data.frame(name=name,type=type,stringsAsFactors = FALSE))
}


# function to get if each field is either NUMERIC or SYMBOLIC
# input:
# dataset - data frame - the data to get the field types of
# 
# returns: vector of types {NUMERIC, SYBOLIC}
getFieldTypes<-function(dataset){

  field_types<-vector()
  for(field in 1:(ncol(dataset))){

    entry<-which(manualTypes$name==names(dataset)[field])
    if (length(entry)>0){
      field_types[field]<-manualTypes$type[entry]
      next
    }

    if (is.numeric(dataset[,field])) {
      field_types[field]<-TYPE_NUMERIC
    }
    else {
      field_types[field]<-TYPE_SYMBOLIC
    }
  }
  field_types <- getDiscreteOrOrdinal(dataset=dataset,
                                      field_types=field_types,
                                      cutoff=DISCRETE_BINS)
  return(field_types)
}


# function to test if a NUMERIC field is DISCRETE or ORDINAL
# inputs:
# dataset - data frame - the dataset to test
# field_types - vector strings - the types of each field, {NUMERIC, SYMBOLIC}
# cutoff - int - the number of empty bins needed to determine discrete (1 - 10)
# 
# returns: a vector of strings updated wit types per field {DISCRETE, ORDINAL}
getDiscreteOrOrdinal<-function(dataset,field_types,cutoff){

  #For every field in our dataset
  for(field in 1:(ncol(dataset))){

    #Only for fields that are all numeric
    if (field_types[field]==TYPE_NUMERIC) {

      histogramAnalysis<-hist(dataset[,field], breaks = 10, plot=FALSE)
      bins<-histogramAnalysis$counts/length(dataset[,field])*100  # Convert to %

      graphTitle<-"AUTO:"

      #If the number of bins with less than 1% of the values is greater than the cutoff
      #then the field is deterimed to be a discrete value

      if (length(which(bins<1.0))>cutoff)
        field_types[field]<-TYPE_DISCRETE
      else
        field_types[field]<-TYPE_ORDINAL

      #Type of field is the chart name
      hist(dataset[,field], breaks = 10, plot=TRUE,
           main=paste(graphTitle,field_types[field]),
           xlab=names(dataset[field]),ylab="Number of Records",
           yaxs="i",xaxs="i",border = NA)

    }
  }
  return(field_types)
}


# function to transform SYMBOLIC or DISCRETE fields using 1-hot-encoding
# inputs:
# dataset - data frame - the dataset whos fields need encoding
# field_types - vector string - types per field {ORDINAL, SYMBOLIC, DISCRETE}
# 
# returns: data frame - transofmed dataset
oneHotEncode<-function(dataset,field_types){

  catagorical<-data.frame()

  categorical_fields<-names(dataset)[which(field_types==TYPE_SYMBOLIC | field_types==TYPE_DISCRETE)]

  # for each field
  for (field in categorical_fields){

    # Convert into factors. A level for each unique string
    ffield<-factor(dataset[,field])

    # Check if too many unique values to encode
    if (nlevels(ffield) > MAX_LITERALS) {
      stop(paste("Practical Business Analytics - too many literals in:",
                 field,
                 nlevels(ffield)))
    }

    # Check if just one value!
    if (nlevels(ffield) ==1) {
      stop(paste("Practical Business Analytics - field stuck at a single value:",
                 field))
    }

    # 1-hot encoding. A new column for each unique "level"
    xx<-data.frame(model.matrix(~ffield+0, data=ffield))

    names(xx)<-gsub("ffield",field,names(xx))

    # If 2 unique values, then can encode as a single "binary" column
    if (ncol(xx)==2){
      xx<-xx[,-2,drop=FALSE]
      names(xx)<-field  # Field name without the value appended
    }

  catagorical<-as.data.frame(append(catagorical,xx))

  }
  return (catagorical)

}


# function to clean data by removing symbolic fields that only have one value and deleting a given list of variables
# inputs:
# dataset - data frame - the dataset whos fields need encoding
# remove - list - a list of column names, to remove from the dataset
# 
# returns: data frame - cleaned dataset
cleanData<-function(dataset, remove = list()){
  print("cleaning data")
  cleanedData <- dataset
  markedColumns <- list()
  
  
  #find the columns from the given list to remove
  for(field in remove){
    print(paste("removing ", field))
    markedColumns <- append(markedColumns, field)
  }
  
  # find columns that have the same value in every row
  #For every field in our dataset
  for(field in 1:(ncol(dataset))){
    # Convert into factors. A level for each unique string
    ffield<-factor(dataset[,field])
    # Check if just one value!
    if (nlevels(ffield) ==1) {
      #mark column for removal
      markedColumns <- append(markedColumns, names(dataset[field]))
      print(paste("removing ", names(dataset[field])))
    }
  }
  
  
  
  
  #remove columns
  cleanedData <- dataset[,!(names(dataset) %in% markedColumns)]
  print("cleaned data")
  return (cleanedData)
}



# function to plot a scatter plot of vield values and colour the outliers in red
# inputs:
# sorted - vector - points to plot as literal values
# outliers - vector - list of above points that are considered outliers
# fieldName - string - name of field to plot
plotOutliers<-function(sorted,outliers,fieldName){

  plot(1:length(sorted),sorted,pch=1,xlab="Unique records",ylab=paste("Sorted values",fieldName),bty="n")
  if (length(outliers)>0)
    points(outliers,sorted[outliers],col="red",pch=19)
}


# function to plot PLOT_correlagram
# inputs:
# cr - data fram - n x n frame of correlation coeficients
PLOT_correlagram<-function(cr){

  #Defines the colour range
  col<-colorRampPalette(c("green", "red"))

  #To fir on screen, convert field names to a numeric
  rownames(cr)<-1:length(rownames(cr))
  colnames(cr)<-rownames(cr)

  corrplot::corrplot(abs(cr),method="square",
                     order="FPC",
                     cl.ratio=0.2,
                     cl.align="r",
                     tl.cex = 0.6,cl.cex = 0.6,
                     cl.lim = c(0, 1),
                     mar=c(1,1,1,1),bty="n")
}

# function to remove redundant fields using linear correlation, data will be lost so use with caution
# inputs:
# dataset - data frame - the dataset to work on, must be numeric values only
# cutoff - double - value above which is determined redundant [0, 1]
# 
# returns: dataset with redundant fields removed
removeRedundantFields<-function(dataset,cutoff){

  print(paste("Before redundancy check Fields=",ncol(dataset)))

  #Remove any fields that have a stdev of zero (i.e. they are all the same)
  xx<-which(apply(dataset, 2, function(x) sd(x, na.rm=TRUE))==0)+1

  if (length(xx)>0L)
    dataset<-dataset[,-xx]

  #Kendall is more robust for data do not necessarily come from a bivariate normal distribution.
  cr<-cor(dataset, use="everything")
  #cr[(which(cr<0))]<-0 #Positive correlation coefficients only
  PLOT_correlagram(cr)

  correlated<-which(abs(cr)>=cutoff,arr.ind = TRUE)
  list_fields_correlated<-correlated[which(correlated[,1]!=correlated[,2]),]

  if (nrow(list_fields_correlated)>0){

    print("Following fields are correlated")
    print(list_fields_correlated)

    # 240220nrt print list of correlated fields as namesß
    for (i in 1:nrow(list_fields_correlated)){
      print(paste(names(dataset)[list_fields_correlated[i,1]],"~", names(dataset)[list_fields_correlated[i,2]]))
    }

    #We have to check if one of these fields is correlated with another as cant remove both!
    v<-vector()
    numc<-nrow(list_fields_correlated)
    for (i in 1:numc){
      if (length(which(list_fields_correlated[i,1]==list_fields_correlated[i:numc,2]))==0) {
        v<-append(v,list_fields_correlated[i,1])
      }
    }
    print("Removing the following fields")
    print(names(dataset)[v])

    return(dataset[,-v]) #Remove the first field that is correlated with another
  }
  return(dataset)
}


# function to determine if a value of a record is an outlier
# inputs:
# ordinals - data frame - numeric fields only
# confidence - double - confidence above which is determined as an outlier, set negative if you dont want to replace outliers
# 
# returns: a data frame of ordinals with any outlier values optionally replaced with the median of the field
removeOutliers<-function(ordinals,confidence){

  #For every ordinal field in our dataset
  for(field in 1:(ncol(ordinals))){

    sorted<-unique(sort(ordinals[,field],decreasing=TRUE))
    outliers<-which(outliers::scores(sorted,type="chisq",prob=abs(confidence)))
    plotOutliers(sorted,outliers,colnames(ordinals)[field])

    #If found records with outlier values
    if ((length(outliers>0))){

      #070819NRT If confidence is positive then replace values with their means, otherwise do nothing
      if (confidence>0){
        outliersGone<-rm.outlier(ordinals[,field],fill=TRUE)
        sorted<-unique(sort(outliersGone,decreasing=TRUE))
        #NplotOutliers(sorted,vector(),colnames(ordinals)[field])
        ordinals[,field]<-outliersGone #Put in the values with the outliers replaced by means
        print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers),"Replaced with MEAN"))
      } else {
        print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers)))
      }
    }

  }
  return(ordinals)
}


# function to output measures
# inputs:
# results - list - the results from a confusion matrix
# name - string - the name of the metrics
printMeasures<-function(results, name){

  tidyTable<-data.frame(t(t(results)))
  names(tidyTable)[1]<-name

  t<-formattable::formattable(tidyTable,list(
    TP = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",TP)),
    FN = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",FN)),
    TN = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",TN)),
    FP = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",FP))))
  print(t)
}

# function to plot a confusion matrix
# inputs:
# results - list - results from a confusion matrix
plotConfusionMatrix<-function(results){

  aa<-matrix(c(round(results$TP,digits=0),
               round(results$FN,digits=0),
               round(results$FP,digits=0),
               round(results$TN,digits=0)),
             nrow=2)
  row.names(aa)<-c("Fraud","Genuine")
  colnames(aa)<-c("Fraud","Genuine")
  fourfoldplot(aa,color=c("#cc6666","#99cc99"),
               conf.level=0,
               margin=2,
               main="TP  FP / FN   TN")
}


# function to calculate the RMSE
# inputs:
# actual_y - vector - numbers indicating the known classes
# y_predicted - vector - the predicted classes from a model
# 
# returns: the RMSE
calculateRMSE<-function(actual_y,y_predicted){

  return(sqrt(mean((actual_y-y_predicted)^2)))
}


# function to calculate the following measures:
#        TP        - double - True Positive records
#        FP        - double - False Positive records
#        TN        - double - True Negative records
#        FN        - double - False Negative records
#        accuracy  - double - accuracy measure
#        pgood     - double - precision for "good" (values are 1) measure
#        pbad      - double - precision for "bad" (values are 1) measure
#        FPR       - double - FPR measure
#        TPR       - double - FPR measure
#        TNR       - double - TNR measure
#        MCC       - double - Matthew's Correlation Coeficient
# inputs:
# TP - numeric - true positive records
# FN - numeric - false negative records
# FP - numeric - false positive records
# TN - numeric - true negative records
# 
# returns: a list with all the measures
calculateMeasures<-function(TP,FN,FP,TN){

  retList<-list(  "TP"=TP,
                  "FN"=FN,
                  "TN"=TN,
                  "FP"=FP,
                  "accuracy"=100.0*((TP+TN)/(TP+FP+FN+TN)),
                  "pgood"=   100.0*(TP/(TP+FP)),
                  "pbad"=    100.0*(TN/(FN+TN)),
                  "FPR"=     100.0*(FP/(FP+TN)),
                  "TPR"=     100.0*(TP/(TP+FN)),
                  "TNR"=     100.0*(TN/(FP+TN)),
                  "MCC"=     ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  )
  return(retList)
}

# function to calculate a confusion matrix
#                    ACTUAL
#               ------------------
# PREDICTED     FRAUD   |  GENUINE
#               ------------------
#     FRAUD      TP     |    FP
#               ==================
#     GENUINE    FN     |    TN
# inputs:
# expectedClass - vector - expected outcome from each row
# predictedClass - vector - predicted outcome from each row
#
# returns: a list with the TP, FP, FN, TN
calculateConfusionMatrix<-function(expectedClass,predictedClass){

  confusion<-table(factor(predictedClass,levels=0:1),factor(expectedClass,levels=0:1))

  # This "converts" the above into our preferred format

  TP<-as.double(confusion[2,2])
  FN<-as.double(confusion[1,2])
  FP<-as.double(confusion[2,1])
  TN<-as.double(confusion[1,1])

  return(calculateMeasures(TP,FN,FP,TN))

}

# function to split a datset into training and testing sets after randomising the data
# intputs:
# combinedML - data frame - the dataset to split
# 
# returns: a list of two data frames, one for the training and one for the testing dataset
splitDataset<-function(combinedML){

  # **** Create a TRAINING dataset using 70% of the records

  combinedML<-combinedML[order(runif(nrow(combinedML))),]
  training_records<-round(nrow(combinedML)*(70/100))

  train <- 1:training_records
  test <- -train

  training_data <- combinedML[train,]
  testing_data = combinedML[test,]

  retList<-list("train"=training_data,
                "test"=testing_data)
  return(retList)
}

# function to print an analysis of a dataset
# inputs:
# dataset - data frame - the dataset to analyse
# string - OPTIONAL string which is used as the table header
# prettyDataset<-function(dataset,...){
prettyDataset<-function(dataset,...){
  params <- list(...)

  tidyTable<-data.frame(Field=names(dataset),
                        Catagorical=FALSE,
                        Symbols=0,
                        Name=0,
                        Min=0.0,
                        Mean=0.0,
                        Max=0.0,
                        Skew=0.0,
                        stringsAsFactors = FALSE)

  if (length(params)>0){
    names(tidyTable)[1]<-params[1]
  }

  for (i in 1:ncol(dataset)){
    isFieldAfactor<-!is.numeric(dataset[,i])
    tidyTable$Catagorical[i]<-isFieldAfactor
    if (isFieldAfactor){
      tidyTable$Symbols[i]<-length(unique(dataset[,i]))  #Number of symbols in catagorical
      #Gets the count of each unique symbol
      symbolTable<-sapply(unique(dataset[,i]),function(x) length(which(dataset[,i]==x)))
      majoritySymbolPC<-round((sort(symbolTable,decreasing = TRUE)[1]/nrow(dataset))*100,digits=0)
      tidyTable$Name[i]<-paste(names(majoritySymbolPC),"(",majoritySymbolPC,"%)",sep="")
    } else
    {
      tidyTable$Max[i]<-round(max(dataset[,i]),2)
      tidyTable$Mean[i]<-round(mean(dataset[,i]),2)
      tidyTable$Min[i]<-round(min(dataset[,i]),2)
      tidyTable$Skew[i]<-round(PerformanceAnalytics::skewness(dataset[,i],method="moment"),2)
    }
  }

  #Sort table so that all numerics are first
  t<-formattable::formattable(tidyTable[order(tidyTable$Catagorical),],
                              list(Catagorical = formatter("span",style = x ~ style(color = ifelse(x,"green", "red")),
                                                           x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))),
                                   Symbols = formatter("span",style = x ~ style(color = "black"),x ~ ifelse(x==0,"-",sprintf("%d", x))),
                                   Min = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Min, nsmall=2, big.mark=","))),
                                   Mean = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",format(Mean, nsmall=2, big.mark=","))),
                                   Max = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Max, nsmall=2, big.mark=","))),
                                   Skew = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",sprintf("%.2f", Skew)))
                              ))
  print(t)
}


# function to plot a dataset
# inputs:
# dataset - dataset to plot
# fieldNameOutput - name of variable whos relation you want to see against other variables
# fieldTypes - a list of the types of each field {ORDINAL, SYMBOLIC, DISCRETE}
plotData <- function(data, fieldNameOutput, fieldTypes){
  # for each variable in the data
  for(i in 1:ncol(data)){
    # a scatter plot of the variable against the fieldNameOutput
    name <- names(data)[i]
    title <- paste(name, "vs")
    title <- paste(title, fieldNameOutput)
    print(title)
    print(data %>% 
      ggplot(aes(x = !!sym(name), y = !!sym(fieldNameOutput)))+
      geom_point(size = 5, alpha = 0.1)+
      theme_bw()+
      labs(title = title))
  
  # a density plot of the variable if it is continuous
    if(fieldTypes[i] == TYPE_ORDINAL){
      print(data %>% 
              ggplot(aes(x = !!sym(name), color = name, fill = name))+
              geom_density(alpha = 0.2)+
              theme_bw()+
              labs(title = name))
    }
    else{
      # a histogram of the variable if it is symbolic or discrete
      print(data %>% 
              ggplot(aes(x = !!sym(name), color = name, fill = name))+
              geom_bar()+
              theme_bw()+
              labs(title = name))
    }
  }
}



