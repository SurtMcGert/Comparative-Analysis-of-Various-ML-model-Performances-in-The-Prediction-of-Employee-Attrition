# ************************************************
# This work is licensed under a Creative Commons
# Attribution-NonCommercial 4.0 International License.
# ************************************************
# ************************************************
#  PRACTICAL BUSINESS ANALYTICS
#  MEACHINE LEARNING & VISULISATIONS
#
# Practical Business Analytics
# Dept. of Computer Science
# University of Surrey
# GUILDFORD
# Surrey GU2 7XH
#
# UPDATE
# 1.00      1/2/2017    Initial Version
# 1.01      14/2/2018   NPREPROCESSING_discreteNumeric - changed for total of ten bins
#                       Added bin number as x-axis label
# 1.02      26/2/2018   Added NcalcMeasures(), updated NcalcConfusion() to use this.
#                       Updated NPREPROCESSING_splitdataset() with descripition
#                       Updated Npreprocessdataset() with scaleFlag
# 1.03      2/3/2018    Renamed Npreprocessdataset() to NPREPROCESSING_dataset()
#                       Added library(neuralnet) inside NLEARN_BasicNeural()
# 1.04      5/3/2018    Added global constants - although these values should be PASSED
#                       NPREPROCESSING_dataset() added vector manualTypes
#                       Updated all functions to include manualTypes where required
#                       Added NPREPROCESSING_setInitialFieldType()
#                       Added MAX_LITERALS as a constant
#                       Added NPREPROCESSING_removeAllSameValueField()
#                       NPREPROCESSING_categorical() does not convert very sparse fields
#                       Removed N_LEARN_BasicNeural() and put in lab functions source
# 1.05      7/3/2018    NPREPROCESSING_discreteNumeric() updated to show plot for all numeric fields
#                       & title on plot shows if field type manually set or determined by preprocessing
# 1.06      18/2/2019   For lab 3
# 1.07      19/2/2019   Fixed NPREPROCESSING_discreteNumeric() with scale
# 1.08      24/2/2019   Updated NcalcConfusion()
#                       Added NROCgraph(),NPREPROCESSING_prettyDataset()
#           9/3/2019    Updated manual setting of field types
#           6/8/2019    NPREPROCESSING_categorical() added "_" to make field name easier to read
#                       NPREPROCESSING_dataset() fixed scaling to do each column seperatley
#                       pass confidence as parameter for outlier detection
#           7/8/2019    NPREPROCESSING_outlier() - if negative confidence then do not replace outlier
#                       NROCgraph() - updated as the pROC library had changed
#                       NcalcConfusion() - convert values to doubles to avoid integers overflowing
#                       NprintMeasures() - output to viewer only
#           8/8/2019    NplotConfusion() - added new function for pretty confusion chart
#           21/8/2019   Removed border from the plot
# 1.09      17/10/2019  Uddates for PBA lab 3
# 1.10      22/10/2019  Nrescaleentireframe() return the scaled values in an object
#                       NPLOT_correlagram() plot abs() value correlations only
# 1.20      24/2/2020   NPREPROCESSING_redundantFields() output field names to console
# 1.21      12/10/2020  NPREPROCESSING_discreteNumeric() use hist() to look at 10 bins
# 1.22      26/11/2020   Changed "discreet" to "discrete" [woops!]
# 1.23      18/2/2021   NPREPROCESSING_discreteNumeric() neater plotting
#                       NPREPROCESSING_categorical() re-write for efficiency
# ************************************************

# Pre-Processing a Dataset functions

# To manually set a field type
# This will store $name=field name, $type=field type
manualTypes <- data.frame()

# ************************************************
# Nrescale() :
#
# These are the real values, that we scale between 0-1
# i.e. x-min / (max-min)
#
# INPUT:   vector - input - values to scale
#
# OUTPUT : vector - scaled values to [0.0,1.0]
# ************************************************
Nrescale<-function(input){

  minv<-min(input)
  maxv<-max(input)
  return((input-minv)/(maxv-minv))
}

# ************************************************
# Nrescaleentireframe() :
#
# Rescle the entire dataframe to [0.0,1.0]
#
# INPUT:   data frame - dataset - numeric data frame
#
# OUTPUT : data frame - scaled numeric data frame
# ************************************************
Nrescaleentireframe<-function(dataset){

  scaled<-sapply(as.data.frame(dataset),Nrescale)
  return(scaled)
}

# ************************************************
# NPREPROCESSING_removePunctuation()
#
# INPUT: String - fieldName - name of field
#
# OUTPUT : String - name of field with punctuation removed
# ************************************************
NPREPROCESSING_removePunctuation<-function(fieldName){
  return(gsub("[[:punct:][:blank:]]+", "", fieldName))
}

# ************************************************
# NreadDataset() :
#
# Read a CSV file from working directory
#
# INPUT: string - csvFilename - CSV filename
#
# OUTPUT : data frame - contents of the headed CSV file
# ************************************************
NreadDataset<-function(csvFilename){

  dataset<-read.csv(csvFilename,encoding="UTF-8",stringsAsFactors = FALSE)

  # The field names "confuse" some of the library algorithms
  # As they do not like spaces, punctuation, etc.
  names(dataset)<-NPREPROCESSING_removePunctuation(names(dataset))

  print(paste("CSV dataset",csvFilename,"has been read. Records=",nrow(dataset)))
  return(dataset)
}

# ************************************************
# NPREPROCESSING_setInitialFieldType() :
#
# Set  each field for NUMERIC or SYNBOLIC
#
# INPUT:
#        String - name - name of the field to manually set
#        String - type - manual type
#
# OUTPUT : None
# ************************************************
NPREPROCESSING_setInitialFieldType<-function(name,type){

  #Sets in the global environment
  manualTypes<<-rbind(manualTypes,data.frame(name=name,type=type,stringsAsFactors = FALSE))
}

# ************************************************
# NPREPROCESSING_initialFieldType() :
#
# Test each field for NUMERIC or SYNBOLIC
#
# INPUT: Data Frame - dataset - data
#
# OUTPUT : Vector - Vector of types {NUMERIC, SYMBOLIC}
# ************************************************
NPREPROCESSING_initialFieldType<-function(dataset){

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
  return(field_types)
}

# ************************************************
# NPREPROCESSING_discreteNumeric() :
#
# Test NUMERIC field if DISCRETE or ORDINAL
#
# INPUT: data frame      - dataset     - input data
#        vector strings  - field_types - Types per field, either {NUMERIC, SYMBOLIC}
#        int             - cutoff      - Number of empty bins needed to determine discrete (1-10)
#
# OUTPUT : vector strings - Updated with types per field {DISCRETE, ORDINAL}
# ************************************************
# Plots histogram for visulisation
# ************************************************
NPREPROCESSING_discreteNumeric<-function(dataset,field_types,cutoff){

  #For every field in our dataset
  for(field in 1:(ncol(dataset))){

    #Only for fields that are all numeric
    if (field_types[field]==TYPE_NUMERIC) {

      #191020NRT use R hist() function to create 10 bins
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

    } #endif numeric types
  } #endof for
  return(field_types)
}

# ************************************************
# NPREPROCESSING_categorical() :
#
# Transform SYMBOLIC or DISCRETE fields using 1-hot-encoding
#
# INPUT: data frame    - dataset      - symbolic fields
#        vector string - field_types  - types per field {ORDINAL, SYMBOLIC, DISCRETE}
#
# OUTPUT : data frame    - transformed dataset
#
# 18/2/2021 NRT Updated for efficiency
# ************************************************

NPREPROCESSING_categorical<-function(dataset,field_types){

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

  } #endof for()
  return (catagorical)

} # endof categorical_encoding()

# ************************************************
# NplotOutliers() :
#
# Scatter plot of field values and colours outliers in red
#
# INPUT: Vector - sorted    -  points to plot as literal values
#        Vector - outliers  - list of above points that are considered outliers
#        String - fieldName - name of field to plot
#
# OUTPUT : None
# ************************************************
NplotOutliers<-function(sorted,outliers,fieldName){

  plot(1:length(sorted),sorted,pch=1,xlab="Unique records",ylab=paste("Sorted values",fieldName),bty="n")
  if (length(outliers)>0)
    points(outliers,sorted[outliers],col="red",pch=19)
}

# ************************************************
# NPLOT_correlagram() :
#
# Plots PLOT_correlagram
#
# INPUT: data frame - cr - n x n frame of correlation coefficients
#
# OUTPUT : None
# 221019 - plot absolute values only
# ************************************************
NPLOT_correlagram<-function(cr){

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

# ************************************************
# NPREPROCESSING_redundantFields() :
#
# Determine if an entire field is redundant
# Uses LINEAR correlation,
# so use with care as information will be lost
#
# INPUT: data frame - dataset - numeric values only
#        double     - cutoff  - Value above which is determined redundant [0,1]
#
# OUTPUT : Frame - dataset with any fields removed
# ************************************************

NPREPROCESSING_redundantFields<-function(dataset,cutoff){

  print(paste("Before redundancy check Fields=",ncol(dataset)))

  #Remove any fields that have a stdev of zero (i.e. they are all the same)
  xx<-which(apply(dataset, 2, function(x) sd(x, na.rm=TRUE))==0)+1

  if (length(xx)>0L)
    dataset<-dataset[,-xx]

  #Kendall is more robust for data do not necessarily come from a bivariate normal distribution.
  cr<-cor(dataset, use="everything")
  #cr[(which(cr<0))]<-0 #Positive correlation coefficients only
  NPLOT_correlagram(cr)

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

# ************************************************
# NPREPROCESSING_outlier() :
#
# Determine if a value of a record is an outlier for each field
#
# INPUT:   data frame - ordinals   - numeric fields only
#          double     - confidence - Confidence above which is determined an outlier [0,1]
#                                  - Set to negative Confidence if NOT remove outliers
#
# OUTPUT : data frame - ordinals with any outlier values replaced with the median of the field
# ************************************************
# ChiSquared method
# Uses   library(outliers)
# https://cran.r-project.org/web/packages/outliers/outliers.pdf

NPREPROCESSING_outlier<-function(ordinals,confidence){

  #For every ordinal field in our dataset
  for(field in 1:(ncol(ordinals))){

    sorted<-unique(sort(ordinals[,field],decreasing=TRUE))
    outliers<-which(outliers::scores(sorted,type="chisq",prob=abs(confidence)))
    NplotOutliers(sorted,outliers,colnames(ordinals)[field])

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

# ************************************************
# NprintMeasures()
#
# Output measures to the Viewer
#
# INPUT:    list - results - results from NcalcConfusion()
#
# OUTPUT :  NONE
#
# 070819NRT updated to output table to viewer only
# 171019NRT added column name "Metric"
# ************************************************
NprintMeasures<-function(results){

  #This outputs our results into the "Viewer" in RStudio
  tidyTable<-data.frame(t(t(results)))
  names(tidyTable)[1]<-"Metric"

  t<-formattable::formattable(tidyTable,list(
    TP = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",TP)),
    FN = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",FN)),
    TN = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",TN)),
    FP = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",FP))))
  print(t)
}

# ************************************************
# NplotConfusion()
#
# Plot confusion matrix
#
# INPUT:    list - results - results from NcalcConfusion()
#
# OUTPUT :  NONE
#
# 070819NRT Plots confusion matrix
# ************************************************
NplotConfusion<-function(results){

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
} #endof NplotConfusion()

# ************************************************
# Nrmse() :
#
# Calculate the RMSE statistic
#
# INPUT: vector - actual_y     -  numbers indicating the known class
#        vector - y_predicted  - numbers indicating the predicted class
#
# OUTPUT : double - calculated RMSE
# ************************************************
Nrmse<-function(actual_y,y_predicted){

  return(sqrt(mean((actual_y-y_predicted)^2)))
}
# ************************************************
# NcalcMeasures() :
#
# Evaluation measures for a confusion matrix
#
# INPUT: numeric  - TP, FN, FP, TN
#
# OUTPUT: A list with the following entries:
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
#
# 080819NRT added TNR measure
# ************************************************
NcalcMeasures<-function(TP,FN,FP,TN){

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

# ************************************************
# NcalcConfusion() :
#
# Calculate a confusion matrix for 2-class classifier
# INPUT: vector - expectedClass  - {0,1}, Expected outcome from each row (labels)
#        vector - predictedClass - {0,1}, Predicted outcome from each row (labels)
#
# OUTPUT: A list with the  entries from NcalcMeasures()
#
# 070819NRT convert values to doubles to avoid integers overflowing
# Updated to the following definition of the confusion matrix
#
#                    ACTUAL
#               ------------------
# PREDICTED     FRAUD   |  GENUINE
#               ------------------
#     FRAUD      TP     |    FP
#               ==================
#     GENUINE    FN     |    TN
#
#
# ************************************************
NcalcConfusion<-function(expectedClass,predictedClass){

  confusion<-table(factor(predictedClass,levels=0:1),factor(expectedClass,levels=0:1))

  # This "converts" the above into our preferred format

  TP<-as.double(confusion[2,2])
  FN<-as.double(confusion[1,2])
  FP<-as.double(confusion[2,1])
  TN<-as.double(confusion[1,1])

  return(NcalcMeasures(TP,FN,FP,TN))

} #endof NcalcConfusion()

# ************************************************
# NPREPROCESSING_splitdataset() :
#
# Randomise and split entire data set
#
# INPUT: data Frame - combinedML - dataset
#
# OUTPUT : data Frame - test dataset
#          data Frame - train dataset
# ************************************************
NPREPROCESSING_splitdataset<-function(combinedML){

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

# ************************************************
# NPREPROCESSING_prettyDataset()
# Output simple dataset field analysis results as a table in "Viewer"
#
# REQUIRES: formattable
#
# INPUT: data frame    - dataset, full dataset used for train/test
#                      - Each row is one record, each column in named
#                      - Values are not scaled or encoded
#        String - OPTIONAL string which is used in table as a header
#
# OUTPUT : none
#
# Requires the library: PerformanceAnalytics
#                       formattable
# ************************************************
NPREPROCESSING_prettyDataset<-function(dataset,...){

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
