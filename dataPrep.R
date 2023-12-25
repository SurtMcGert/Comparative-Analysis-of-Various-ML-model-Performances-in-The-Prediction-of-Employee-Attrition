# The majority of these functions are from the labs, they have been renamed,
# some bits slightly re-written and re-commented. some new functions have been
# added as well

# Pre-Processing a Dataset functions
# This will store $name=field name, $type=field type
# manualTypes <- data.frame()

#' Function to scale a vector between 0 and 1
#'
#' @param input vector - values to be scaled
#'
#' @return a scaled vector
#' @export
rescale <- function(input) {
  minv <- min(input)
  maxv <- max(input)
  return((input - minv) / (maxv - minv))
}

#' Function to rescale an entire dataframe between 0 and 1
#'
#' @param dataset data frame - the data frame to scale
#'
#' @return a scaled data frame
#' @export
rescaleDataFrame <- function(dataset) {
  scaled <- sapply(as.data.frame(dataset), rescale)
  return(scaled)
}

#' Function to remove punctuation from a string
#'
#' @param input string - the string to remove the punctuation from
#'
#' @return a string with no punctuation
#' @export
removePunctuation <- function(input) {
  return(gsub("[[:punct:][:blank:]]+", "", input))
}

#' Function to read a dataset from the working directory
#'
#' @param csvFilename the file name of the dataset to read
#'
#' @return data frame - the contents of the CSV file
#' @export
readDataset <- function(csvFilename) {
  # read the dataset
  dataset <-
    read.csv(csvFilename,
             fileEncoding = "UTF-8-BOM",
             stringsAsFactors = FALSE)
  
  # read the names of all the variables in the dataset and remove any punctuation
  names(dataset) <- removePunctuation(names(dataset))
  
  # print a confirmation
  print(paste(
    "CSV dataset",
    csvFilename,
    "has been read. Records=",
    nrow(dataset)
  ))
  
  # return the data frame
  return(dataset)
}

#' Function to get if each field is either NUMERIC or SYMBOLIC
#'
#' @param dataset data frame - the data to get the field types of
#' @param continuousFields list - an optional list of field names whos values
#'   are symbolic and ordered
#' @param orderedFields list - an optional list of fields that should be
#'   overriden and set to continuous
#' @param discreteFields list - an optional list of fields that should be
#'   overriden and set to discrete
#'
#' @return vector of types {DISCRETE, CONTINUOUS, SYBOLIC, ORDERED_SYMBOLIC}
#' @export
getFieldTypes <-
  function(dataset,
           continuousFields = list(),
           orderedFields = list(),
           discreteFields = list()) {
    field_types <- vector()
    for (field in 1:(ncol(dataset))) {
      if (is.numeric(dataset[, field]) &&
          !(names(dataset)[field] %in% orderedFields)) {
        field_types[field] <- TYPE_NUMERIC
      } else if (names(dataset)[field] %in% orderedFields) {
        field_types[field] <- TYPE_ORDERED_CATEGORICAL
      } else {
        field_types[field] <- TYPE_SYMBOLIC
      }
    }
    field_types <- getDiscreteOrContinuous(
      dataset = dataset,
      field_types = field_types,
      cutoff = DISCRETE_BINS,
      continuousFields = continuousFields,
      discreteFields = discreteFields
    )
    return(field_types)
  }

#' Function to test if a NUMERIC field is DISCRETE or CONTINUOUS
#'
#' @param dataset data frame - the dataset to test
#' @param field_types vector strings - the types of each field, {NUMERIC,
#'   SYMBOLIC}
#' @param cutoff int - the number of empty bins needed to determine discrete (1
#'   - 10)
#' @param continuousFields
#' @param discreteFields
#'
#' @return a vector of strings updated wit types per field {DISCRETE,
#'   CONTINUOUS}
#' @export
getDiscreteOrContinuous <-
  function(dataset,
           field_types,
           cutoff,
           continuousFields = list(),
           discreteFields = list()) {
    # For every field in our dataset
    for (field in 1:(ncol(dataset))) {
      # Only for fields that are all numeric
      if (field_types[field] == TYPE_NUMERIC) {
        histogramAnalysis <-
          hist(
            dataset[, field],
            main = names(dataset)[field],
            breaks = 10,
            plot = FALSE
          )
        bins <-
          histogramAnalysis$counts / length(dataset[, field]) * 100 # Convert to %
        mean <- mean(bins)
        
        # If the number of bins with less than 1% of the values is greater than
        # the cutoff then the field is determined to be a discrete value
        
        # get the percentage of data that is unique
        percentageUnique <-
          (1 - (nrow(dataset) / length(unique(dataset[[field]]))) * 100)
        numOfvalues <- nrow(dataset)
        numOfUniqueValues <- length(unique(dataset[[field]]))
        percentageUnique <-
          ((numOfUniqueValues / numOfvalues) * 100)
        
        # if the data has any histogram bins with 0 counts, or the data is less
        # than 5% unique, then it is discrete
        if ((length(which(bins <= 0)) > 0) |
            (percentageUnique < 3)) {
          field_types[field] <- TYPE_DISCRETE
        } else {
          field_types[field] <- TYPE_CONTINUOUS
        }
      }
      # override
      if (names(dataset)[field] %in% continuousFields) {
        field_types[field] <- TYPE_CONTINUOUS
      } else if (names(dataset)[field] %in% discreteFields) {
        field_types[field] <- TYPE_DISCRETE
      }
    }
    return(field_types)
  }

#' Function to transform SYMBOLIC or DISCRETE fields using 1-hot-encoding
#'
#' @param dataset data frame - the dataset whos fields need encoding
#' @param field_types vector string - types per field {CONTINUOUS, SYMBOLIC,
#'   DISCRETE, ORDERED_CATEGORICAL}
#'
#' @return data frame - transofmed dataset
#' @export
oneHotEncode <- function(dataset, field_types) {
  categorical <- data.frame()
  
  categorical_fields <-
    names(dataset)[which(field_types == TYPE_SYMBOLIC |
                           field_types == TYPE_DISCRETE)]
  
  # for each field
  for (field in categorical_fields) {
    # Convert into factors. A level for each unique string
    ffield <- factor(dataset[, field])
    
    # Check if too many unique values to encode
    if (nlevels(ffield) > MAX_LITERALS) {
      stop(paste(
        "Practical Business Analytics - too many literals in:",
        field,
        nlevels(ffield)
      ))
    }
    
    # Check if just one value!
    if (nlevels(ffield) == 1) {
      stop(paste(
        "Practical Business Analytics - field stuck at a single value:",
        field
      ))
    }
    
    # 1-hot encoding. A new column for each unique "level"
    xx <- data.frame(model.matrix( ~ ffield + 0, data = ffield))
    
    names(xx) <- gsub("ffield", field, names(xx))
    
    # If 2 unique values, then can encode as a single "binary" column
    if (ncol(xx) == 2) {
      xx <- xx[,-2, drop = FALSE]
      names(xx) <- field # Field name without the value appended
    }
    
    categorical <- as.data.frame(append(categorical, xx))
  }
  return(categorical)
}

#' Function to encode ordered categorical data
#'
#' @param dataset data frame - the dataset to encode the ordered categorical
#'   data from
#' @param field_types list - the list of field types for the dataset
#'   {CONTINUOUS, SYMBOLIC, DISCRETE, ORDERED_CATEGORICAL}
#'
#' @return data frame of all the encoded ordered categorical fields
#' @export
encodeOrderedCategorical <- function(dataset, field_types) {
  ordered_categorical <-
    dataset[which(field_types == TYPE_ORDERED_CATEGORICAL)]
  ordered_categorical[] <- (lapply(ordered_categorical, factor))
  # for each field
  for (field in 1:(ncol(ordered_categorical))) {
    # get the number of unique values for this field
    numOfUniqueValues <-
      length(unique(ordered_categorical[[field]]))
    ordered_categorical[[field]] <-
      as.integer((ordered_categorical[[field]]))
    ordered_categorical[[field]] <-
      ordered_categorical[[field]] / numOfUniqueValues
  }
  return(ordered_categorical)
}

#' Function to clean data by removing symbolic fields that only have one value and deleting a given list of variables
#'
#' @param dataset data frame - the dataset whos fields need encoding
#' @param remove list - a list of column names, to remove from the dataset
#'
#' @return data frame - cleaned dataset
#' @export
cleanData <- function(dataset, remove = list()) {
  print("cleaning data")
  cleanedData <- dataset
  markedColumns <- list()
  
  
  # find the columns from the given list to remove
  for (field in remove) {
    print(paste("removing ", field))
    markedColumns <- append(markedColumns, field)
  }
  
  # find columns that have the same value in every row
  # For every field in our dataset
  for (field in 1:(ncol(dataset))) {
    # Convert into factors. A level for each unique string
    ffield <- factor(dataset[, field])
    # Check if just one value!
    
    # nlevels returns the number of unique rows in the column
    # If there is only one unique row in the column it is useless as we cannot learn from it
    if (nlevels(ffield) == 1) {
      # mark column for removal
      markedColumns <- append(markedColumns, names(dataset[field]))
      print(paste("removing ", names(dataset[field])))
    }
  }
  
  # remove columns
  cleanedData <- dataset[,!(names(dataset) %in% markedColumns)]
  
  # identify numeric columns for removeRedundantFields
  numericColumns <- sapply(cleanedData, is.numeric)
  
  # apply removeRedundantFields only to numeric columns
  if (any(numericColumns)) {
    cleanedData[, numericColumns] <-
      removeRedundantFields(cleanedData[, numericColumns], 0.95)
    print("removed redundant numeric fields")
  } else {
    print("no numeric columns for removeRedundantFields")
  }
  
  print("removed redundant fields")
  print("cleaned data")
  return(cleanedData)
}

#' Function to plot a scatter plot of field values and colour the outliers in
#' red
#'
#' @param sorted vector - points to plot as literal values
#' @param outliers vector - list of above points that are considered outliers
#' @param fieldName string - name of field to plot
#'
#' @return
#' @export
#'
#' @examples
plotOutliers <- function(sorted, outliers, fieldName) {
  plot(
    1:length(sorted),
    sorted,
    pch = 1,
    xlab = "Unique records",
    ylab = paste("Sorted values", fieldName),
    bty = "n"
  )
  if (length(outliers) > 0) {
    points(outliers, sorted[outliers], col = "red", pch = 19)
  }
}

#' Function to plot PLOT_correlagram
#'
#' @param cr data fram - n x n frame of correlation coeficients
#'
#' @return
#' @export
PLOT_correlagram <- function(cr) {
  # Defines the colour range
  col <- colorRampPalette(c("green", "red"))
  
  # To fir on screen, convert field names to a numeric
  rownames(cr) <- 1:length(rownames(cr))
  colnames(cr) <- rownames(cr)
  
  corrplot::corrplot(
    abs(cr),
    method = "square",
    order = "FPC",
    cl.ratio = 0.2,
    cl.align = "r",
    tl.cex = 0.6,
    cl.cex = 0.6,
    cl.lim = c(0, 1),
    mar = c(1, 1, 1, 1),
    bty = "n"
  )
}

#' Function to remove redundant fields using linear correlation, data will be
#' lost so use with caution
#'
#' @param dataset data frame - the dataset to work on, must be numeric values
#'   only
#' @param cutoff double - value above which is determined redundant [0, 1]
#'
#' @return dataset with redundant fields removed
#' @export
removeRedundantFields <- function(dataset, cutoff) {
  print(paste("Before redundancy check Fields=", ncol(dataset)))
  
  # Remove any fields that have a stdev of zero (i.e. they are all the same)
  xx <- which(apply(dataset, 2, function(x) {
    sd(x, na.rm = TRUE)
  }) == 0) + 1
  
  if (length(xx) > 0L) {
    dataset <- dataset[,-xx]
  }
  
  # Kendall is more robust for data do not necessarily come from a bivariate
  # normal distribution.
  cr <- cor(dataset, use = "everything")
  PLOT_correlagram(cr)
  
  correlated <- which(abs(cr) >= cutoff, arr.ind = TRUE)
  list_fields_correlated <-
    correlated[which(correlated[, 1] != correlated[, 2]),]
  
  if (nrow(list_fields_correlated) > 0) {
    print("Following fields are correlated")
    print(list_fields_correlated)
    
    # 240220nrt print list of correlated fields as namesß
    for (i in 1:nrow(list_fields_correlated)) {
      print(paste(names(dataset)[list_fields_correlated[i, 1]], "~", names(dataset)[list_fields_correlated[i, 2]]))
    }
    
    # We have to check if one of these fields is correlated with another as cant remove both!
    v <- vector()
    numc <- nrow(list_fields_correlated)
    for (i in 1:numc) {
      if (length(which(list_fields_correlated[i, 1] == list_fields_correlated[i:numc, 2])) ==
          0) {
        v <- append(v, list_fields_correlated[i, 1])
      }
    }
    print("Removing the following fields")
    print(names(dataset)[v])
    
    return(dataset[,-v]) # Remove the first field that is correlated with another
  }
  return(dataset)
}

#' Function to determine if a value of a record is an outlier
#'
#' @param continuous data frame - numeric fields only
#' @param confidence double - confidence above which is determined as an outlier, set negative if you dont want to replace outliers
#'
#' @return a data frame of ordinals with any outlier values optionally replaced with the median of the field
#' @export
#'
#' @examples
removeOutliers <- function(continuous, confidence) {
  library(car)
  # For every ordinal field in our dataset
  for (field in 1:(ncol(continuous))) {
    # Sort the data in decreasing order
    sorted <- unique(sort(continuous[, field], decreasing = TRUE))
    outliers <-
      which(outliers::scores(sorted, type = "chisq", prob = abs(confidence)))
    plotOutliers(sorted, outliers, colnames(continuous)[field])
    # If found records with outlier values
    if ((length(outliers > 0))) {
      # 070819NRT If confidence is positive then replace values with their means, otherwise do nothing
      if (confidence > 0) {
        # create a new non_outliers vector
        non_outliers <- sorted[-outliers]
        mean_value <- mean(non_outliers, na.rm = TRUE)
        # uses ifelse function to replace outliers in a specific column (field) or the ordinals dataframe with the mean
        continuous[, field] <-
          ifelse(continuous[, field] %in% non_outliers, continuous[, field], mean_value)
        print(paste(
          "Outlier field=",
          names(continuous)[field],
          "Records=",
          length(outliers),
          "Replaced with MEAN"
        ))
      } else {
        print(paste(
          "Outlier field=",
          names(continuous)[field],
          "Records=",
          length(outliers)
        ))
      }
    }
  }
  return(continuous)
}

#' Function to output measures
#'
#' @param results list - the results from a confusion matrix
#' @param name string - the name of the metrics
#'
#' @return
#' @export
#'
#' @examples
printMeasures <- function(results, name) {
  tidyTable <- data.frame(t(t(results)))
  names(tidyTable)[1] <- name
  
  t <- formattable::formattable(tidyTable,
                                list(
                                  TP = formatter("span", style = x ~ style(color = "black"), ~ sprintf("%.0f", TP)),
                                  FN = formatter("span", style = x ~ style(color = "black"), ~ sprintf("%.0f", FN)),
                                  TN = formatter("span", style = x ~ style(color = "black"), ~ sprintf("%.0f", TN)),
                                  FP = formatter("span", style = x ~ style(color = "black"), ~ sprintf("%.0f", FP))
                                ))
  print(t)
}

#' Function to plot a confusion matrix
#'
#' @param results list - results from a confusion matrix
#'
#' @return
#' @export
#'
#' @examples
plotConfusionMatrix <- function(results) {
  aa <- matrix(c(
    round(results$TP, digits = 0),
    round(results$FN, digits = 0),
    round(results$FP, digits = 0),
    round(results$TN, digits = 0)
  ),
  nrow = 2)
  row.names(aa) <- c("Fraud", "Genuine")
  colnames(aa) <- c("Fraud", "Genuine")
  fourfoldplot(
    aa,
    color = c("#cc6666", "#99cc99"),
    conf.level = 0,
    margin = 2,
    main = "TP  FP / FN   TN"
  )
}

#' Function to calculate the RMSE
#'
#' @param actual_y vector - numbers indicating the known classes
#' @param y_predicted vector - the predicted classes from a model
#'
#' @return the RMSE
#' @export
#'
#' @examples
calculateRMSE <- function(actual_y, y_predicted) {
  return(sqrt(mean((
    actual_y - y_predicted
  ) ^ 2)))
}

#' Function to calculate the following measures:
#' 
#'        TP        - double - True Positive records
#'        FP        - double - False Positive records
#'        TN        - double - True Negative records
#'        FN        - double - False Negative records
#'        accuracy  - double - accuracy measure
#'        pgood     - double - precision for "good" (values are 1) measure
#'        pbad      - double - precision for "bad" (values are 1) measure
#'        FPR       - double - FPR measure
#'        TPR       - double - FPR measure
#'        TNR       - double - TNR measure
#'        MCC       - double - Matthew's Correlation Coeficient
#'
#' @param TP numeric - true positive records
#' @param FN numeric - false negative records
#' @param FP numeric - false positive records
#' @param TN numeric - true negative records
#'
#' @return a list with all the measures
#' @export
#'
#' @examples
calculateMeasures <- function(TP, FN, FP, TN) {
  retList <- list(
    "TP" = TP,
    "FN" = FN,
    "TN" = TN,
    "FP" = FP,
    "accuracy" = 100.0 * ((TP + TN) / (TP + FP + FN + TN)),
    "pgood" = 100.0 * (TP / (TP + FP)),
    "pbad" = 100.0 * (TN / (FN + TN)),
    "FPR" = 100.0 * (FP / (FP + TN)),
    "TPR" = 100.0 * (TP / (TP + FN)),
    "TNR" = 100.0 * (TN / (FP + TN)),
    "MCC" = ((TP * TN) - (FP * FN)) / sqrt((TP + FP) *
                                             (TP + FN) * (TN + FP) * (TN + FN))
  )
  return(retList)
}

#' Function to calculate a confusion matrix
#'
#' @param expectedClass vector - expected outcome from each row
#' @param predictedClass vector - predicted outcome from each row
#'
#' @return a list with the TP, FP, FN, TN
#' @export
#'
#' @examples
calculateConfusionMatrix <-
  function(expectedClass, predictedClass) {
    confusion <-
      table(factor(predictedClass, levels = 0:1),
            factor(expectedClass, levels = 0:1))
    
    # This "converts" the above into our preferred format
    
    TP <- as.double(confusion[2, 2])
    FN <- as.double(confusion[1, 2])
    FP <- as.double(confusion[2, 1])
    TN <- as.double(confusion[1, 1])
    
    return(calculateMeasures(TP, FN, FP, TN))
  }

#' Function to split a datset into training and testing sets after randomising
#' the data
#'
#' @param combinedML data frame - the dataset to split
#'
#' @return a list of two data frames, one for the training and one for the
#'   testing dataset
#' @export
#' 
#' @examples
splitDataset <- function(combinedML) {
  # **** Create a TRAINING dataset using 70% of the records
  set.seed(123)
  combinedML <- combinedML[order(runif(nrow(combinedML))),]
  training_records <- round(nrow(combinedML) * (70 / 100))
  
  train <- 1:training_records
  test <- -train
  
  training_data <- combinedML[train,]
  testing_data <- combinedML[test,]
  
  retList <- list("train" = training_data,
                  "test" = testing_data)
  return(retList)
}

#' Function to print an analysis of a dataset
#'
#' @param dataset data frame - the dataset to analyse
#' @param ... OPTIONAL string which is used as the table header
#'
#' @return
#' @export
#'
#' @examples
prettyDataset <- function(dataset, ...) {
  params <- list(...)
  
  tidyTable <-
    data.frame(
      Field = names(dataset),
      Categorical = FALSE,
      Symbols = 0,
      Name = 0,
      Min = 0.0,
      Mean = 0.0,
      Max = 0.0,
      Skew = 0.0,
      stringsAsFactors = FALSE
    )
  
  if (length(params) > 0) {
    names(tidyTable)[1] <- params[1]
  }
  
  for (i in 1:ncol(dataset)) {
    isFieldAfactor <- !is.numeric(dataset[, i])
    tidyTable$Categorical[i] <- isFieldAfactor
    if (isFieldAfactor) {
      tidyTable$Symbols[i] <-
        length(unique(dataset[, i])) # Number of symbols in categorical
      # Gets the count of each unique symbol
      symbolTable <-
        sapply(unique(dataset[, i]), function(x) {
          length(which(dataset[, i] == x))
        })
      majoritySymbolPC <-
        round((sort(symbolTable, decreasing = TRUE)[1] / nrow(dataset)) * 100,
              digits =
                0)
      tidyTable$Name[i] <-
        paste(names(majoritySymbolPC),
              "(",
              majoritySymbolPC,
              "%)",
              sep = "")
    } else {
      tidyTable$Max[i] <- round(max(dataset[, i]), 2)
      tidyTable$Mean[i] <- round(mean(dataset[, i]), 2)
      tidyTable$Min[i] <- round(min(dataset[, i]), 2)
      tidyTable$Skew[i] <-
        round(PerformanceAnalytics::skewness(dataset[, i], method = "moment"),
              2)
      # add Median and Mode
      tidyTable$Median[i] <- round(median(dataset[, i]), 2)
      
      get_mode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
      }
      tidyTable$Mode[i] <- get_mode(dataset[, i])
    }
  }
  
  # Sort table so that all numerics are first
  t <-
    formattable::formattable(
      tidyTable[order(tidyTable$Categorical),],
      list(
        Categorical = formatter(
          "span",
          style = x ~ style(color = ifelse(x, "green", "red")),
          x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))
        ),
        Symbols = formatter(
          "span",
          style = x ~ style(color = "black"),
          x ~ ifelse(x == 0, "-", sprintf("%d", x))
        ),
        Min = formatter(
          "span",
          style = x ~ style(color = "black"),
          ~ ifelse(Categorical, "-", format(
            Min,
            nsmall = 2, big.mark = ","
          ))
        ),
        Mean = formatter(
          "span",
          style = x ~ style(color = "black"),
          ~ ifelse(Categorical, "-", format(
            Mean,
            nsmall = 2, big.mark = ","
          ))
        ),
        Max = formatter(
          "span",
          style = x ~ style(color = "black"),
          ~ ifelse(Categorical, "-", format(
            Max,
            nsmall = 2, big.mark = ","
          ))
        ),
        Skew = formatter(
          "span",
          style = x ~ style(color = "black"),
          ~ ifelse(Categorical, "-", sprintf("%.2f", Skew))
        )
      )
    )
  print(t)
}


#' Function to plot a dataset
#'
#' @param data dataset to plot
#' @param fieldNameOutput name of variable whos relation you want to see against
#'   other variables
#' @param fieldTypes a list of the types of each field {CONTINUOUS, SYMBOLIC,
#'   DISCRETE}
#'
#' @return
#' @export
#'
#' @examples
plotData <- function(data, fieldNameOutput, fieldTypes) {
  # for each variable in the data
  for (i in 1:ncol(data)) {
    # make sure we dont try to plot fieldNameOutput against itself
    if (names(data)[i] != fieldNameOutput) {
      # a scatter plot of the variable against the fieldNameOutput
      name <- names(data)[i]
      title <- paste(name, "vs")
      title <- paste(title, fieldNameOutput)
      print(title)
      print(
        data %>%
          ggplot(aes(
            x = !!sym(name),
            y = !!sym(fieldNameOutput)
          )) +
          geom_point(size = 5, alpha = 0.1) +
          theme_bw() +
          labs(title = title)
      )
      
      
      # a histogram of the variable against the fieldNameOutput
      # convert to categorical
      data[[name]] <- as.factor(data[[name]])
      # Counting and grouping with fieldNameOutput and the compared field
      result <-
        data %>%
        group_by(!!sym(fieldNameOutput), .data[[name]]) %>%
        summarize(Count = n())
      # plot
      print(
        result %>%
          ggplot(aes(x = result[[name]], y = Count)) +
          geom_bar(stat = "identity", aes(fill = !!sym(
            fieldNameOutput
          )), position = "dodge") +
          labs(y = "count", x = name)
      )
      
      # a density plot of the variable if it is continuous
      if (fieldTypes[i] == TYPE_CONTINUOUS) {
        print(
          data %>%
            ggplot(aes(
              x = !!sym(name),
              color = name,
              fill = name
            )) +
            geom_density(alpha = 0.2) +
            theme_bw() +
            labs(title = name)
        )
      } else {
        # a histogram of the variable if it is symbolic or discrete
        print(data %>%
                ggplot(aes(
                  x = !!sym(name),
                  color = name,
                  fill = name
                )) +
                geom_bar() +
                theme_bw() +
                labs(title = name))
      }
    }
  }
}

#  column name 2, the expression to apply to the columns,
# dataframe and name of the new column
#' Function to combine two fields
#'
#' @param dataframe
#' @param colName1 column name 1
#' @param colName2 column name 2
#' @param fun function to apply to the two input columns
#' @param newColName The name of the new column that will be created
#' @param combine set to true to remove th+e two input columns after the new
#'   column has been created
#' @param threshold
#'
#' @return
#' @export
#'
#' @examples
combineOrDeriveFields <-
  function(dataframe,
           colName1,
           colName2,
           fun,
           newColName,
           combine = FALSE,
           threshold = 1) {
    newColumn <-
      mapply(
        FUN = function(x, y) {
          fun(x, y, threshold)
        },
        dataframe[[colName1]],
        dataframe[[colName2]]
      )
    
    # add the new column to the dataframe
    dataframe[[newColName]] <- newColumn
    
    if (combine) {
      print("Deleting...")
      dataframe <-
        dataframe[,-which(names(dataframe) %in% c(colName1, colName2))]
    }
    
    return(dataframe)
  }

#' Function to resample a dataframe in the case that a given class is
#' imbalanced.
#'
#' @param dataframe the dataframe you want to resample
#' @param methodUsed can be either 'both', 'over' or 'under'. Both uses both
#'   oversampling and undersampling. Over refers to oversampling by creating
#'   synthetic data for the underrepresented class. This data may not relate to
#'   the real world! Under refers to undersampling by randomly removing from
#'   both classes according to the disparity between them.
#' @param columnName what column you want rebalance according to, NOTE THE
#'   COLUMN MUST HAVE 2 UNIQUE VALUES FOR IT TO WORK ie. Attrition
#'
#' @return the rebalanced dataframe calculated
#' @export
#'
#' @examples
rebalance <- function(dataframe, methodUsed = "both", columnName) {
  # Show the imbalance of the selected column, by selecting the counts for each unique value
  print("Before") # Note there are two prints because R is bipolar and chooses when to break each one
  print(table(dataframe[columnName])) # 1233 Nos to 237 Yes for Attrition
  
  # Setting as global variable so that ovun.sample can see it (dont ask me why)
  columnName <<- columnName
  dataframe <<- dataframe
  
  # Converts the column name from a string into a formula that can be used by ovun sample to select according to
  formula <- as.formula(paste(columnName, "~ ."))
  
  # Uses different sampling according to the methodUsed. You can adjust the probability of a row being selected by adjusting p.
  if (methodUsed == "both") {
    print("Using both undersampling and oversampling")
    rebalanced <-
      ovun.sample(
        formula,
        data = dataframe,
        N = nrow(dataframe),
        p = 0.5,
        seed = 1,
        method = "both"
      )$data
  } else if (methodUsed == "under") {
    print("Using Undersampling")
    rebalanced <-
      ovun.sample(
        formula,
        data = dataframe,
        p = 0.5,
        seed = 1,
        method = "under"
      )$data
  } else if (methodUsed == "over") {
    ("Using Oversampling")
    rebalanced <-
      ovun.sample(
        formula,
        data = dataframe,
        p = 0.5,
        seed = 1,
        method = "over"
      )$data
  }
  
  # Shows the after results of rebalancing
  print("After")
  print(table(rebalanced[columnName]))
  
  return(rebalanced)
}
