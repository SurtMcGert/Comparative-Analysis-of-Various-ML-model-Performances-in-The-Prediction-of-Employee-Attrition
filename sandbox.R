


main <- function() {
  
  dataset <- readDataset("dataset/HR_Analytics.csv")
  
  #print(dataset)
  
  # Age column needs to have its name fixed, resave in excel?
  
  # Convert to dataframe 
  
  
  
  df <- as.data.frame(dataset)
  
  View(df)
  
  
  print("Testing if dataset has missing data")
  print(sum(is.na(df)))
  
  # Removing the irrelevant fields | Feature Selection
  newdf <- subset(df, select = -c(Over18, EmployeeCount, MaritalStatus, EmployeeNumber, JobInvolvement, PerformanceRating, RelationshipSatisfaction, StandardHours, YearsWithCurrManager, MonthlyIncome, MonthlyRate, DailyRate, PercentSalaryHike))
  
  prettyDataset(newdf)
  
  print(ncol(newdf))
  
  # Converting ordinals to ordered categorical
  
  newdf$Education <- factor(newdf$Education, levels = c(1,2,3,4,5), labels = c(1/5, 2/5, 3/5, 4/5, 5/5), ordered = TRUE)
  newdf$EnvironmentSatisfaction <- factor(newdf$EnvironmentSatisfaction, levels = c(1,2,3,4), labels = c(1/4, 2/4, 3/4, 4/4), ordered = TRUE)
  newdf$JobLevel <- factor(newdf$JobLevel, levels = c(1,2,3,4,5), labels = c(1/5, 2/5, 3/5, 4/5, 5/5), ordered = TRUE)
  newdf$JobSatisfaction <- factor(newdf$JobSatisfaction, levels = c(1,2,3,4), labels = c(1/4, 2/4, 3/4, 4/4), ordered = TRUE)
  newdf$WorkLifeBalance <- factor(newdf$WorkLifeBalance, levels = c(1,2,3,4), labels = c(1/4, 2/4, 3/4, 4/4), ordered = TRUE)
  newdf$BusinessTravel <- factor(newdf$BusinessTravel, levels = c('Non-Travel', 'Travel_Rarely', 'Travel_Frequently'), labels = c(1/3, 2/3, 3/3), ordered = TRUE)
  
  prettyDataset(newdf)
  
  View(newdf)

}

debugSource("dataPrep.R")
library("dplyr")
library("formattable")
main()