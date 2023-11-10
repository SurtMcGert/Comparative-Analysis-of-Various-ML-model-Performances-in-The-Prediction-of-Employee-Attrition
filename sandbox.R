


main <- function() {
  
  dataset <- readDataset("dataset/HR_Analytics.csv")
  
  #print(dataset)
  
  # Age column needs to have its name fixed, resave in excel?
  
  # Convert to dataframe 
  
  
  
  df <- as.data.frame(dataset)
  
  #View(df)
  
  
  print("Testing if dataset has missing data")
  print(sum(is.na(df)))
  
  # Removing the irrelevant fields | Feature Selection
  newdf <- subset(df, select = -c(Over18, EmployeeNumber, EmployeeCount, MaritalStatus, JobInvolvement, PerformanceRating, RelationshipSatisfaction, StandardHours, YearsWithCurrManager, MonthlyIncome, MonthlyRate, DailyRate, PercentSalaryHike))
  
  # , EmployeeID
  
  prettyDataset(newdf)
  
  print(ncol(newdf))
  
  # Converting ordinals to ordered categorical
  
  newdf$Education <- factor(newdf$Education, levels = c(1,2,3,4,5), labels = c(1/5, 2/5, 3/5, 4/5, 5/5), ordered = TRUE)
  newdf$EnvironmentSatisfaction <- factor(newdf$EnvironmentSatisfaction, levels = c(1,2,3,4), labels = c(1/4, 2/4, 3/4, 4/4), ordered = TRUE)
  newdf$JobLevel <- factor(newdf$JobLevel, levels = c(1,2,3,4,5), labels = c(1/5, 2/5, 3/5, 4/5, 5/5), ordered = TRUE)
  newdf$JobSatisfaction <- factor(newdf$JobSatisfaction, levels = c(1,2,3,4), labels = c(1/4, 2/4, 3/4, 4/4), ordered = TRUE)
  newdf$WorkLifeBalance <- factor(newdf$WorkLifeBalance, levels = c(1,2,3,4), labels = c(1/4, 2/4, 3/4, 4/4), ordered = TRUE)
  newdf$BusinessTravel <- factor(newdf$BusinessTravel, levels = c('Non-Travel', 'Travel_Rarely', 'Travel_Frequently'), labels = c(1/3, 2/3, 3/3), ordered = TRUE)
  newdf$StockOptionLevel <- factor(newdf$StockOptionLevel, levels = c(0,1,2,3), labels = c(1/4, 2/4, 3/4, 4/4), ordered = TRUE)
  
  # Encoding the categorical fields
  
  newdf$Department <- model.matrix(~Department-1, data=newdf)
  newdf$EducationField <- model.matrix(~EducationField-1, data = newdf)
  newdf$Gender <- model.matrix(~Gender-1, data = newdf)
  newdf$JobRole <- model.matrix(~JobRole-1, data = newdf)
  newdf$OverTime <- model.matrix(~OverTime-1, data = newdf)
  newdf$Attrition <- model.matrix(~Attrition-1, data = newdf)
  
  # Encoding Numerical Fields
  
  
  
  # This would be the logical method but for some reason merging the two subsets back break
  
  # numericaldf <- subset(newdf, select = c(XUFEFFAge, DistanceFromHome, HourlyRate, NumCompaniesWorked, TotalWorkingYears, TrainingTimesLastYear, YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion))
  # discretedf <- subset(newdf, select = -c(XUFEFFAge, DistanceFromHome, HourlyRate, NumCompaniesWorked, TotalWorkingYears, TrainingTimesLastYear, YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion))
  # 
  # numericaldf <- rescaleDataFrame(numericaldf)
  # 
  # 
  # View(numericaldf)
  # View(discretedf)
  # 
  # superdf <- merge(numericaldf, discretedf) # Some things break when using this method
  # 
  # View(superdf)
  
  
  
  # Other method using the two libraries
  # newdf$EducationField <- as.factor(newdf$EducationField)
  # newdf$EducationField <- one_hot(as.data.table(newdf$EducationField))
  
  
  # Making this a dataframe and using one function breaks some things?
  
  newdf$XUFEFFAge <- rescaleDataFrame(newdf$XUFEFFAge)
  newdf$DistanceFromHome <- rescaleDataFrame(newdf$DistanceFromHome)
  newdf$HourlyRate <- rescaleDataFrame(newdf$HourlyRate)
  newdf$NumCompaniesWorked <- rescaleDataFrame(newdf$NumCompaniesWorked)
  newdf$TotalWorkingYears <- rescaleDataFrame(newdf$TotalWorkingYears)
  newdf$TrainingTimesLastYear <- rescaleDataFrame(newdf$TrainingTimesLastYear)
  newdf$YearsAtCompany <- rescaleDataFrame(newdf$YearsAtCompany)
  newdf$YearsInCurrentRole <- rescaleDataFrame(newdf$YearsInCurrentRole)
  newdf$YearsSinceLastPromotion <- rescaleDataFrame(newdf$YearsSinceLastPromotion)

  
  
  
  prettyDataset(newdf)
  
  View(newdf)

}

debugSource("dataPrep.R")
library("dplyr")
library("formattable")
library("mltools")
library("data.table")
main()