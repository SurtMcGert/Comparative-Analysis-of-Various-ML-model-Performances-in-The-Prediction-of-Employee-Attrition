main <- function() {
  print("Hello!")
  
  dataset <- readDataset("dataset/HR_Analytics.csv")
  
  # #plotData(dataset, "WorkLifeBalance")
  # 
  # # Converting this to a categorical column
  # dataset$WorkLifeBalance <- as.factor(dataset$WorkLifeBalance)
  # 
  # newdataset <- as.data.frame(dataset)
  # 
  # print(dataset$WorkLifeBalance.isCat) # Checking that it is categorical
  # 
  # # ggplot2.barplot(data=newdataset, xName="attrition", yName='WorkLifeBalance')
  # 
  # 
  # # Counting and grouping by attrition and worklifebalance
  # result <- newdataset %>% group_by(Attrition, WorkLifeBalance) %>% summarize(Count = n())
  # 
  # print(result)
  # 
  # 
  # p <- ggplot(result, aes(x = WorkLifeBalance, y = Count)) + geom_bar(stat = "identity", aes(fill = Attrition), position = "dodge")
  # 
  # # c <- ggplot(newdataset, aes(x = WorkLifeBalance)) + geom_bar(aes(fill = WorkLifeBalance))
  # 
  # 
  # 
  # print(p)
  # # print(c)
  
  
  #dataset$WorkLifeBalance <- as.factor(dataset$WorkLifeBalance)

  
  # le example :3
  plotAttritionAsBarGraphs(dataset, "EnvironmentSatisfaction", TRUE, TRUE)

}

# Requires ggplot2 and dplyr to be installed

# install.packages("devtools")
# library(devtools)
# install_github("easyGgplot2", "kassambara")

#install.packages(dplyr)

debugSource("dataPrep.R")
#library("ggplot2")
#library("dplyr")
library("tidyverse")
main()