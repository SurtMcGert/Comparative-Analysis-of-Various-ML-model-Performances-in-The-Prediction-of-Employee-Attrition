main <- function() {
  
  dataset <- readDataset("dataset/HR_Analytics.csv")
  # dataDistribution_BarPlot(dataset,"Age")
  plotData(dataset,"Age",TRUE)

}

debugSource("dataPrep.R")
library("ggplot2")
main()
