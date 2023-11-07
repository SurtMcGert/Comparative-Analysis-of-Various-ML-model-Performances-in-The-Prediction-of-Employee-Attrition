main <- function() {
  
  dataset <- readDataset("dataset/HR_Analytics.csv")
  dataDistribution_BarPlot(dataset,"Age")

}

debugSource("dataPrep.R")
library("ggplot2")
main()
