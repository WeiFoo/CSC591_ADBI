library(car)
library(gdata)
# library(AER)
rm(list=ls())
setwd("/Users/WeiFu/Github/CSC591_ADBI/HW/topic2/")
data <-read.xls("eBayAuctions.xls", sheet=1)
attach(data)
set.seed(1)
dummy <-data
dummy$Duration <- factor(data$Duration)
getpivot <-function(category){
  pivot <-data.frame()
  for (i in 1:length(levels(category))){
    pivot[i,1] <- length(which(data[data$Category==levels(category)[i],]$Competitive.==1))/length(which(data$Category==levels(category)[i]))
    pivot[i,2] <- length(which(data[data$Category==levels(category)[i],]$Competitive.==0))/length(which(data$Category==levels(category)[i]))
    pivot[i,3] <- levels(category)[i]
  }
  order_pivot <-pivot[order(-pivot$V1),]
  i <- 1
  while(i<=length(levels(category))){
    if ((i+1) <=length(levels(category))){
      if((order_pivot[i,1] - order_pivot[i+1,1])<=0.05){
        order_pivot[i,4]<-paste("new_",order_pivot[i,3])
        order_pivot[i+1,4]<-paste("new_",order_pivot[i,3])
        i =i+2
      }
      else
        i =i+1
    }
    else
      break
  }
  return (order_pivot)
}

merge <-function(dummy$Category, pivot){
  for( i in nrow(pivot)){
    if(!is.na(pivot[i,4])){
      
    }
  }
  
}
X<-getpivot(data$Category)
merge(dummy, X)
