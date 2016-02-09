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
    pivot[i,1] <- length(which(data[category==levels(category)[i],]$Competitive.==1))/length(which(category==levels(category)[i]))
    pivot[i,2] <- length(which(data[category==levels(category)[i],]$Competitive.==0))/length(which(category==levels(category)[i]))
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

merge <-function(Category, pivot){
  for( i in 1:nrow(pivot)){
    if(!is.na(pivot[i,4])){
      levels(Category)<-c(levels(Category),pivot[i,4])
      Category[which(Category==pivot[i,3])] <- c(pivot[i,4])
    }
  }
  return (Category)
  
}

#### Category 
X<-getpivot(data$Category)
dummy$Category<-merge(data$Category, X)
levels(dummy$Category)<-factor(dummy$Category)

#### currency 
Y<-getpivot(data$currency)
dummy$currency<-merge(data$currency, Y)
dummy$currency<-factor(dummy$currency)



#### endDay
Z<-getpivot(data$endDay)
dummy$endDay<-merge(data$endDay, Z)
dummy$endDay<-factor(dummy$endDay)


#### Duration 
W<-getpivot(factor(data$Duration))
dummy$Duration<-merge(factor(data$Duration), W)
dummy$Duration<-factor(dummy$Duration)


#### get the final dummy table
newdummy <- model.matrix(~., data =dummy )

###########################
#####
## 60% of the sample size
smp_size <- floor(0.6 * nrow(newdummy))

train_ind <- sample(seq_len(nrow(newdummy)), size = smp_size)

train <- newdummy[train_ind, ]
validate <- newdummy[-train_ind, ]


##### fit model
model <- glm( Competitive. ~. ,family=binomial(link='logit'),data=train)
summary(model)

##quesion 1 highest is new_currencyGBP

model1 <- glm( Competitive. ~ new_currency,family=binomial(link='logit'),data=train)
summary(model1)

##### question 2
model2 <- glm( Competitive. ~ new_currency+new_category+new_duration+new_endday+ClosePrice+OpenPrice,family=binomial(link='logit'),data=train)
summary(model2)


##### question 4
model4 <- glm( Competitive. ~ 
                 new_currency+
                 new_category+
                 new_endday+
                 ClosePrice+OpenPrice,family=binomial(link='logit'),data=train)
summary(model4)

library(AER)


