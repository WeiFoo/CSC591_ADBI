##### 
library(car)
library(gdata)
library(qcc)
# library(AER)
rm(list=ls())
setwd("/Users/WeiFu/Github/CSC591_ADBI/HW/topic2/")
data <-read.xls("eBayAuctions.xls", sheet=1)
attach(data)
set.seed(1)



dummy <-data
dummy$Duration <- factor(data$Duration)
getpivot <-function(category){
##### generate a pivot table  like the following one.
#  firstly, calculate the distribution of each entry based on competitive is 0 or 1.
#  then, find the items closed to each other within 0.05 to merge
#  (Note, you can pick most nearest two! but I didn't do it here for simplicity).
#  since EUR and US can be merged, I create a new col V4 to give
#  a common level to both "EUR" and "US".
#         V1        V2     V3      V4
#   2 0.6870748 0.3129252 GBP     <NA>
#   1 0.5515947 0.4484053 EUR new_ EUR
#   3 0.5193498 0.4806502  US new_ EUR
#
#
  pivot <-data.frame()
  for (i in 1:length(levels(category))){
    pivot[i,1] <- length(which(data[category==levels(category)[i],]$Competitive.==0))/length(which(category==levels(category)[i]))
    pivot[i,2] <- length(which(data[category==levels(category)[i],]$Competitive.==1))/length(which(category==levels(category)[i]))
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
##### according to "V4"in pivot is NA or not NA, we merge the items.
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
dummy$Category<-factor(dummy$Category)

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
newdummy <- model.matrix(~ Category+currency+sellerRating+Duration+endDay+ClosePrice+OpenPrice+Competitive., data =dummy )

###########################
##### 
## 60% of the sample size
smp_size <- floor(0.6 * nrow(newdummy))

train_ind <- sample(seq_len(nrow(newdummy)), size = smp_size)


train <- data.frame(newdummy[train_ind, ])
validate <- data.frame(newdummy[-train_ind, ])


##### fit model
model <- glm( Competitive. ~. ,family=binomial,data=train)
summary(model)

##quesion 1 highest is new_currencyGBP

model1 <- glm( Competitive. ~ currencynew_.US,family=binomial,data=train)
summary(model1)

##### question 4
model2 <- glm( Competitive. ~ OpenPrice+ClosePrice+endDaynew_.Wed+endDaynew_.Sat+
               endDayThu+endDayTue+Categorynew_.Electronics+sellerRating+
               currencynew_.US+
               CategorySportingGoods+Categorynew_.Toys.Hobbies+Categorynew_.Collectibles+
              Categorynew_.Pottery.Glass,family=binomial,data=train)
modelReduced<-predict(model2,newdata = validate,type="response")
table<-table(modelReduced>0.5,validate$Competitive.)
accuracyReduced<-sum(diag(table))/sum(table)
accuracyReduced


modelAll<-predict(model,newdata = validate,type="response")
table<-table(modelAll>0.5,validate$Competitive.)
accuracyAll<-sum(diag(table))/sum(table)
accuracyAll

#Overdispersion Test
library(qcc)
qcc.overdispersion.test(train$Competitive.,type="binomial",
                        size=rep(length(train$Competitive.),
                        length(train$Competitive.)))






