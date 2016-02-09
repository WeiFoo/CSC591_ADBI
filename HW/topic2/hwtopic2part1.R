library(car)
library(gdata)
ilbrary(AER)
rm(list=ls())
setwd("/Users/WeiFu/Github/CSC591_ADBI/HW/topic2/")
data <-read.xls("eBayAuctions.xls", sheet=1)
attach(data)
set.seed(1)

change <-function(dummy_col, old_level, new_level ){
  levels(dummy_col)[levels(dummy_col) == old_level] <- new_level 
  return (dummy_col)
}


# currency
tableCur <-table(varC=data[,'currency'],compet=data[,'Competitive.'],useNA='ifany')

new_currency <- data$currency
new_currency <-change(new_currency,"US","No_GBP")
new_currency <-change(new_currency,"EUR","No_GBP")
data <- data.frame(data,new_currency)

# Category
new_category <-data$Category
new_category <-change(new_category,"Electronics","Ele_Photo")
new_category <-change(new_category,"Photography","Ele_Photo")
new_category <-change(new_category,"Books","Boo_Cloth")
new_category <-change(new_category,"Clothing/Accessories","Boo_Cloth")
new_category <-change(new_category,"Business/Industrial","Busi_Comp")
new_category <-change(new_category,"Computer","Busi_Comp" )
new_category <-change(new_category,"Collectibles","Coll_Anti" )
new_category <-change(new_category,"Antique/Art/Craft","Coll_Anti" )
new_category <-change(new_category,"Automotive","Auto_Pott" )
new_category <-change(new_category,"Pottery/Glass","Auto_Pott" )
data <- data.frame(data, new_category)

#Duration
new_duration <- data$Duration
new_duration[new_duration==1] <-10
new_duration[new_duration==3] <-7
data <-data.frame(data, new_duration)

#endDay
new_endday <-data$endDay
new_endday <-change(new_endday,"Sun","Sun_Wed")
new_endday <-change(new_endday, "Wed","Sun_Wed")
new_endday <-change(new_endday, "Fri","Fri_Sat")
new_endday <-change(new_endday, "Sat", "Fri_Sat")
data <-data.frame(data,new_endday)


#####
## 60% of the sample size
smp_size <- floor(0.6 * nrow(data))

train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
validate <- data[-train_ind, ]


##### fit model
model <- glm( Competitive. ~ new_currency+new_category+new_duration+new_endday+ClosePrice+OpenPrice,family=binomial(link='logit'),data=train)
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
