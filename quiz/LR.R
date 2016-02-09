library(car)
states <- as.data.frame(
  state.x77[,c("Murder","Population",
               "Illiteracy", "Income", "Frost")])
dim(states)
t(states[1,])
dtrain <- states[1:25,]
dtest <- states[26:50,]
murderModel <- lm (Murder ~ Population + Illiteracy 
                   + Income + Frost, data=dtrain)
summary (murderModel)
## Linearity
crPlots(murderModel)
crPlots(lm (Murder ~ poly(Population,2) + log2(Illiteracy)+ sin(Income) + poly(Frost,2), data=dtrain))

## Normality
library(car)
qqPlot(dtrain$Murder)
library(gvlma)
print(gvlma(murderModel))

##Error
durbinWatsonTest(murderModel$residuals) # staticstic =2.44, p-value = 0.256

## Homoscedasticity
ncvTest(murderModel) #  p = 0.935

## Multicollinearity
sqrt(vif(murderModel))>2.0 ### False

## Sensitivity to outliers.
outlierTest(murderModel) ### p=0.065

##Model complexity
summary(murderModel)
summary(lm(Murder~Population+Illiteracy,data=dtrain))
