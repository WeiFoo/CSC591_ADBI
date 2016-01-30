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
crPlots(murderModel)
qqPlot(dtrain$Murder)
durbinWatsonTest(murderModel)
ncvTest(murderModel)
sqrt(vif(murderModel))>2.0
outlierTest(murderModel)
