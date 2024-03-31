x=read.csv("C:/Users/Rohan/Downloads/Planets.csv",na.string="")
table(x$year)
x$orbital_period[is.na(x$orbital_period)]=mean(x$orbital_period,na.rm = TRUE)
x$mass[is.na(x$mass)]=mean(x$mass,na.rm = TRUE)
x$distance[is.na(x$distance)]=mean(x$distance,na.rm = TRUE)

##modeling##
sam=sample.split(x$method,SplitRatio = 0.80)
train=subset(x,sam==TRUE)
test=subset(x,sam==FALSE)
mdl=naive_bayes(method~.,data=train)
test$predi=predict(mdl,newdata = test)
table(test$method,test$predi)
