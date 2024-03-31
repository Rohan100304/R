y=read.csv("C:/Users/Rohan/Downloads/NBA.csv",na.strings = "")
x=y[-458,]


x$Salary[is.na(x$Salary)]=mean(x$Salary,na.rm = TRUE)


a1=aov(Salary~(Name),data=x)
summary(a1)

a2=aov(Salary~(Team),data=x)
summary(a2)

c1=cor(x$Salary,x$Number)
c1

a3=aov(Salary~(Position),data=x)
summary(a3)

c2=cor(x$Salary,x$Age)
c2

a4=aov(Salary~(Height),data=x)
summary(a4)


c3=cor(x$Salary,x$Weight)
c3

table(x$College)
x$College[is.na(x$College)]="Kentucky"
a5=aov(Salary~(College),data=x)
summary(a5)

c=y[-c(1,3,6,8)]
library(caTools)
samples=sample.split(c$Salary,SplitRatio = 0.70)
trainingsetss=subset(c,samples==TRUE)
testsetss=subset(c,samples==FALSE)
models=lm(Salary~.,data=trainingsetss)
testsetss$pred=predict(models,testsetss)
cor(testsetss$Salary,testsetss$pred)

