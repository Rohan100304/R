getwd()
setwd("D:/Rstudio")
x=read.csv("D:/Rstudio/ParisHousing.csv")



C1=cor(x$price,x$squareMeters)
C1

C2=cor(x$price,x$numberOfRooms)
C2

C3=aov(price~(hasYard),data = x)
summary(C3)


C4=aov(price~(hasPool),data = x)
summary(C4)

C5=cor(x$price,x$floors)
C5


C6=cor(x$price,x$cityCode)
C6


C7=cor(x$price,x$cityPartRange)
C7

C8=cor(x$price,x$numPrevOwners)
C8


C9=cor(x$price,x$made)
C9

C10=aov(price~(isNewBuilt),data = x)
summary(C10)


C11=aov(price~(hasStormProtector),data = x)
summary(C11)


C12=cor(x$price,x$basement)
C12



C13=cor(x$price,x$attic)
C13


C14=cor(x$price,x$garage)
C14

C15=aov(price~(hasStorageRoom),data = x)
summary(C15)



C16=cor(x$price,x$hasGuestRoom)
C16
X=x[-c(6,7,8,9,10,11)]



Sample=sample.split(X$price,SplitRatio = 0.80)

trainingset=subset(X,Sample == TRUE)
testset=subset(X,Sample==FALSE)
model=lm(price~., data = trainingset)


testset$pridicted_price=predict(model,testset)

cor(testset$price,testset$pridicted_price)

