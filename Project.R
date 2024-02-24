getwd()
setwd("D:/Rstudio")
x=read.csv("D:/Rstudio/car_price_prediction.csv")

#The above dataset is about the car price prediction based on the accessories of the car
#In the dataset PRICE is dependent or output variable whereas all the other variables are indepedent
#The PRICE which is dependent variable here is Continuous variable.


#In the data ID is a catagorical data and price is continuous so we use anova 
C1=aov(Price~(ID), data = x)
summary(C1)
#As Pr(>F)is 0.907 which is greater than 0.05 we can exclude this variable



C2=aov(Price~(Manufacturer),data = x)
summary(C2)
#As Pr(>F)is 0.284 greater than 0.05 we can exclude this variable


C3=aov(Price~(Model),data = x)
summary(C3)
#Model should not to include as it is more than the value


C4=aov(Price~(Prod..year),data = x)
summary(C4)
#As Pr(>F)is 0.071greater than 0.05 we can exclude this variable



C5=aov(Price~(Category),data = x)
summary(C5)
#Pr(>F) is 0.000000000000111 ,it is less than 0.05 so it should be included


C6=aov(Price~(Leather.interior),data = x)
summary(C6)
#as Pr(>F) is 0.917 ,it is less than 0.05 so it should not be included



C7=aov(Price~(Fuel.type),data = x)
summary(C7)
#It should be included as it is alternative hypothesis




C8=aov(Price~(Engine.volume),data = x)
summary(C8)
#it should be included as its Pr(>F)value is 0.000107 which is less than 0.05



C9=aov(Price~(Cylinders),data = x)
summary(C9)
#its shouldnt the included


C10=aov(Price~(Gear.box.type),data = x)
summary(C10)
#it should be inclued as is it alternative hypothesis



C11=aov(Price~(Drive.wheels),data = x)
summary(C11)
#is it exceding 0.05 so it should be included



C12=aov(Price~(Doors),data = x)
summary(C12)
#to be included as the Pr(>F) is greater than 0.05


C13=aov(Price~(Wheel),data = x)
summary(C13)
#to be included


C14=aov(Price~(Color),data = x)
summary(C14)
#The value is more than 0.05 so not to be inclued



C15=aov(Price~(Airbags),data = x)
summary(C15)
#to include

C16=aov(Price~(Mileage),data = x)

X=x[-c(1,3,4,5,9,10,11,16)]

                       
                                     ###MODELING###

Sample=sample.split(X$Price,SplitRatio = 0.80)

trainingset=subset(X,Sample == TRUE)
testset=subset(X,Sample==FALSE)
modell=lm(Price~.,data = trainingset)

testset$pridicted_price=predict(modell,testset)
cor(testset$Price,testset$pridicted_price)
