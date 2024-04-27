x=read.csv("C:/Users/Rohan/Downloads/Diabetes Missing Data.csv",na.strings = "")


######## Filling The Missing Values In The Data #########

x$Glucose[is.na(x$Glucose)]=mean(x$Glucose,na.rm = TRUE)

x$Diastolic_BP[is.na(x$Diastolic_BP)]=mean(x$Diastolic_BP,na.rm = TRUE)

x$Skin_Fold[is.na(x$Skin_Fold)]=mean(x$Skin_Fold,na.rm = TRUE)

x$Serum_Insulin[is.na(x$Serum_Insulin)]=mean(x$Serum_Insulin,na.rm = TRUE)

x$BMI[is.na(x$BMI)]=mean(x$BMI,na.rm = TRUE)



###### Verification Through Anova Test  #######
a1=aov(Class~(Pregnant),data = x)
summary(a1)

a2=aov(Class~(Glucose),data = x)
summary(a2)

a3=aov(Class~(Diastolic_BP),data = x)
summary(a3)

a4=aov(Class~(Skin_Fold),data = x)
summary(a4)

a5=aov(Class~(Serum_Insulin),data = x)
summary(a5)

a6=aov(Class~(BMI),data = x)
summary(a6)

a7=aov(Class~(Diabetes_Pedigree),data = x)
summary(a7)

a8=aov(Class~(Age),data = x)
summary(a8)

x=x[-c(1)]
###############################################
######## MODELLING Using Random Forest #########
###############################################
library("randomForest")

#Spliting the data into Training Data and Testing Data 
Sample_data=sample.split(x$Class,SplitRatio = 0.70)
Testing_data= subset(x,Sample_data == TRUE)
Training_data=subset(x,Sample_data == FALSE)

Data_Model= randomForest(Class~., data = Testing_data, na.action = na.exclude)



#Predicting The Testset Results
Testing_data$Predicited_data= predict(Data_Model,Testing_data)
Testing_data$Binary= ifelse(Testing_data$Predicited_data>0.5,1,0)
table(Testing_data$Class,Testing_data$Binary)
plot(Data_Model)

 "
Here TRUE POSITIVE is 350
     TRUE NEGATIVE is 184
     FALSE NEAGTIVE is 4
      
    Therefore the ACCURACY is 350+184/350+184+4= 0.992565
    ACCURACY=99.25%
"

