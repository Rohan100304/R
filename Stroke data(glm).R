x=read.csv("C:/Users/Rohan/Downloads/Stroke.csv",na.strings = "Unknown")
table(x$smoking_status)
x$smoking_status[is.na(x$smoking_status)]="never smoked"
table(x$smoking_status)

G=x[-c(5,6,7)]
library("caTools")
samp=sample.split(G$stroke,SplitRatio = 0.80)
trainset=subset(G,samp==TRUE)
testset=subset(G,samp==FALSE)
mdl=glm(stroke~.,data = trainset)
testset$predict=predict(mdl,testset)
testset$biary=ifelse(testset$predict>0.5,1,0)
table(testset$stroke,testset$biary)



############OUTPUT##########

   0
0 947
1  50

#accuracy=94.9%