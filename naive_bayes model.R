x=(iris)

#modeling
library("caTools")
library("")
samp=sample.split(x$Species,SplitRatio = 0.80)
trset=subset(x, samp==TRUE)
tsset=subset(x,samp==FALSE)
mdl=naive_bayes(Species~.,data = trset)
predi=predict(mdl,newdata = tsset)
mat=table(tsset$Species,predi)
mat
