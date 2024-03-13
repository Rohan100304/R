x=read.csv("C:/Users/Rohan/Downloads/students_adaptability_level_online_education (1).csv")

v1=chisq.test(x$Adaptivity.Level,x$Gender)
v1
#to be taken

v2=chisq.test(x$Adaptivity.Level,x$Age)
v2
#to be takne

v3=chisq.test(x$Adaptivity.Level,x$Education.Level)
v3
# to be takne

v4=chisq.test(x$Adaptivity.Level,x$Institution.Type)
v4
#taken

v5=chisq.test(x$Adaptivity.Level,x$IT.Student)
v5

v6=chisq.test(x$Adaptivity.Level,x$Location)
v6

v7=chisq.test(x$Adaptivity.Level,x$Load.shedding)
v7
#to be taken

v8=chisq.test(x$Adaptivity.Level,x$Financial.Condition)
v8

#to be takne

v9=chisq.test(x$Adaptivity.Level,x$Internet.Type)
v9
#to be taken

v10=chisq.test(x$Adaptivity.Level,x$Network.Type)
v10
# to eb takne

v11=chisq.test(x$Adaptivity.Level,x$Class.Duration)
v11

v12=chisq.test(x$Adaptivity.Level,x$Self.Lms)
v12
#to be takne

v13=chisq.test(x$Adaptivity.Level,x$Device)
summary(v13)
#to be takne

dat=x[-c(6,8,13)]
dat


samples=sample.split(x$Adaptivity.Level,SplitRatio = 0.80)
trainingset=subset(x,samples==TRUE)
testset=subset(x,samples==FALSE)
modl=glm(Adaptivity.Level~., data=dat)
summary(models)summary(models)
testsets$predicted_probability=predict(modl,testsetss)
testsets$binary=ifelse(testsets$predicted_probability>0.50,1,0)
table(testsets$set,testsets$binary)

