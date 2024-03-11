getwd()
setwd("D:")
c=read.csv("loan data.csv",na.strings = "")

table(c$Gender)
c$Gender[is.na(c$Gender)]="Male"
chisq.test(c$Loan_Status,c$Gender)

table(c$Married)
c$Married[is.na(c$Married)]="Yes"
chisq.test(c$Loan_Status,c$Married)

table(c$Dependents)
c$Dependents[is.na(c$Dependents)]="0"
chisq.test(c$Loan_Status,c$Dependents)

chisq.test(c$Loan_Status,c$Education)

table(c$Self_Employed)
c$Self_Employed[is.na(c$Self_Employed)]="No"
chisq.test(c$Loan_Status,c$Self_Employed)

table(c$ApplicantIncome)
anovatest=aov(ApplicantIncome~(Loan_Status),data=c)
summary(anovatest)

anova=aov(CoapplicantIncome~(Loan_Status),data=c)
summary(anova)


mean(c$LoanAmount)
c$LoanAmount[is.na(c$LoanAmount)]=mean(c$LoanAmount,na.rm = TRUE)
anova=aov(LoanAmount~(Loan_Status),data=c)
summary(anova)

table(c$Loan_Amount_Term)
c$Loan_Amount_Term[is.na(c$Loan_Amount_Term)]="360"
chisq.test(c$Loan_Status,c$Loan_Amount_Term)

table(c$Credit_History)
c$Credit_History[is.na(c$Credit_History)]="1"
chisq.test(c$Loan_Status,c$Credit_History)

table(c$Property_Area)
c$Property_Area[is.na(c$Property_Area)]="Semiurban"
chisq.test(c$Loan_Status,c$Property_Area)

library(caTools)
samples=sample.split(c$Loan_Status,SplitRatio = 0.80)
trainingsetss=subset(c,samples==TRUE)
testsetss=subset(c,samples==FALSE)
models=glm(Loan_Status~.,data=trainingsetss)
summary(models)
testsetss$predicted_probability=predict(models,testsetss)
#testsetss$binary=ifelse(testsetss$predicted_probability>0.50,1,0)
#table(testsetss$Loan_Status,testsetss$binary)

