getwd()
setwd("D:/231bcada31")
x<-read.csv("Computer_Data.csv")
View(x)
head(x)
tail(x)
head(x,3)
tail(x,4)
nrow(x)
ncol(x)
names(x)
x[6,5]
x[1:3]
x[7:15,2]
x[c(1,5,7),c(2,4)]


data1<-x(-c(7,9))
data2<-subset(x,price>4000)
data3<-subset(x,price>4000 & premium=="yes" & screen=="14")

write.csv(data2,"mydata.csv")

View(iris)
iris1<-iris
iris1$hi=5
data5<-subset(iris1,Species=="versicolor")
s<-filter(iris1,Sepal.Length,Species)
f<-iris1%>%filter(Sepal.Length>5)
a<-iris1%>%select(Petal.Length)
data11=iris%>% summarise(N1=mean(Sepal.Length),N2=median(Sepal.Width))  
















D<-read.csv("C:/Users/SJU 32/Desktop/dplyr_practice.csv")
View(D)


Q1=sample_n(D,10)
Q1

Q2=sample_frac(D,size=0.1)
Q2

Q3=distinct(D,Index)
Q3

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
Q4=distinct(D,Index, Y2010)
Q4

q5=D%>%select(-starts_with("Y"))
q5

Q6= D%>%select(contains("I"))


