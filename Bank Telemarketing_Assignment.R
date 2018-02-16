#Read the csv file
bank <- read.csv("D:\\Module3_Assignment\\Bank.csv",sep=";", header=T)

#Structure of the data frame
str(bank)
#Number of rows
NROW(bank)
#Build a frequence table
table(bank$y)


class(bank)
edit(bank)
unique(bank$job)
table(bank$job)
#Remove the rows with unknown in job column
mydata1<-subset(bank,subset=(job!="unknown"))

table(mydata1$job)
table(mydata1$education)

#Remove the rows with unknown in education column
mydata2<-subset(bank,subset=(education!="unknown"))
table(mydata2$education)
table(mydata2$contact)
table(mydata2$default)
barplot(table(mydata2$default))
table(mydata2$poutcome)
table(mydata2$duration)

#drop  the default,duration,contact and poutcome columns which have more number of rows with unknown values.
mydata3<-subset(mydata2,select=-c(default,duration,contact,poutcome)
)

table(mydata3$y)

mydata3$yvar<-ifelse(mydata3$y=="yes",1,2)
write.csv(mydata3, file = "D:\\Module3_Assignment\\Cleaned_data.csv")
mydata3$yvar<-as.factor(mydata3$yvar)

mydata4<-subset(mydata3,select=-c(y))
table(mydata4$yvar)

barplot(table(mydata4$yvar))
table(mydata4$yvar)
#plot of age and y var
ggplot()+geom_bar(data=mydata4,aes(x=(mydata4$age),fill=factor(mydata4$yvar)),position="fill")
str(mydata4)

#plot of marital status and y var
ggplot()+geom_bar(data=mydata4,aes(x=(mydata4$marital),fill=factor(mydata4$yvar)),position="fill")

ggplot()+geom_bar(data=mydata4,aes(x=(mydata4$day),fill=factor(mydata4$yvar)),position="fill")


#Modelling
library(C50)
set.seed(123)
NROW(mydata4)
datamixed=mydata4[order(runif(43354)),]
traindata<-datamixed[1:30347,]
tesdata<-datamixed[30348:43354,]
modelc5<-C5.0(traindata$yvar~.,data=traindata)

tree.bank=tree(yvar~.,data=mydata4)
Prunetree1 <- cv.tree(modelc5, FUN=prune.tree, K=10, method="misclass")
predictedc5=predict(modelc5,tesdata[,1:12])
plot(modelc5)
plot(Prunetree1)

#Confusion matrix
confusionMatrix(predictedc5,tesdata[,13])

table(traindata$yvar)

ordereddata<-traindata[order(-xtfrm(traindata$yvar)),]
#under sampling
barplot(table(ordereddata$yvar))
sampletraindata1<-ordereddata[18680:30347,]
barplot(table(sampletraindata1$yvar))
modelc5_u1<-C5.0(sampletraindata1$yvar~.,data=sampletraindata1)
predictedc5_u1=predict(modelc5_u1,tesdata[,1:12])
plot(modelc5_u1)
plot(modelc5_u1)
confusionMatrix(predictedc5_u1,tesdata[,13])
#over Sampling
ordereddata<-traindata[order(-xtfrm(traindata$yvar)),]

sampletraindata2<-ordereddata[21598:30347,]
barplot(table(sampletraindata1$yvar))
barplot(table(sampletraindata2$yvar))
modelc5_u2<-C5.0(sampletraindata1$yvar~.,data=sampletraindata1)
plot(modelc5_u2)
predictedc5_u2=predict(modelc5_u2,tesdata[,1:12])
confusionMatrix(predictedc5_u2,tesdata[,13])
library(caret)

#Neural networks model
library(nnet)
modelnnet<-nnet(sampletraindata1$yvar~.,size=7,data=sampletraindata1)
 library(NeuralNetTools)
 
 NeuralNetTools::plotnet(modelnnet)
 predictednnet=predict(modelnnet,tesdata[,1:12],Type="class")
confusionMatrix(predictednnet,tesdata[,13])
  
predictednnet=predict(modelnnet,tesdata[,1:12],type="class")
confusionMatrix(predictednnet,tesdata[,13])

#Naive Bayes model
install.packages("kernlab")
library(kernlab)
modelsvm = ksvm(sampletraindata1$yvar ~ ., data = sampletraindata1, kernel = "vanilladot")

