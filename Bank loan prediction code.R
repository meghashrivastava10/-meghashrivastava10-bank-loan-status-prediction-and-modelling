library(readxl)
credit=read.csv("C:/Users/abhhi/OneDrive/Desktop/DA project/credit_train.csv")
head(credit)

library(dplyr)
credit=select(credit,-1)#removing the Loan id column as it does not play any part in data analysis.
head(credit)
credit=select(credit,-1)#removing the customer ID column as it does not play any part in data analysis.
credit
head(credit)




install.packages("tidyverse") #installing new packages

library(tidyverse)
str(credit)
sapply(credit,class)
# code for finding a N/A values in the data set.
missing_data <- colSums(is.na(credit))[colSums(is.na(credit)) > 0] %>% sort(decreasing=T) 
missing_data

#removing Months.since.last.delinquent column as it contains more than 50% of N/A value
credit=select(credit,-"Months.since.last.delinquent")
head(credit)

#There are 514 row columns at the tail of the data
tail(credit,n=514)
#removing those columns
credit<- credit[-seq(nrow(credit),nrow(credit)-514),]
tail(credit)
#code to find missing values
missing_data <- colSums(is.na(credit))[colSums(is.na(credit)) > 0] %>% sort(decreasing=T)
missing_data


#swaping the N/A values with the mean value
credit$Credit.Score=ifelse(is.na(credit$Credit.Score),ave(credit$Credit.Score, FUN= function(x) mean(x,na.rm=TRUE)),credit$Credit.Score)
credit$Annual.Income=ifelse(is.na(credit$Annual.Income),ave(credit$Annual.Income, FUN= function(x) mean(x,na.rm=TRUE)),credit$Annual.Income)
credit$Bankruptcies=ifelse(is.na(credit$Bankruptcies),ave(credit$Bankruptcies, FUN= function(x) mean(x,na.rm=TRUE)),credit$Bankruptcies)
credit$Tax.Liens=ifelse(is.na(credit$Tax.Liens),ave(credit$Tax.Liens, FUN= function(x) mean(x,na.rm=TRUE)),credit$Tax.Liens)
credit$Maximum.Open.Credit=ifelse(is.na(credit$Maximum.Open.Credit),ave(credit$Maximum.Open.Credit, FUN= function(x) mean(x,na.rm=TRUE)),credit$Maximum.Open.Credit)

#code to find missing value
missing_data <- colSums(is.na(credit))[colSums(is.na(credit)) > 0] %>% sort(decreasing=T)
missing_data

head(credit,n=100)




#num.vars<-sapply(credit, is.numeric)
#num.vars

#normalize selected data using function scale
credit[num.vars] <-lapply(credit[num.vars], scale) 
credit[num.vars]
scale

credit[num.vars]

head(credit)
credit[num.vars] <-apply(credit[num.vars], 2, FUN = function(x) (x - min(x))/(max(x)-min(x)))
head(credit[num.vars])

str(credit)

#training and testing data
#credit=credit[sample(nrow(credit)),]
#select.data = sample (1:nrow(credit), 0.8*nrow(credit))
#train.data=credit[select.data,]
#test.data = credit[-select.data,]
#head(train.data)
#head(train.data)

#head(credit)
#train.numeric=credit[,c(2,4,5,9:16)]
#head(train.numeric)
#numeric=train.numeric

#head(numeric)


#dummies
install.packages("dummies")
library(dummies)

credit.dummy=dummy.data.frame(credit,names=c("Term"))
head(credit.dummy)

credit.dummy2=dummy.data.frame(credit.dummy,names = c("Years.in.current.job"))

head(credit.dummy2)


credit.dummy3=dummy.data.frame(credit.dummy2,names = c("Home.Ownership"))
head(credit.dummy3)


credit.dummy4=dummy.data.frame(credit.dummy3,names = c("Purpose"))
head(credit.dummy4)


credit.final=select(credit.dummy4,-c("TermLong Term","Years.in.current.jobn/a","Home.OwnershipHaveMortgage","PurposeBusiness Loan"))
head(credit.final)

str(credit)
unique(credit$Term)


#final model KNN
# training and testing data
credit.final=credit.final[sample(nrow(credit.final)),]
select.data = sample (1:nrow(credit.final), 0.8*nrow(credit.final))
train.data.final=credit.final[select.data,]
test.data.final = credit.final[-select.data,]
head(train.data.final)
head(test.data.final)

#creating labels for training data and testing data
train.Loan.status.final=train.data.final$Loan.Status
head(train.Loan.status.final)
train.data.final=select(train.data.final,-1)
head(train.data.final)

test.Loan.status.final=test.data.final$Loan.Status
head(test.Loan.status.final)
test.data.final=select(test.data.final,-1)
head(test.data.final)

#KNN model
model.final=knn(train = train.data.final,test = test.data.final,cl=train.Loan.status.final,k=3)
summary(model.final)

#accuracy
acc.final=100*sum(test.Loan.status.final== model.final)/NROW(test.Loan.status.final)
acc.final

table(model.final,test.Loan.status.final)

install.packages("caret")
library(caret)
confusionMatrix(table(test.Loan.status.final,model.final))

#numeric model
#numeric=numeric[sample(nrow(numeric)),]
#select.data = sample (1:nrow(numeric), 0.8*nrow(numeric))
#train.data.numeric=numeric[select.data,]
#test.data.numeric = numeric[-select.data,]
#head(train.data.numeric)
#head(test.data.numeric)

#model.numeric=knn(train = train.data.numeric,test = test.data.numeric,cl=train.data_Loan.status,k=3)
#summary(model.numeric)
#acc.numeric=100*sum(test.data_Loan.status==model.numeric)/NROW(test.data_Loan.status)
#acc.numeric



#model including term
#head(credit)
#numeric.term=credit[,c(2,3,4,5,9:16)]
#head(numeric.term)
#numeric.term=numeric.term[sample(nrow(numeric.term)),]
#select.data = sample (1:nrow(numeric.term), 0.8*nrow(numeric.term))
#train.data.numeric.term=numeric.term[select.data,]
#test.data.numeric.term = numeric.term[-select.data,]
#head(train.data.numeric.term)
#head(test.data.numeric.term)

#model.numeric.term=knn(train = train.data.numeric.term,test = test.data.numeric.term,cl=train.data_Loan.status,k=3)
#summary(model.numeric)
#acc.numeric=100*sum(test.data_Loan.status==model.numeric)/NROW(test.data_Loan.status)
#acc.numeric





#to find n/a in string
#grepl("n/a", credit$Home.Ownership,)

#train.data_Loan.status=train.data$Loan.Status
#head(train.data_Loan.status)
#test.data_Loan.status=test.data$Loan.Status
#head(test.data_Loan.status)

#train.data=select(train.data,-1)
#head(train.data)



#train.data1=train.data
#Exam=train.data$Years.in.current.job
#head(Exam)
#train.data1=select(train.data,-"Years.in.current.job")
#head(train.data1)

#test.data=select(test.data,-1)
#head(test.data)


#Eam1=test.data$Years.in.current.job
#head((Eam1))
#test.data1=select(test.data,-"Years.in.current.job")
#head(test.data1)


install.packages("class")
library(class)
 


#model.knn=knn(train = train.data1,test = test.data1,cl=train.data_Loan.status,k=3)


#str(test.data_Loan.status)
#length(unique(test.data_Loan.status))
#table(test.data_Loan.status)




#head(train.data1)






#Logistic regression


head(credit)
credit=credit[sample(nrow(credit)),]
select.data=sample(1:nrow(credit),0.8*nrow(credit))
train.data.logistic=credit[select.data,]
test.data.logistic=credit[-select.data,]
head(train.data.logistic)

train.logistic.label=train.data.logistic$Loan.Status
test.logistic.label=test.data.logistic$Loan.Status
head(train.logistic.label)
head(test.logistic.label)

train.data.logistic=select(train.data.logistic,-1)
test.data.logistic=select(test.data.logistic,-1)
head(train.data.logistic)
head(test)


#model ()
base=glm(train.grad.label~train.data.grad$Term, data=train.data.grad,family=binomial())
full=glm(train.logistic.label~., data=train.data.logistic, family = binomial)

install.packages("leaps")
library(leaps)
step=(base, scope=list(upper=full,lower=~1),direction="both",trace=F)

modellog=glm(train.grad.label~., data=train.data.grad,family=binomial())





model.logistic=glm(train.logistic.label~.,data = train.data.logistic,family=binomial())
summary(model.logistic)


model.logistic.backward=step(modellog,direction="backward",trace=F)
warnings(model.logistic.backward)

summary(model.logistic.backward)
base=glm(train.logistic.label~train.data.logistic$Term, data=train.data.logistic,family=binomial())
model.logistic.forward=step(base,scope=list(upper=model.logistic,lower=~1),direction = "forward",trace=F)


model.logistic.both=step(base,scope = list(upper=model.logistic,lower=~1),direction = "both",trace=F)




prob=predict(model.logistic.backward,type="response",newdata = test.data.grad)
prob
for(i in 1:length(prob)){
  if(prob[i]>0.3){
    prob[i]=1
    
  }else{
    prob[i]=0
  }
}
install.packages("Metrics")
library(Metrics)
accuracy(test.data.logistic,prob) 




#GRADIENTBOOSTING
credit.grad=read.csv("C:/Users/abhhi/OneDrive/Desktop/DA project/credit_train.csv")
head(credit.grad)

library(dplyr)
credit.grad=select(credit.grad,-1)#removing the Loan id column as it does not play any part in data analysis.
head(credit.grad)
credit.grad=select(credit.grad,-1)#removing the customer ID column as it does not play any part in data analysis.

head(credit.grad)

#..
Loan_status=credit$Loan.status
term=credit$Term
credit_score=credit$Credit.Score
Annual_Income=credit$Annual.Income
Years_in_current_Job=credit$Years.in.current.job
Home_Ownership=credit$Home.Ownership
Purpose=credit$Purpose
Monthly_debt=credit$Monthly.Debt
Annual_Income=credit$Annual.Income
Credit_history=credit$Years.of.Credit.History
Months_since_last_delinquent=credit$Months.since.last.delinquent
No_of_open_accounts=credit$Number.of.Open.Accounts
No_of_Credit_problems=credit$Number.of.Credit.Problems
Current_credit_balance=credit$Current.Credit.Balance
Max_open_credit=credit$Maximum.Open.Credit
Bankruptcies=credit$Bankruptcies
Tax_liens=credit$Tax.Liens
..#



install.packages("tidyverse")

library(tidyverse)
str(credit)
sapply(credit,class)
missing_data1 <- colSums(is.na(credit.grad))[colSums(is.na(credit.grad)) > 0] %>% sort(decreasing=T)
missing_data1

credit.grad=select(credit.grad,-"Months.since.last.delinquent")
credit.grad
head(credit.grad)
tail(credit.grad,n=514)

credit.grad<- credit.grad[-seq(nrow(credit.grad),nrow(credit.grad)-514),]
tail(credit.grad)
missing_data1 <- colSums(is.na(credit.grad))[colSums(is.na(credit.grad)) > 0] %>% sort(decreasing=T)
missing_data1



credit.grad$Credit.Score=ifelse(is.na(credit.grad$Credit.Score),ave(credit.grad$Credit.Score, FUN= function(x) mean(x,na.rm=TRUE)),credit.grad$Credit.Score)
credit.grad$Annual.Income=ifelse(is.na(credit.grad$Annual.Income),ave(credit.grad$Annual.Income, FUN= function(x) mean(x,na.rm=TRUE)),credit.grad$Annual.Income)
credit.grad$Bankruptcies=ifelse(is.na(credit.grad$Bankruptcies),ave(credit.grad$Bankruptcies, FUN= function(x) mean(x,na.rm=TRUE)),credit.grad$Bankruptcies)
credit.grad$Tax.Liens=ifelse(is.na(credit.grad$Tax.Liens),ave(credit.grad$Tax.Liens, FUN= function(x) mean(x,na.rm=TRUE)),credit.grad$Tax.Liens)
credit.grad$Maximum.Open.Credit=ifelse(is.na(credit.grad$Maximum.Open.Credit),ave(credit.grad$Maximum.Open.Credit, FUN= function(x) mean(x,na.rm=TRUE)),credit.grad$Maximum.Open.Credit)
missing_data1 <- colSums(is.na(credit.grad))[colSums(is.na(credit.grad)) > 0] %>% sort(decreasing=T)
missing_data1

head(credit.grad,n=100)
head(credit.grad)
credit.dummy1=dummy.data.frame(credit.grad,names=c("Term","Years.in.current.job","Home.Ownership","Purpose"))
head(credit.dummy1)
credit.dummy1=select(credit.dummy1,-c("TermLong Term","Years.in.current.jobn/a","Home.OwnershipHaveMortgage","PurposeBusiness Loan"))
head(credit.dummy1)

credit.dummy1$Current.Loan.Amount=scale(credit.dummy1$Current.Loan.Amount)
head(credit.dummy1$Current.Loan.Amount)
head(credit.dummy1)
credit.dummy1$Credit.Score=scale(credit.dummy1$Credit.Score)
head(credit.dummy1)
credit.dummy1$Annual.Income=scale(credit.dummy1$Annual.Income)
head(credit.dummy1)
credit.dummy1$Monthly.Debt=scale(credit.dummy1$Monthly.Debt)
credit.dummy1$Years.of.Credit.History=scale(credit.dummy1$Years.of.Credit.History)
credit.dummy1$Number.of.Open.Accounts=scale(credit.dummy1$Number.of.Open.Accounts)
credit.dummy1$Current.Credit.Balance=scale(credit.dummy1$Current.Credit.Balance)
credit.dummy1$Maximum.Open.Credit=scale(credit.dummy1$Maximum.Open.Credit)
head(credit.dummy1)


#test and train
credit.dummy1=credit.dummy1[sample(nrow(credit.dummy1)),]
select.data=sample(1:nrow(credit.dummy1),0.8*nrow(credit.dummy1))
train.data.grad=credit.dummy1[select.data,]
test.data.grad=credit.dummy1[-select.data,]
head(train.data.grad)

train.grad.label=train.data.grad$Loan.Status
test.grad.label=test.data.grad$Loan.Status
head(train.grad.label)
head(test.grad.label)

train.data.grad=select(train.data.grad,-1)
test.data.grad=select(test.data.grad,-1)
head(train.data.grad)
head(test.data.grad)

head(train.grad.label)
unique(test.grad.label)



is.factor(train.grad.label)


train.grad.label=as.character(train.grad.label)
head(train.grad.label)
is.character(train.grad.label)
str(train.grad.label)
train.grad.label[train.grad.label == "Fully Paid"]=1
train.grad.label[train.grad.label == "Charged Off"]=0

head(train.grad.label,n=10)
train.grad.label=as.integer(train.grad.label)
is.integer(train.grad.label)
head(train.grad.label)






test.grad.label=as.character(test.grad.label)
head(test.grad.label)
is.character(test.grad.label)
str(train.grad.label)
test.grad.label[test.grad.label == "Fully Paid"]=1
test.grad.label[test.grad.label == "Charged Off"]=0

head(test.grad.label,n=10)
test.grad.label=as.integer(test.grad.label)
is.integer(test.grad.label)
head(test.grad.label)


#model Grad
install.packages("gbm")
library(gbm)
Gradient.boost=gbm(train.grad.label~.,data = train.data.grad,distribution = "gaussian",n.trees = 100,shrinkage = 0.01, interaction.depth = 4)
summary(Gradient.boost)



n.trees = seq(from=1 ,to=100, by=1) #no of trees-a vector of 100 values 

#Generating a Prediction matrix for each Tree
predmatrix<-predict(Gradient.boost,test.data.grad,n.trees = n.trees)
dim(predmatrix) #dimentions of the Prediction Matrix

#Calculating The Mean squared Test Error
test.error<-with(test.data.grad,apply( (predmatrix-test.grad.label)^2,2,mean))
head(test.error) #contains the Mean squared test error for each of the 100 trees averaged

#Plotting the test error vs number of trees

plot(n.trees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")

#adding the RandomForests Minimum Error line trained on same data and similar parameters
abline(h = min(test.error),col="red") #test.err is the test error of a Random forest fitted on same data
legend("topright",c("Minimum Test error Line for Random Forests"),col="red",lty=1,lwd=1)


#Plot of Response variable with Credit.Score variable
plot(Gradient.boost,i="Credit.Score") 
#Inverse relation with Credit.Score variable

plot(Gradient.boost,i="Current.Loan.Amount") 
#as the average number of rooms increases the the price increases















#LOGISTIC2



credit.log=read.csv("C:/Users/abhhi/OneDrive/Desktop/DA project/credit_train.csv")
head(credit.log)

library(dplyr)
credit.log=select(credit.log,-1)#removing the Loan id column as it does not play any part in data analysis.
head(credit.log)
credit.log=select(credit.log,-1)#removing the customer ID column as it does not play any part in data analysis.

head(credit.log)

#..
Loan_status=credit$Loan.status
term=credit$Term
credit_score=credit$Credit.Score
Annual_Income=credit$Annual.Income
Years_in_current_Job=credit$Years.in.current.job
Home_Ownership=credit$Home.Ownership
Purpose=credit$Purpose
Monthly_debt=credit$Monthly.Debt
Annual_Income=credit$Annual.Income
Credit_history=credit$Years.of.Credit.History
Months_since_last_delinquent=credit$Months.since.last.delinquent
No_of_open_accounts=credit$Number.of.Open.Accounts
No_of_Credit_problems=credit$Number.of.Credit.Problems
Current_credit_balance=credit$Current.Credit.Balance
Max_open_credit=credit$Maximum.Open.Credit
Bankruptcies=credit$Bankruptcies
Tax_liens=credit$Tax.Liens
..#



install.packages("tidyverse")

library(tidyverse)
str(credit)
sapply(credit,class)
missing_data2 <- colSums(is.na(credit.log))[colSums(is.na(credit.log)) > 0] %>% sort(decreasing=T)
missing_data2

credit.log=select(credit.log,-"Months.since.last.delinquent")
credit.log
head(credit.log)
tail(credit.log,n=514)

credit.log<- credit.log[-seq(nrow(credit.log),nrow(credit.log)-514),]
tail(credit.log)
missing_data2 <- colSums(is.na(credit.log))[colSums(is.na(credit.log)) > 0] %>% sort(decreasing=T)
missing_data2



credit.log$Credit.Score=ifelse(is.na(credit.log$Credit.Score),ave(credit.log$Credit.Score, FUN= function(x) mean(x,na.rm=TRUE)),credit.log$Credit.Score)
credit.log$Annual.Income=ifelse(is.na(credit.log$Annual.Income),ave(credit.log$Annual.Income, FUN= function(x) mean(x,na.rm=TRUE)),credit.log$Annual.Income)
credit.log$Bankruptcies=ifelse(is.na(credit.log$Bankruptcies),ave(credit.log$Bankruptcies, FUN= function(x) mean(x,na.rm=TRUE)),credit.log$Bankruptcies)
credit.log$Tax.Liens=ifelse(is.na(credit.log$Tax.Liens),ave(credit.log$Tax.Liens, FUN= function(x) mean(x,na.rm=TRUE)),credit.log$Tax.Liens)
credit.log$Maximum.Open.Credit=ifelse(is.na(credit.log$Maximum.Open.Credit),ave(credit.log$Maximum.Open.Credit, FUN= function(x) mean(x,na.rm=TRUE)),credit.log$Maximum.Open.Credit)
missing_data2 <- colSums(is.na(credit.log))[colSums(is.na(credit.log)) > 0] %>% sort(decreasing=T)
missing_data2

head(credit.log,n=100)
head(credit.log)
#credit.dummy1=dummy.data.frame(credit.grad,names=c("Term","Years.in.current.job","Home.Ownership","Purpose"))
#head(credit.dummy1)
#credit.dummy1=select(credit.dummy1,-c("TermLong Term","Years.in.current.jobn/a","Home.OwnershipHaveMortgage","PurposeBusiness Loan"))
#head(credit.dummy1)

credit.log$Current.Loan.Amount=scale(credit.log$Current.Loan.Amount)
head(credit.log$Current.Loan.Amount)
head(credit.log)
credit.log$Credit.Score=scale(credit.log$Credit.Score)
head(credit.log)
credit.log$Annual.Income=scale(credit.log$Annual.Income)
head(credit.log)
credit.log$Monthly.Debt=scale(credit.log$Monthly.Debt)
credit.log$Years.of.Credit.History=scale(credit.log$Years.of.Credit.History)
credit.log$Number.of.Open.Accounts=scale(credit.log$Number.of.Open.Accounts)
credit.log$Current.Credit.Balance=scale(credit.log$Current.Credit.Balance)
credit.log$Maximum.Open.Credit=scale(credit.log$Maximum.Open.Credit)
head(credit.log)
str(credit.log)

#test and train
credit.log=credit.log[sample(nrow(credit.log)),]
select.data=sample(1:nrow(credit.log),0.8*nrow(credit.log))
train.data.log=credit.log[select.data,]
test.data.log=credit.log[-select.data,]
head(train.data.log)

train.log.label=train.data.log$Loan.Status
test.log.label=test.data.log$Loan.Status
head(train.grad.label)
head(test.grad.label)

train.data.grad=select(train.data.grad,-1)
test.data.grad=select(test.data.grad,-1)
head(train.data.grad)
head(test.data.grad)

head(train.grad.label)
unique(test.grad.label)



model.log.final=glm(train.data.log$Loan.Status~.,data = train.data.log,family="binomial")
summary(model.log.final)


res=predict(model.log.final,test.data.log,type = "response")
res
for(i in 1:length(res)){
  if(res[i]>0.4){
    res[i]=1
    
  }else{
    res[i]=0
  }
}
install.packages("Metrics")
library(Metrics)


confmatrix=table(Actual_Value=train.data.log$Loan.Status,predicted_value= res>0.5)
accuracy(test.log.label,res)
