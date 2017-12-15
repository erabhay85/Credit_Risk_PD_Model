#default Comapny prediction
data<- read.csv("F:/Credit_risk_model/raw_data.csv",header = TRUE)
validationdata<-read.csv("F:/Credit_risk_model/validation_data.csv",header = TRUE)
View(data)
data$default=ifelse(data$Networth.Next.Year<=0,1,0)
str(data)
summary(data)
nrow(data)
ncol(data)
colSums(is.na(data))*100/nrow(data) # percentage missing values in each variable
prop.table(table(data$default))
paste(prop.table(table(data$default))*100,"%")  # default and non default %age
#droping high %age missing values
drops<-c("Deposits..accepted.by.commercial.banks.","slno")
data<-data[,!(names(data)%in%drops)]
# missing value treatment
#missing for numeric ariable ,if missing %age > 50% then simple drop the variable,if missing %age 10% to 50% then replace by 99999(high ale) so that missing will separate class
# if missing %age 5% to 10% then impute by regression, if missing  <5% impute by median
# for categorical variable missing,if missing %50% to 100% replace by NA.so that separate class will be created
#<50% replace by mode and it is subjective
for (i in 1:ncol(data))
{
  print(colnames(data)[i])
  print(quantile(data[i],c(0.01,0.05,0.1,0.2,0.95,0.96,0.97,0.98,0.99,1.0),na.rm = TRUE))
}

percentiledata<-data.frame(c("Dummy"),c(0),c(0))
names(percentiledata)<-c("varname","lowcut","highcut")
percentiledata

for (i in 1:ncol(data))
{
 x1<-quantile(data[i],0.01,na.rm = TRUE)
 x2<-quantile(data[i],0.99,na.rm = TRUE)
 xname<-colnames(data)[i]
 tempdata<-data.frame(xname,x1,x2  )
 names(tempdata)<-c("varname","lowcut","highcut")
 percentiledata<-rbind.data.frame(percentiledata,tempdata)
}

percentiledata
remove(tempdata)
# missing values and caping codes are below

for (i in 1:ncol(data))
{
  xname<-colnames(data)[i]
  x1<-quantile(data[i],0.01,na.rm = TRUE)
  x2<-quantile(data[i],0.99,na.rm = TRUE)
  xmedian<-median(data[,xname],na.rm = TRUE)
  data[,xname]<-ifelse(data[,xname]<=x1,x1,ifelse(data[,xname]>x2,x2,data[,xname]))
  data[,xname][is.na(data[,xname])]<-xmedian
}

# Create new ratio variables

data$PATTI<-data$Profit.after.tax/data$Total.income
data$PATSFCRP<-data$Profit.after.tax/(data$Shareholders.funds+data$Cumulative.retained.profits)
data$PATTS<-data$Profit.after.tax/data$Total.assets
data$TASFCRP<-data$Total.assets/(data$Shareholders.funds+data$Cumulative.retained.profits)
data$BOSFCRP<-data$Borrowings/(data$Shareholders.funds+data$Cumulative.retained.profits)
data$BOTA<-data$Borrowings/data$Total.assets
data$TITA<-data$Total.income/data$Total.assets
data$TINFA<-data$Total.income/data$Net.fixed.assets
data$CACLP<- data$Current.assets/data$Current.liabilities...provisions
data$TLTA<-data$Total.liabilities/data$Total.assets
data$ROS<-data$PBT/data$Sales
data$PBTNWC<-data$PBT/data$Net.working.capital
data$SLTA<-data$Sales/data$Total.assets

# check new ration variable nan and inf values 

for (i in 1:ncol(data))
{
  xname<-colnames(data)[i]
  xmedian<-median(data[,xname],na.rm = TRUE)
  data[,xname][is.na(data[,xname])]<-xmedian
  data[,xname][is.infinite(data[,xname])]<-xmedian
}

# find the mean difference between default group and non default group
require(ggplot2)
for (i in 1:ncol(data))
{
  gdata<-aggregate(data[i], list(company = data$default), mean,na.rm=TRUE) #it will find mean of default and non default
  print(gdata)
  a<-colnames(data)[i]
  barplot(gdata[,a],legend=gdata$company,col=c("darkblue","red"),main=colnames(data)[i],names.arg=c("0(Non-Default)","1(default)"))
 }

#for (i in 1:ncol(data))
#{
#  boxplot(data[i],main=colnames(data)[i])
#}

#spliting training and testing data
require(caTools)
set.seed(151)
sample1=sample.split(data$default,SplitRatio = 0.7)
traindata=subset(data,sample1==TRUE)
testdata=subset(data,sample1==FALSE)
prop.table(table(data$default))
prop.table(table(traindata$default))
prop.table(table(testdata$default))

######################safely loading a package###########
a<-require(aod)
##ifelse( a == FALSE,install.packages("aod"),a <- NA)

################################################ model1 ######################################################### 
#create a base model
#default ~ Total.assets+Net.worth+Total.income+Change.in.stock+Total.expenses+Profit.after.tax+PBDITA
#+PBT+Cash.profit+PBDITA.as...of.total.income+PBT.as...of.total.income+PAT.as...of.total.income
#+Cash.profit.as...of.total.income+PAT.as...of.net.worth+Sales+Income.from.financial.services
#+	Other.income+Total.capital+Reserves.and.funds+Borrowings
#+Current.liabilities...provisions+Deferred.tax.liability+Shareholders.funds+Cumulative.retained.profits
#+Capital.employed+TOL.TNW+Total.term.liabilities...tangible.net.worth+Contingent.liabilities...Net.worth....
#+Contingent.liabilities+Net.fixed.assets+Investments+Current.assets+Net.working.capital+Quick.ratio..times.
#+Current.ratio..times.+Debt.to.equity.ratio..times.+Cash.to.current.liabilities..times.
#+Cash.to.average.cost.of.sales.per.day+Creditors.turnover+Debtors.turnover+Finished.goods.turnover+WIP.turnover
#+Raw.material.turnover+Shares.outstanding+Equity.face.value+EPS+Adjusted.EPS+Total.liabilities+PE.on.BSE
#+PATTI+PATSFCRP+PATTS+TASFCRP+BOSFCRP+BOTA+TITA+TINFA+CACLP+TLTA+ROS+PBTNWC+SLTA

model1<-glm(default ~ PBDITA.as...of.total.income+PBT.as...of.total.income+PAT.as...of.total.income
            +Cash.profit.as...of.total.income+Quick.ratio..times.
            +Current.ratio..times.+Debt.to.equity.ratio..times.+Cash.to.current.liabilities..times.
            +Cash.to.average.cost.of.sales.per.day+EPS+Adjusted.EPS+PBDITA.as...of.total.income
            +PAT.as...of.total.income+Cash.profit.as...of.total.income+TOL.TNW
            +PATTI+PATSFCRP+PATTS+TASFCRP+BOSFCRP+BOTA+TITA+TINFA+CACLP+ROS+PBTNWC+SLTA,
            data=traindata,family = binomial(link="logit"))

summary(model1)


model2<-glm(default ~ Cash.profit.as...of.total.income+Debt.to.equity.ratio..times.
            +EPS+Cash.profit.as...of.total.income+TOL.TNW
            +PATSFCRP+PBTNWC,
            data=traindata,family = binomial(link="logit"))
summary(model2)
confint(model2)
coef(model2)
#confint.default(mymodel1)
library(aod)
wald.test(b=coef(model2),Sigma = vcov(model2),Terms = 2:7)
anova(model2,test="Chisq")
exp(coef(model2))    ## odds ratios only
exp(cbind(OR = coef(model2), confint(model2)))   ## odds ratios and 95% CI
fitted.testresult2<- predict(model2,newdata=subset(testdata,select=c(15,48,15,54,28,64,38)),type="response")
fitted.testresult2<-ifelse(fitted.testresult2>0.6,1,0)
misclassificationerror2<-mean(fitted.testresult2!=testdata$default)
print(paste("Accuracy : ",1-misclassificationerror2))

library(ROCR)
p<- predict(model2,newdata=subset(testdata,select=c(15,48,15,54,28,64,38)),type="response")
pr<-prediction(p,testdata$default)
prf<-performance(pr,measure = "tpr",x.measure = "fpr")
plot(prf)
auc<-performance(pr,measure="auc")
auc<-auc@y.values[[1]]
auc
#confusionMatrix {caret} try
#library(caret)
#confusionMatrix(data=fitted.testresult2, reference=testdata$default)


