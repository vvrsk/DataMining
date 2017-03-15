rm(list = setdiff(ls(), lsf.str()))

D <- read.csv("D:/UIC Fall/Data Mining/HW/7/D.csv")

str(D)

#change relevant ones to Factor
D$OUTCOME = factor(D$OUTCOME)
D$First_home = factor(D$First_home)
D$UPB.Appraisal = factor(D$UPB.Appraisal)

D = D[c(-11,-13)]

#Checking for Outliers and replacing them with mean
sum(is.na(D))

boxplot(D$Bo_Age)
Bo_Age_OutLiers = boxplot.stats(D$Bo_Age)$out
D$Bo_Age = ifelse(D$Bo_Age%in%Bo_Age_OutLiers,NA,D$Bo_Age)
D$Bo_Age[is.na(D$Bo_Age)] = mean(D$Bo_Age,na.rm=T)
boxplot(D$Bo_Age)

boxplot(D$Ln_Orig)
Ln_Orig_OutLiers = boxplot.stats(D$Ln_Orig)$out
D$Ln_Orig = ifelse(D$Ln_Orig%in%Ln_Orig_OutLiers, NA, D$Ln_Orig)
D$Ln_Orig[is.na(D$Ln_Orig)] = mean(D$Ln_Orig,na.rm=T)
boxplot(D$Ln_Orig)

boxplot(D$Orig_LTV_Ratio_Pct)
Orig_LTV_Ratio_Pct_OutLiers = boxplot.stats(D$Orig_LTV_Ratio_Pct)$out
D$Orig_LTV_Ratio_Pct = ifelse(D$Orig_LTV_Ratio_Pct%in% Orig_LTV_Ratio_Pct_OutLiers, NA, D$Orig_LTV_Ratio_Pct)
D$Orig_LTV_Ratio_Pct[is.na(D$Orig_LTV_Ratio_Pct)] = mean(D$Orig_LTV_Ratio_Pct,na.rm=T)
boxplot(D$Orig_LTV_Ratio_Pct)

boxplot(D$Credit_score)
Credit_score_OutLiers = boxplot.stats(D$Credit_score)$out
D$Credit_score = ifelse(D$Credit_score%in% Credit_score_OutLiers, NA, D$Credit_score)
D$Credit_score[is.na(D$Credit_score)] = mean(D$Credit_score,na.rm=T)
boxplot(D$Credit_score)

boxplot(D$Tot_mthly_debt_exp)
Tot_mthly_debt_exp_OutLiers = boxplot.stats(D$Tot_mthly_debt_exp)$out
D$Tot_mthly_debt_exp = ifelse(D$Tot_mthly_debt_exp%in% Tot_mthly_debt_exp_OutLiers, NA, D$Tot_mthly_debt_exp)
D$Tot_mthly_debt_exp[is.na(D$Tot_mthly_debt_exp)] = mean(D$Tot_mthly_debt_exp,na.rm=T)
boxplot(D$Tot_mthly_debt_exp)

boxplot(D$Tot_mthly_incm)
Tot_mthly_incm_OutLiers = boxplot.stats(D$Tot_mthly_incm)$out
D$Tot_mthly_incm = ifelse(D$Tot_mthly_incm%in% Tot_mthly_incm_OutLiers, NA, D$Tot_mthly_incm)
D$Tot_mthly_incm[is.na(D$Tot_mthly_incm)] = mean(D$Tot_mthly_incm,na.rm=T)
boxplot(D$Tot_mthly_incm)

boxplot(D$orig_apprd_val_amt)
orig_apprd_val_amt_OutLiers = boxplot.stats(D$orig_apprd_val_amt)$out
D$orig_apprd_val_amt = ifelse(D$orig_apprd_val_amt%in% orig_apprd_val_amt_OutLiers, NA, D$orig_apprd_val_amt)
D$orig_apprd_val_amt[is.na(D$orig_apprd_val_amt)] = mean(D$orig_apprd_val_amt,na.rm=T)
boxplot(D$orig_apprd_val_amt)

boxplot(D$pur_prc_amt)
pur_prc_amt_OutLiers = boxplot.stats(D$pur_prc_amt)$out
D$pur_prc_amt = ifelse(D$pur_prc_amt%in% pur_prc_amt_OutLiers, NA, D$pur_prc_amt)
D$pur_prc_amt[is.na(D$pur_prc_amt)] = mean(D$pur_prc_amt,na.rm=T)
boxplot(D$pur_prc_amt)

boxplot(D$DTI.Ratio)
DTI.Ratio_OutLiers = boxplot.stats(D$DTI.Ratio)$out
D$DTI.Ratio = ifelse(D$DTI.Ratio%in% DTI.Ratio_OutLiers, NA, D$DTI.Ratio)
D$DTI.Ratio[is.na(D$DTI.Ratio)] = mean(D$DTI.Ratio,na.rm=T)
boxplot(D$DTI.Ratio)

boxplot(D$median.income....)
median.income...._OutLiers = boxplot.stats(D$median.income....)$out
D$median.income.... = ifelse(D$median.income....%in% median.income...._OutLiers, NA, D$median.income....)
D$median.income....[is.na(D$median.income....)] = mean(D$median.income....,na.rm=T)
boxplot(D$median.income....)

boxplot(D$X..of.people.in.poverty)
X..of.people.in.poverty_OutLiers = boxplot.stats(D$X..of.people.in.poverty)$out
D$X..of.people.in.poverty = ifelse(D$X..of.people.in.poverty%in% X..of.people.in.poverty_OutLiers, NA, D$X..of.people.in.poverty)
D$X..of.people.in.poverty[is.na(D$X..of.people.in.poverty)] = mean(D$X..of.people.in.poverty,na.rm=T)
boxplot(D$X..of.people.in.poverty)

boxplot(D$LoanValuetoAppraised)
LoanValuetoAppraised_OutLiers = boxplot.stats(D$LoanValuetoAppraised)$out
D$LoanValuetoAppraised = ifelse(D$LoanValuetoAppraised%in% LoanValuetoAppraised_OutLiers, NA, D$LoanValuetoAppraised)
D$LoanValuetoAppraised[is.na(D$LoanValuetoAppraised)] = mean(D$LoanValuetoAppraised,na.rm=T)
boxplot(D$LoanValuetoAppraised)

#Summary of Data
summary(D)
sd(D$Bo_Age)
stset.seed(2014)

D2=D[c(-5,-11,-12)]
cor_d=cor(D2)
View(cor_d)

cor_d

D = D[c(-9)]

#Split data into 75% Training and 25% testing
E = sample(2,nrow(D),replace=T,prob=c(0.75,0.25))
Train = D[E==1,]
Test = D[E==2,]

table(Train$OUTCOME)
table(Test$OUTCOME)

str(Train)
str(Test)

#Install and Load libraries for Ovum
install.packages("ROSE")
library(ROSE)

#Sampling Traning data to contain 30% and 10% default 'OUTCOME' cases
TrgA <- ovun.sample(OUTCOME~., data = Train, method = "both", p=0.3, N=1000, seed = 2014)$data
table(TrgA$OUTCOME)

TrgB <- ovun.sample(OUTCOME~., data = Train, method = "both", p=0.1, N=1000, seed = 2014)$data
table(TrgB$OUTCOME)





#Rtree
library(rpart)

#Plot RTree on TrgA
r_tree_TrgA = rpart(TrgA$OUTCOME~.,data=TrgA,method="class")
print(r_tree_TrgA)
plot(r_tree_TrgA)
text(r_tree_TrgA, use.n = T, xpd= T)

#Predicts rtree on TrgA
predA = predict(r_tree_TrgA,type="class")
table(predA,TrgA$OUTCOME)

table(predict(r_tree_TrgA, type = "class", newdata = Test), Test$OUTCOME)

#Predicts rtree on TrgB
r_tree_TrgB = rpart(OUTCOME~.,data=TrgB,method="class")
print(r_tree_TrgB)
plot(r_tree_TrgB)
text(r_tree_TrgB, use.n = T, xpd= T)


table(predict(r_tree_TrgB, type = "class", newdata = Test), Test$OUTCOME)

predLB = predict(r_tree_TrgB,newdata=Test,type="class")
predLiftB = prediction(predLB, Test$OUTCOME)
perfLB = performance(predLiftB,"lift","rpp")
plot(perfLB)

#Logistic Regression
#On TrgA Data
head(TrgA)
str(TrgA)

#Execute Logistic Reg on TrgA
Logis_TrgA = glm(OUTCOME~., data = TrgA, family = "binomial")
summary(Logis_TrgA)
exp(coef(Logis_TrgA))

library(caret)

#Predict Test data on the Model for TrgA
TestA = predict(Logis_TrgA, newdata = Test, type = "response")
predA = rep('default',length(TestA))
predA[TestA>=0.5] <-'non-default'
confusionMatrix(predA, Test$OUTCOME, dnn = c("Predictions", "Actual Values"))

#Execute Logistic Reg on TrgB
Logis_TrgB = glm(OUTCOME~., data = TrgB, family = "binomial")
summary(Logis_TrgB)
exp(coef(Logis_TrgB))

#Predict Test data on the Model for TrgB
TestB = predict(Logis_TrgB, newdata = Test, type = "response")
predB = rep('default',length(TestB))
predB[TestB>=0.5] <-'non-default'
confusionMatrix(predB, Test$OUTCOME, dnn = c("Predictions", "Actual Values"))



#Neural Networks
install.packages("ISLR")
library(ISLR)

# Create vector of column Max and Min values
maxs = apply(TrgA[,c(1:4, 6:9, 12:14)], 2, max) 
mins = apply(TrgA[,c(1:4, 6:9, 12:14)], 2, min)

# Use scale() and convert the resulting matrix to a data frame
scaled.data = as.data.frame(scale(TrgA[,c(1:4, 6:9, 12:14)], center = mins, scale = maxs - mins))
head(scaled.data)
summary(scaled.data)

#Adding Outcome to the scaled data set
OUTCOME = TrgA$OUTCOME
data = data.frame(OUTCOME,scaled.data)
head(data)

#Constructing neural network
library(nnet)
nn  = nnet(OUTCOME ~ ., data=TrgA, linout=F, size=10, decay=0.01, maxit=1000)

#Neural network model out put
summary(nn) 

#To plot the neural network using nnet we need to use devtools
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(nn)

nn$wts 
nn$fitted.values 

# Using nnet model on the test data:
nn.preds = predict(nn, Test, type = "class")
table(Test$OUTCOME, nn.preds)

install.packages("ROCR")
library(ROCR)
predLA = predict(nn,newdata=Test,type="raw")
predLiftA = prediction(predLA, Test$OUTCOME)
perfLA = performance(predLiftA,"lift","rpp")
plot(perfLA)

# Create vector of column Max and Min values
maxs = apply(TrgB[,c(1:4, 6:9, 12:14)], 2, max) 
mins = apply(TrgB[,c(1:4, 6:9, 12:14)], 2, min)

# Use scale() and convert the resulting matrix to a data frame
scaled.data = as.data.frame(scale(TrgB[,c(1:4, 6:9, 12:14)], center = mins, scale = maxs - mins))
head(scaled.data)
summary(scaled.data)

#Adding Outcome to the scaled data set
OUTCOME = TrgB$OUTCOME
data = data.frame(OUTCOME,scaled.data)
head(data)

#Constructing neural network
library(nnet)
nnB  = nnet(OUTCOME ~ ., data=TrgB, linout=F, size=10, decay=0.01, maxit=1000)

#Neural network model out put
summary(nnB) 

#To plot the neural network using nnet we need to use devtools
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(nnB)

nnB$wts 
nnB$fitted.values 

# Using nnet model on the test data:
nnB.preds = predict(nnB, Test, type = "class")
table(Test$OUTCOME, nnB.preds)

confusionMatrix(predict(nnB,type = "class",newdata = Test), Test$OUTCOME, dnn = c("Predictions", "Actual Values"))

predLB = predict(Logis_TrgB,newdata=Test,type="raw")
predLiftB = prediction(predLB, Test$OUTCOME)
perfLB = performance(predLiftB,"lift","rpp")
plot(perfLB)

