install.packages("ElemStatLearn")
library(ElemStatLearn)
View(spam)
Email = table(spam$spam)

library("party")

Div = sample(2, spam, replace = T, prob = c(0.5,0.5))
Train = spam[Div==1,]
test = spam[Div==2,]