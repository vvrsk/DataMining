rm(list = setdiff(ls(), lsf.str()))

p <- read.csv("D:/UIC Fall/Data Mining/HW/9/prospects.csv")

str(p)
summary(p)

p = p[,-1]
p = p[,-6]

str(p)
summary(p)

sum(is.na(p))

sum(is.na(p$AGE))
p$AGE[is.na(p$AGE)] = mean(p$AGE, na.rm=TRUE)
sum(is.na(p$AGE))

sum(is.na(p$INCOME))
p$INCOME[is.na(p$INCOME)] = mean(p$INCOME, na.rm=TRUE)
sum(is.na(p$CLIMATE))

sum(is.na(p))

library(mice)
md.pattern(p)

p1 = mice(p, m=5, maxit=50, meth='pmm', seed=500)

p_complete = complete(p1, 1)

summary(p_complete)
str(p_complete)

p_complete$SEX1[p_complete$SEX=="M"] <- "1"
p_complete$SEX1[p_complete$SEX=="F"] <- "2"
p_complete$SEX1 <- factor(p_complete$SEX1)

p_complete <- p_complete[,-3]

summary(p_complete)

sum(is.na(p_complete$SEX1))
table(p_complete$SEX1)
p_complete$SEX1[is.na(p_complete$SEX1)] = 1

c = kmeans(p_complete, 4, nstart = 100)
c$size

aggregate(p_complete,by=list(c$cluster),FUN=mean)
aggregate(p_complete,by=list(c$cluster),FUN=var)

data = p_complete
a = (nrow(data)-1)*sum(apply(data,2,var))
for (i in 2:15) a[i] = sum(kmeans(data, centers=i)$withinss)
plot(1:15, a, type="b", xlab="Number of Clusters", ylab="Sum of squares within groups", 
     main="Getting number of Clusters with the Elbow Method", pch=20, cex=2)
