rm(list = setdiff(ls(), lsf.str()))

d <- read.csv("D:/UIC Fall/Data Mining/HW/8/Assignments/HW7/Supplement/UV6696-XLS-ENG - Copy.csv")

d$Churn = factor(d$Churn)

d=d[,2:13]

str(d)

boxplot(d$CHI.Score.Month.0)$out

Out1 = boxplot(d$CHI.Score.0.1)$out
d$CHI.Score.0.1 = ifelse(d$CHI.Score.0.1%in%Out1, NA, d$CHI.Score.0.1)
boxplot(d$CHI.Score.0.1)$out
d$CHI.Score.0.1[is.na(d$CHI.Score.0.1)] = mean(d$CHI.Score.0.1, na.rm = TRUE)
boxplot(d$CHI.Score.0.1)$out

Out2 = boxplot(d$Support.Cases.Month.0)$out
d$Support.Cases.Month.0 = ifelse(d$Support.Cases.Month.0%in%Out2, NA, d$Support.Cases.Month.0)
d$Support.Cases.Month.0[is.na(d$Support.Cases.Month.0)] = mean(d$Support.Cases.Month.0, na.rm = TRUE)
boxplot(d$Support.Cases.Month.0)$out

Out3 = boxplot(d$CHI.Score.0.1)$out
d$CHI.Score.0.1 = ifelse(d$CHI.Score.0.1%in%Out3, NA, d$CHI.Score.0.1)
d$CHI.Score.0.1[is.na(d$CHI.Score.0.1)] = mean(d$CHI.Score.0.1, na.rm = TRUE)
boxplot(d$CHI.Score.0.1)$out

Out4 = boxplot(d$Support.Cases.0.1)$out
d$Support.Cases.0.1 = ifelse(d$Support.Cases.0.1%in%Out4, NA, d$Support.Cases.0.1)
d$Support.Cases.0.1[is.na(d$Support.Cases.0.1)] = mean(d$Support.Cases.0.1, na.rm = TRUE)
boxplot(d$Support.Cases.0.1)$out

Out5 = boxplot(d$SP.Month.0)$out
d$SP.Month.0 = ifelse(d$SP.Month.0%in%Out5, NA, d$SP.Month.0)
d$SP.Month.0[is.na(d$SP.Month.0)] = mean(d$SP.Month.0, na.rm = TRUE)
boxplot(d$SP.Month.0)$out

Out6 = boxplot(d$SP.0.1)$out
d$SP.0.1 = ifelse(d$SP.0.1%in%Out6, NA, d$SP.0.1)
d$SP.0.1[is.na(d$SP.0.1)] = mean(d$SP.0.1, na.rm = TRUE)
boxplot(d$SP.0.1)$out

Out7 = boxplot(d$Logins.0.1)$out
d$Logins.0.1 = ifelse(d$Logins.0.1%in%Out7, NA, d$Logins.0.1)
d$Logins.0.1[is.na(d$Logins.0.1)] = mean(d$Logins.0.1, na.rm = TRUE)
boxplot(d$Logins.0.1)$out

Out8 = boxplot(d$Blog.Articles.0.1)$out
d$Blog.Articles.0.1 = ifelse(d$Blog.Articles.0.1%in%Out8, NA, d$Blog.Articles.0.1)
d$Blog.Articles.0.1[is.na(d$Blog.Articles.0.1)] = mean(d$Blog.Articles.0.1, na.rm = TRUE)
boxplot(d$Blog.Articles.0.1)$out

Out9 = boxplot(d$Views.0.1)$out
d$Views.0.1 = ifelse(d$Views.0.1%in%Out9, NA, d$Views.0.1)
d$Views.0.1[is.na(d$Views.0.1)] = mean(d$Views.0.1, na.rm = TRUE)
boxplot(d$Views.0.1)$out

Out10 = boxplot(d$Days.Since.Last.Login.0.1)$out
d$Days.Since.Last.Login.0.1 = ifelse(d$Days.Since.Last.Login.0.1%in%Out10, NA, d$Days.Since.Last.Login.0.1)
d$Days.Since.Last.Login.0.1[is.na(d$Days.Since.Last.Login.0.1)] = mean(d$Days.Since.Last.Login.0.1, na.rm = TRUE)
boxplot(d$Days.Since.Last.Login.0.1)$out

Logit = glm(Churn~., data = d, family = "binomial")

step(Logit, scope=list(upper=Logit), direction="both")

pred=predict(Logit,d, type="response")

sum(is.na(d$Support.Cases.0.1))
