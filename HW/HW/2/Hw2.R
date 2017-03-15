#Removing Outliers from all the columns
#Coulmn 1
Outliers1 = boxplot(hw2$Times.Pregnant)$out
View(Outliers1)
hw2$Times.Pregnant = ifelse(hw2$Times.Pregnant%in%Outliers1, NA,hw2$Times.Pregnant)
boxplot(hw2$Times.Pregnant)

#Coulmn 2
Outliers2 = boxplot(hw2$P.Glucose.conc)$out
View(Outliers2)
hw2$P.Glucose.conc = ifelse(hw2$P.Glucose.conc%in%Outliers2, NA,hw2$P.Glucose.conc)
boxplot(hw2$P.Glucose.conc)

#Column 3
Outliers3 = boxplot(hw2$Diastolic.BP)$out
View(Outliers3)
hw2$Diastolic.BP = ifelse(hw2$Diastolic.BP%in%Outliers3, NA,hw2$Diastolic.BP)
boxplot(hw2$Diastolic.BP)

#Column 4
Outliers4 = boxplot(hw2$Triceps.skin.thickness)$out
View(Outliers4)
hw2$Triceps.skin.thickness = ifelse(hw2$Triceps.skin.thickness%in%Outliers4, NA,hw2$Triceps.skin.thickness)
boxplot(hw2$Triceps.skin.thickness)

#Column 5
Outliers5 = boxplot(hw2$X2.Hour.serum.insulin)$out
View(Outliers5)
hw2$X2.Hour.serum.insulin = ifelse(hw2$X2.Hour.serum.insulin%in%Outliers5, NA,hw2$X2.Hour.serum.insulin)

#Column 6
Outliers6 = boxplot(hw2$Body.mass.index)$out
View(Outliers6)
hw2$Body.mass.index= ifelse(hw2$Body.mass.index%in%Outliers6, NA,hw2$Body.mass.index)

#Column 7
Outliers7 = boxplot(hw2$Diabetes.pedigree.function)$out
View(Outliers7)
hw2$Diabetes.pedigree.function = ifelse(hw2$Diabetes.pedigree.function%in%Outliers7, NA,hw2$Diabetes.pedigree.function)

#Column 8
Outliers8 = boxplot(hw2$Age)$out
View(Outliers8)
hw2$Age = ifelse(hw2$Age%in%Outliers8, NA,hw2$Age)

#Changing Target variablee into a Factor
hw2$Class.variable=factor(hw2$Class.variable)

#Set Seed and Spilt data into traning and testing
set.seed(123);
hw = sample(2, nrow(hw2), replace  = T, prob = c(0.8, 0.2))
Trainhw = hw2[hw == 1,]
Testhw = hw2[hw == 2,]

dim(Trainhw)
dim(Testhw)

#Plot CTREE
library("party")
hw2_tree = ctree(Class.variable ~ ., data = Trainhw)
print(hw2_tree)
plot(hw2_tree) 
plot(hw2_tree, type = "simple")
predict(hw2_tree)

#Checking the result on Traning data
table(predict(hw2_tree), Trainhw$Class.variable)

#Checking the result on test data
table(predict(hw2_tree, newdata = Testhw), Testhw$Class.variable)

#Rtree
library(rpart)
str(Trainhw)
hw2_Rpart = rpart(Class.variable ~ ., data = Trainhw, parms = list(split = "gini") )
print(hw2_Rpart)
plot(hw2_Rpart)
text(hw2_pruning, use.n = T, xpd= T)

predict(hw2_Rpart)
rmtable(predict(hw2_Rpart), Trainhw$Class.variable)

#Pruning the tree
opt = which.min(hw2_Rpart$cptable[,"xerror"])
cp = hw2_Rpart$cptable[opt, "CP"]
hw2_pruning = prune(hw2_Rpart, cp = cp)

plot(hw2_pruning)
text(hw2_pruning, use.n = T, xpd= T)

