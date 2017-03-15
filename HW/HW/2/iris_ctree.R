# Partitioning data to Training and Testing data set
set.seed(1234)
ind = sample(2, nrow(iris), replace = T, prob = c(0.7, 0.3))
TrainData = iris[ind == 1, ]
TestData = iris[ind == 2, ]

# Ctree decision tree construction
iris_ctree = ctree(Species ~ ., data = Train)
# You could also do:
# MyFormula = Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
# iris_ctree = ctree(MyFormula, data = TrainData)

# Examine result
print(iris_ctree) # print decision rules
plot(iris_ctree) #plot the decision tree
predict(iris_ctree) # Prediction results on training set
table(predict(iris_ctree), TrainData$Species) # Accuracy of decision tree on training data

#Checking the result on test data
table(predict(iris_ctree, newdata = TestData), Test$Species)