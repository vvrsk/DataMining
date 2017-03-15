install.packages("ISLR")
library(ISLR)
data(College)
View(College)


# Create vector of column Max and Min values
maxs = apply(College[,2:18], 2, max) 
mins = apply(College[,2:18], 2, min)

# Use scale() and convert the resulting matrix to a data frame
scaled.data = as.data.frame(scale(College[,2:18], center = mins, scale = maxs - mins))
head(scaled.data)
summary(scaled.data)

# Private = as.numeric(College$Private)-1
Private = College$Private
data = data.frame(Private, scaled.data)
head(data)

# Partitioning data to train and test sets
set.seed(1234)
ind = sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
TrainData = data[ind == 1, ]
TestData = data[ind == 2, ]

#Constructing neural network
library(nnet)
nn  = nnet(Private ~ ., data=TrainData, linout=F, size=10, decay=0.01, maxit=1000)
#linout stands for linear out-put and is used to determine whether the target variable is continuous or not
#size sets the number of units in hidden layer
#decay is the regularization parameter. You can use any value between between zero and one
#maxit denotes the maximum number of iterations taken by the algorithm that computes the weights

#Neural network model out put
summary(nn) #Summary gives you the number units in each layer in addition to all the weights


#To plot the neural network using nnet we need to use devtools
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(nn)
# The darker lines are associated to the higher weights and gray lines are for small weights

nn$wts #if you only want to look at weights you can use wts
nn$fitted.values #This will provide the probability of predicted values for each instance

# Using nnet model on the test data:
nn.preds = predict(nn, TestData, type = "class")
table(TestData$Private, nn.preds)

# Constructing a neural network model using neuralnet function
TrainData$Private = as.numeric(TrainData$Private)-1
Formula = as.formula(paste("Private", paste(colnames(College[,2:18]), collapse="+"), sep="~"))
library(neuralnet)
nn.neuralnet = neuralnet(Formula, data = TrainData, hidden = 3, rep = 10, learningrate = 1, linear.output = F, algorithm = "backprop")
# hidden shows the number of units in the hidden layer. You can include a vector as well if you want to have more than one hidden layer.
# rep indicates the number of repetitions for the training process
# learningrate is the learning rate used in backpropogation algorithm
# You can include the type of algorithm for updating weights in the algorithm argument
# linear.output is the same as linout in nnet

plot(nn.neuralnet)

#To do the prediction in the test data we use compute() function
TestData$Private = as.numeric(TestData$Private)-1
View(TestData)
neural.predict = compute(nn.neuralnet,TestData[,2:18])$net.result #net.result returns the probability of predictions
result = ifelse(neural.predict[,1]>=0.5,1,0) # We have to convert the probabilities to classes using a threshold
table(TestData$Private, result)
