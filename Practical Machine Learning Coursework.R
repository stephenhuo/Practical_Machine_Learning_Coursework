#You should create one R script called Practical Machine Learning Coursework.R that does the following: 
# 1.preprocess the training and the test sets .
# 2.split the dataset into 7:3
# 3.plot a correlation matrix
# 4.fit a model to predict the classe using everything else as a predictor
# 5.crossvalidate the model using the remaining 30% of data
# 6.Predict on Testset

library(caret)
library(kernlab)
library(randomForest)
library(corrplot)



# data preprocessing
data_training <- read.csv("./Practical_ML/pml-training.csv", na.strings= c("NA",""," "))
data_training_NAs <- apply(data_training, 2, function(x) {sum(is.na(x))})
data_training_clean <- data_training[,which(data_training_NAs == 0)]
data_training_clean <- data_training_clean[8:length(data_training_clean)]


data_test <- read.csv("./Practical_ML/pml-testing.csv", na.strings= c("NA",""," "))
data_test_NAs <- apply(data_test, 2, function(x) {sum(is.na(x))})
data_test_clean <- data_test[,which(data_test_NAs == 0)]
data_test_clean <- data_test_clean[8:length(data_test_clean)]


# split the cleaned testing data into training and cross validation
inTrain <- createDataPartition(y = data_training_clean$classe, p = 0.7, list = FALSE)
training <- data_training_clean[inTrain, ]
crossval <- data_training_clean[-inTrain, ]

# plot a correlation matrix
correlMatrix <- cor(training[, -length(training)])
corrplot(correlMatrix, order = "FPC", method = "circle", type = "lower", tl.cex = 0.8,  tl.col = rgb(0, 0, 0))

# fit a model to predict the classe using everything else as a predictor
model <- randomForest(classe ~ ., data = training)

# crossvalidate the model using the remaining 30% of data
predictCrossVal <- predict(model, crossval)
confusionMatrix(crossval$classe, predictCrossVal)



# predict the classes of the test set
predictTest <- predict(model, data_test_clean)
