#install.packages("e1071")
#install.packages("RTextTools")
#install.packages("caret")

#library(e1071)
#library(RTextTools)
#library(caret)

# Load the data from the csv file
data <- read.csv(file = "/Users/liarthur/Desktop/labeledData.csv", header = TRUE)
#trace("create_matrix", edit=T)
# Create the document term matrix
dtMatrix <- create_matrix(data["data"]) 

# Configure the training data
container <- create_container(dtMatrix, data$result, trainSize=1:6690, virgin=FALSE) 

# train a SVM Model
model <- train_model(container, "SVM", kernel="linear", cost=1)

# new data
predictionData <- read.csv(file = "/Users/liarthur/Desktop/labeledIphone.csv", header = TRUE)
# create a prediction document term matrix 
predMatrix <- create_matrix(predictionData$data, originalMatrix=dtMatrix) 
# create the corresponding container
predSize = length(predictionData);
predictionContainer <- create_container(predMatrix, labels=rep(0,438), testSize=1:438, virgin=FALSE) 
# predict
results <- classify_model(predictionContainer, model)
results$SVM_LABEL

jdata.test <- predictionData[1:438,]
confsvm.mat <- confusionMatrix(results$SVM_LABEL,jdata.test$result)
confsvm.mat$overall['Accuracy']