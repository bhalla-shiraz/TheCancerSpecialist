#args <- commandArgs(TRUE)
#dataURL<-as.character(args[1])
#header<-as.logical(args[2])
header <- F
dataURL <- "BreastCancerData.csv"
d<-read.csv(dataURL,header = header)



#set.seed(123)

library(rpart)
library(e1071)

#c = as.integer(args[3])
c = 2
d[[c]] <- as.factor(d[[c]])

for(i in 1:ncol(d)){
  a = sapply(d[[i]],is.factor)
  if(a[1] == TRUE && i != c) d[[i]] <- as.numeric(d[[i]])
}



sampleInstances<-sample(1:nrow(d),size = 0.9*nrow(d))
trainingData<-d[sampleInstances,]
testData<-d[-sampleInstances,]
# which one is the class attribute



cnames <- colnames(d)
y <- paste(cnames[c], "~", sep="")
count <- 1
if(c != 1) y <- paste(y,cnames[1] , sep="") else {
  y <- paste(y , cnames[2] , sep="") 
  count = 2
}

for(i in 1:length(cnames)){
  
  if(c != i && i != count){
    y <- paste(y, cnames[i], sep="+")
  }
}
y

str(trainingData)
train_model <- rpart(y,data = trainingData, method = 'class', parms = list(split = 'information'), minsplit = 2, minbucket = 1)

prediction <- predict(train_model, testData, type = "class")
print(prediction)


#prediction <- as.integer(prediction)
#prediction <-sapply(prediction, FUN=function(x) if (x>0) 1 else 0)
#print(prediction)

accuracy <- table(prediction, testData[[c]])
print(accuracy)

library(lattice)
library(ggplot2)
library(caret)


if(nrow(accuracy) == 1)acc = accuracy[1]/nrow(testData)else{
  #Accuracy given by confusionMatrix command
  confuse_mat <- confusionMatrix(accuracy)
  overal <- confuse_mat$overall
  accuracy <- overal['Accuracy']
  acc <- as.double(accuracy)
}
all <- list(acc=acc)

