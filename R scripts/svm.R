header <- F
dataURL <- "BreastCancerData.csv"
d<-read.csv(dataURL,header = header)

library(lattice)
library(ggplot2)
library(caret)

library(rpart)
library(e1071)

#c = as.integer(args[3])
c = 2
str(d)
d[[c]] <- factor(d[[c]],labels = c(0,1))


for( i in 1: nrow(d)){
  for( j in 1:ncol(d)){
    if ( d[i,j] == '?'){
      print(d[i,j])
      d[i,j] <- gsub("?",0,d[i,j], fixed = TRUE)
    }
  }
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



y <- as.formula(y)


model1<- svm(y, data = trainingData, kernel = 'polynomial', degree = '2',  cost = '10')
#model1<- svm(y, data = trainingData, kernel = 'linear', scale = FALSE)
#model1<- svm(y, data = trainingData, cost = 100, gamma = 1)
prediction <- predict(model1, testData, type = "class")


#prediction <- as.integer(prediction)
#a<-matrix(list(), nrow=2, ncol=length(prediction))
prediction <-sapply(prediction, FUN=function(x) if (x == 0) 0 else 1)



accuracy <- table(prediction, testData[[c]])

if(nrow(accuracy) == 1)accuracy = accuracy[1]/nrow(testData)else{
  #Accuracy given by confusionMatrix command
  confuse_mat <- confusionMatrix(accuracy)
  overal <- confuse_mat$overall
  accuracy <- overal['Accuracy']
  #acc <- as.double(accuracy)
  acc <-  as.numeric(format(round(accuracy, 2), nsmall = 2))
  
}
all <- list(acc=acc)
