library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(e1071)
header <- F
dataURL <- "BreastCancerData.csv"
d<-read.csv(dataURL,header = header)
c = 2
d[[c]] <- factor(d[[c]],labels = c(0,1))


for( i in 1: nrow(d)){
  for( j in 1:ncol(d)){
    if ( d[i,j] == '?'){
      print(d[i,j])
      d[i,j] <- gsub("?",0,d[i,j], fixed = TRUE)
    }
  }
}



sampleInstances<-sample(1:nrow(d),size = 0.5*nrow(d))
trainingData<-d[sampleInstances,]
testData<-d[-sampleInstances,]




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


model1<- svm(y, data = trainingData, kernel = 'polynomial', degree = '2',  
             cost = '10')


testData$V2 <- as.numeric(testData$V2)






testData[1,1] <-7877.0
testData[1,2] <- 20.29
testData[1,3] <- 14.34
testData[1,4] <- 135.1
testData[1,5] <- 1297.0
testData[1,6] <- 0.1003
testData[1,7] <- 0.1328
testData[1,8] <- 0.198
testData[1,9] <- 0.1043
testData[1,10] <- 0.1809
testData[1,11] <- 0.05883
testData[1,12] <- 0.7572
testData[1,13] <- 0.7813
testData[1,14] <- 5.438
testData[1,15] <- 94.44
testData[1,16] <- 0.01149
testData[1,17] <- 0.02461
testData[1,18] <- 0.05688
testData[1,19] <- 0.01885
testData[1,20] <- 0.01756
testData[1,21] <- 0.005115
testData[1,22] <- 22.54
testData[1,23] <- 16.67
testData[1,24] <- 152.2
testData[1,25] <- 1575.0
testData[1,26] <- 0.1374
testData[1,27] <- 0.205
testData[1,28] <- 0.4
testData[1,29] <- 0.1625
testData[1,30] <- 0.2364
testData[1,31] <- 0.07678
testData[1,32] <- 0.0
testData <- testData[1,]

testData$V2 <- as.factor(testData$V2)

prediction <- predict(model1, testData, type = "class")
if(prediction == "M") result <- "M" else result <- "B"



all <- list(result=result)


