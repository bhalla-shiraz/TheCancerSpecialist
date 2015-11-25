#import libraries
library(rpart)
library(e1071)
library(lattice)
library(ggplot2)
library(caret)
library(neuralnet)

header <- F
dataURL <- "BreastCancerData.csv"
d<-read.csv(dataURL,header = header)


c = 2
d[[c]] <- factor(d[[c]],labels = c(0,1))


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
y <- as.formula(y)

an <- ""

for(i in 1:length(cnames)){
  if(c != i){
    an[length(an) + 1] <- cnames[i]
  }
}

bn = an[2:length(an)]


## build the neural network

trainingData[[c]] <- as.numeric(trainingData[[c]])
testData[[c]] <- as.numeric(testData[[c]])
nNet <- neuralnet(y, trainingData, hidden = 4, lifesign = "minimal", 
                  linear.output = FALSE, threshold = 0.1)

# test
temp_test <- subset(testData, select = bn)
nNet.results <- compute(nNet, temp_test)

results <- data.frame(actual = testData[[c]], prediction = nNet.results$net.result)

results$prediction <- round(results$prediction)
correct = sum(results$actual == results$prediction)
total =  sum(results$prediction == results$prediction)
acc = correct / total

all <- list(acc=acc)
