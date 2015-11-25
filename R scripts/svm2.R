testData <- testData[1,]

testData$V2 <- as.factor(testData$V2)

prediction <- predict(model1, testData, type = "class")
if(prediction == "M") result <- "M" else result <- "B"



all <- list(result=result)


