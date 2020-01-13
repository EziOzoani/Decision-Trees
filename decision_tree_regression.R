# Decision Tree Regression

#loading filr --> set it to the current location of the file
df = read.table("/Users/ezi/Desktop/Masters/Modules/DataAna/CA1/ProjectDataGroup1CSV.csv", header = TRUE, sep = ',')


# Splitting the dataset into the training and test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(df$Response, SplitRatio = 2/3)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

# newdata<-data.frame(test_set)

print("**USING DECISION TREES**")
# install.packages('rpart')
# install.packages('rattle')
# install.packages('rpart.plot')
library(rpart)
library(rattle)
library(rpart.plot)


# fitting it over X 
fitX <- rpart(Response ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 ,
             method="class", data=training_set)

prp(fitX)
# fancyRpartPlot(fitX)
PredictionX <- predict(fitX,test_set,type="class")
tree.sseX = sum((as.numeric(PredictionX) - as.numeric(test_set$Response))^2)
tree.sseX

table(as.numeric(PredictionX), as.numeric(test_set$Response))


# fitting it over  Y
fitY <- rpart(Response ~ Y1 + Y2 + Y3 + Y4 + Y5 + Y6 + Y7,
             method="class", data=training_set)

prp(fitY)
# fancyRpartPlot(fitY)
PredictionY <- predict(fitY,test_set,type="class")
tree.sseY = sum((as.numeric(PredictionY) - as.numeric(test_set$Response))^2)
tree.sseY

table(as.numeric(PredictionX), as.numeric(test_set$Response))


# fitting it over X & Y
fit <- rpart(Response ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 +Y1 + Y2 + Y3 + Y4 + Y5 + Y6 + Y7,
             method="class", data=training_set)

prp(fit)
# fancyRpartPlot(fit)
Prediction <- predict(fit,test_set,type="class")
tree.sse = sum((as.numeric(Prediction) - as.numeric(test_set$Response))^2)
tree.sse

table(as.numeric(Prediction), as.numeric(test_set$Response))
