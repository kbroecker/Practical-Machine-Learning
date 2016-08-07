library(RCurl)
train <- getURL('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv')
test <- getURL('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv')

lift_train <- read.csv(textConnection(train), header=TRUE, na.strings = c("NA", "#DIV/0!", ""))

head(lift_train)

lift_test <- read.csv(textConnection(test), header=TRUE, na.strings = c("NA", "#DIV/0!", ""))

head(lift_test)

missing_col <- colnames(lift_test) [colSums(is.na(lift_test))>0]
lift_train_clean <- lift_train[, !(names(lift_train) %in% missing_col)]
dim(lift_train_clean)
lift_test_clean <- lift_test[, !(names(lift_test) %in% missing_col)]
dim(lift_test_clean)

library(caret)
nzv <- nearZeroVar(lift_test_clean, saveMetrics = TRUE)
nzv

lift_test_clean <- lift_test_clean[, nzv$nzv==FALSE]
lift_test_final <- lift_test_clean[, -(1:5), drop=FALSE]
dim(lift_test_final)

lift_train_clean <- lift_train_clean[, nzv$nzv==FALSE]
lift_train_final <- lift_train_clean[, -(1:5), drop=FALSE]
dim(lift_train_final)

set.seed(4444)
inTrain = createDataPartition(lift_train_final$classe, p = .70, list=FALSE)
training = lift_train_final[inTrain,]
validation = lift_train_final[-inTrain,]

dim(training)
dim(validation)
library(randomForest)

fit <- train(classe ~., method="rf", data=training, ntree=500, tuneGrid = data.frame(.mtry = 7), allowParallel=TRUE )

fit$finalModel

predict <- predict(fit, newdata = validation)

confusionMatrix(predict, validation$classe)






