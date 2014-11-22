# Libraries
library(caret)
library(randomForest)

# Downloading the data
if(!file.exists("data")){
        dir.create("data")
}
trainUrl<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
if(!file.exists("data/train.csv")){
        download.file(trainUrl, destfile="./data/train.csv", method="auto")
}
if(!file.exists("data/test.csv")){
        download.file(testUrl, destfile="./data/test.csv", method="auto")
}
dateDownloaded<-date()

# loading the data
data<-read.csv("./data/train.csv", , na.strings = c("NA", ""))
final_test<-read.csv("./data/test.csv", na.strings = c("NA", ""))

# Exploring the data
str(data)
names(data)
summary(data$classe)


# Cross Validation
set.seed(0)
inTrain = createDataPartition(y=data$classe, p=0.7, list=FALSE)
training = data[inTrain,]
testing = data[-inTrain,]
dim(training);dim(testing)

# Clearing out variables with too many missing values.
missingvals = sapply(training, function(x) {sum(is.na(x))})
table(missingvals)
# 100 columns have 13767 missing values, we must filter them out from all dataframes.
tbexcluded<- names(missingvals[missingvals !=0])
training = training[, !names(training) %in% tbexcluded]
testing = testing[, !names(testing) %in% tbexcluded]
# final_test = final_test[, !names(final_test) %in% tbexcluded]
str(training)

# Clearing variables with not much sense like time stamps, usernames now names etc
training = training[, - c(1:7)]
testing = testing[, - c(1:7)]

# Still 53 variables, let's do PCA
dim(training)

# Model Building 

# Principal COmponents
preProc <- preProcess(training[,-53],method="pca",thresh = 0.95)
preProc

# Forget about initial variables, we now use the Principal Components. (25)
trainTransformed <- predict(preProc, training[,-53])
testTransformed <- predict(preProc, testing[,-53])
dim(trainTransformed)

# Random Forest
#modelFit <- train(training$classe ~ ., data = trainTransformed, method="rf")
modelFit <- randomForest(trainTransformed,training$classe, do.trace = TRUE)
modelFit
##Accuracy 
predicts<-predict(modelFit,testTransformed)
confusionMatrix(testing$classe,predicts)


###################################
#######  Submission  ##############
###################################

# Clean the test data
final_test = final_test[, !names(final_test) %in% tbexcluded]
dim(final_test)
final_test = final_test[, - c(1:7)]
dim(final_test)
final_final_test <- predict(preProc, final_test[,-53])

answers <- predict(modelFit,final_final_test)
submission<-as.character(answers)

pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

pml_write_files(submission)
