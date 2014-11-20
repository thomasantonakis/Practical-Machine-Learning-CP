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


training<-read.csv("./data/train.csv")
testing<-read.csv("./data/test.csv")

# Exploring the data
library(ggplot2)

str(training)
names(training)

# Model Building
library(caret)


preProc <- preProcess(training[,-160],method="pca",data = training)
trainPC <- predict(preProc,training[,-160])
modelFit <- train(training$classe ~ .,method="glm",data = training)


###################################
#######  Submission  ##############
###################################

answers = rep("A", 20)


pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

pml_write_files(answers)
