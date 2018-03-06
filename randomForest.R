args = commandArgs(trailingOnly=TRUE)
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)
infile="data_sets/breast-cancer-wisconsin.data.txt"
#infile="data_sets/iris.txt"
#data <- read.table(infile,sep="\t",header = T)
data <- read.table(infile,sep="\t",header=T)
data <- data[,-1] #remove first column
data$Class <- as.factor(data$Class)
index <- sample(nrow(data),nrow(data)/3)

rf.model <- randomForest(Class ~ .,data=data,replace=TRUE,ntree=500,importance=T)
#res <- table(pred = rf.pred, true = test_data[,10])
#get accurary
#print(sum(diag(res))/sum(res))
#dev.new()
#quartz()
pdf(paste(infile,".rf.plots.pdf",sep=""))
plot(rf.model)
plot(c(1:500),rf.model$err.rate[,1],type="l",xlab ="nTrees",ylab="OOB error")

varImpPlot(rf.model,
           sort = T,
           main="Variable Importance",
           n.var=9)

var_imp <- data.frame(importance(rf.model,type=2))
var_imp$Variables <- row.names(var_imp)
var_imp[order(var_imp$MeanDecreaseGini,decreasing = T),]
dev.off()

#tuning rf paramters might take a while
rf_tune <- tune.randomForest(Class ~ ., data = data, cross = 10, ntree = c(10:50:300))
print(rf_tune)

#tuning decision trees parameters
rpart_tune <- tune.rpart(Class ~ . , data = data , minsplit=c(2,10,20),cp=seq(0.0002,0.01,0.0002))






