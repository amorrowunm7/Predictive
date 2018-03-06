

#function to get cutoff based on cost matrix and prior probabilities
getCutoff <- function(CostMatrix,prior)
{
   cutoff = (CostMatrix[1,2]*prior[2])/(CostMatrix[2,1]*prior[1] + CostMatrix[1,2]*prior[2])
   cutoff

}

#function to compute cost cutoff based on cost matrix and prior probabilities

CalculateCost <- function(caret.tune,params,CostMatrix,prior, positiveclassName,negativeclassName)
{ pred <- caret.tune$pred
   
   # compute cost
   Cost = pred[,3]*prior[1]*CostMatrix[2,1] + pred[,4]*prior[2]*CostMatrix[1,2]
   pred$expectedcost = Cost  
   # compute cutoff
   cutoff = getCutoff(CostMatrix,prior)
   print("cutoff ")
   print(cutoff)
   newpred <-  pred$pred
 
   newpred[pred[,3] > cutoff ] = positiveclassName
   newpred[pred[,3] <= cutoff ] = negativeclassName

   pred$newpred <- newpred
  newCol <- params[,1]
  i = 2
  while(i<=(ncol(params)))
  {
    newCol <- paste(newCol,params[,i],sep="_")
    #print(i)
    i=i+1
  }
  params$newCol <- newCol


  newCol <- pred[,6]
  i = 1
  while(i < ncol(params)-1)
  {
   newCol <- paste(newCol,pred[,6+i],sep="_")
   i=i+1
  }
  pred$newCol <- newCol

  folds <- unique(pred$Resample)
  Result <- data.frame(para = params$newCol,cost=rep(0,nrow(params)))
  Result$newAccuracy <- Result[,2]
  Result$newKappa <- Result[,2]
  Result$newSensitivity <- Result[,2]
  Result$newSpecificity <- Result[,2]
  
  Result$oldAccuracy <- Result[,2]
  Result$oldKappa <- Result[,2]
  Result$oldSensitivity <- Result[,2]
  Result$oldSpecificity <- Result[,2]


  
  for(i in 1:nrow(params))
  {
    avgCost = 0
      metric_new <- c(0,0,0,0)
      metric_old <- c(0,0,0,0)
    for(f in 1:length(folds))
    {
     d <- pred[grep(params$newCol[i],pred$newCol),]
     d <- d[grep(folds[f], d$Resample),]
     avgCost = avgCost + mean(d$cost)
     c_new <- confusionMatrix(d$newpred, d$obs)
     metric_new <- metric_new + c(c_new$overall['Accuracy'],c_new$overall['Kappa'],c_new$byClass['Sensitivity'],c_new$byClass['Specificity'])
     
     #print(c_new$table)
     c_old <- confusionMatrix(d$pred, d$obs)
     metric_old <- metric_old + c(c_old$overall['Accuracy'],c_old$overall['Kappa'],c_old$byClass['Sensitivity'],c_old$byClass['Specificity'])
     #print(c_old$byClass)
     #print(c_old$table)
    }
        avgCost=avgCost / length(folds)
        metric_new <- metric_new/length(folds)
        metric_old <- metric_old/length(folds)

        Result[i,2] <- avgCost
        Result[i,c(3,4,5,6,7,8,9,10)] <- c(metric_new,metric_old)

  }
  print(Result)
  #plot(c(1:nrow(Result)),Result[,2],xlab = "parameter set",ylab ="cost",type="b")
  Result
}

library(caret)
library(e1071)
############################
#the data might have some attributes which have zero variance#
#might give you some warnings #
#otherwise remove attributes with zero variance
ticdata <- read.table("ticdata.txt",sep="\t",header=TRUE)
#remove some columns which have zero variance
ticdata <- ticdata[,-c(50,71,60,81)]
classCol = 82
ticdata$V86 <- make.names(as.factor(ticdata$V86))

unique(ticdata$V86)

#class imbalance
sum(ticdata$V86=="X0")
sum(ticdata$V86=="X1")

#training set
train_index <- sample(1:nrow(ticdata),1000)
train_data <- ticdata[train_index,]
tmp_data <- ticdata[-train_index,]
tmp_dataX0 <- tmp_data[tmp_data$V86 == "X0",][1:250,]
tmp_dataX1 <- tmp_data[tmp_data$V86 == "X1",][1:50,]
#eval_index <- sample(1:nrow(tmp_data),300)
eval_data <- rbind(tmp_dataX0,tmp_dataX1)


#check number of samples in each class
sum(train_data$V86=="X0")
sum(train_data$V86=="X1")
sum(eval_data$V86=="X0")
sum(eval_data$V86=="X1")



##############################build weighted knn caret
grid = expand.grid(kmax=c(1,3,5,7,9,11,13,15,17,19,21,23,25,27),distance=2,kernel="optimal")
#sampling method choices
ctrl.cross <- trainControl(method="cv",number=3,classProbs=TRUE,savePredictions=TRUE)
#using Caret weighted knn
knnFit.cross <- train(V86 ~ ., data = train_data, method ="kknn",metric="Accuracy",preProc=c("center","scale"),tuneGrid = grid,trControl=ctrl.cross)
plot(knnFit.cross)

#note X0 is the majority Class
#X0 is positive Class here, X1 is negative Class


#########################################################################
costMatrix <- matrix(c(0,1,100,0),nrow=2)
Cost <- CalculateCost(knnFit.cross,grid,costMatrix,c(0.95,0.05),"X0","X1")

##############################build RandomForest caret
grid <- data.frame(mtry=c(10,20,30,40,50,60,80))
rf.cross <- train(V86 ~ ., data = train_data, method ="rf",metric="Accuracy",preProc=c("center","scale"),ntree = 200,tuneGrid = grid,trControl=ctrl.cross)
Cost <- CalculateCost(rf.cross,grid,costMatrix,c(0.95,0.05),"X0","X1")
########################################################################




