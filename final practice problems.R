## Problem 1
am.glm <- glm(am ~ hp+wt, data=mtcars,family=binomial)
summary(am.glm)
## Now we need to set up a data frame with hp and wt
new1 <- data.frame(hp=120,wt=2.8)
predict(am.glm, newdata=new1, type="response")
## This will show the probabilty the car has a manual transmission 
flu <- read.table("flushot.txt", header=TRUE)
str(flu)

flu.glm <- glm(shot~.,family=binomial,data=flu)
summary(flu.glm)
new2<- data.frame(gender=0, age=65, aware =50)
predict(flu.glm,newdata=new2,type="response")
library(RcmdrMisc)
stepwise(flu.glm)
## since this model was not great, we are going to do this 

flu2.glm <- glm(shot~ age+aware,family=binomial,data=flu)
summary(flu2.glm)
new3 <- data.frame(age=65,aware=50)
predict(flu2.glm,newdata=nw3,type="response")


## problem 3 
ger1 <- read.table("geriatric.text",header=TRUE)
str(ger1)
## this will be the same as above, except for family we will enter in in  poisson, instead of binomial
ger1.glm <- glm(falls~.,family=poisson, data=ger1)
summary(ger1.glm)
## predict the first time then we are going to run the stepwise

new4 <- data.frame(gender=0,inter=0,balance=60,strength=55)
predict(ger1.glm,newdata=new4,type="response")
stepwise(ger1.glm)

ger2.glm <- glm(falls ~ inter+bal, family=poisson,data=ger1)
summary(ger2.glm)

### now lets try the new predictionmodel that will be gender free

new4 <- data.frame(inter=0, bal=60)
predict(ger2.glm,newdata=new5,type="response")





