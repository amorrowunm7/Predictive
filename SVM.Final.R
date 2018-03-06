setwd("~/Desktop/School R Files")
> mydata <- read.csv("myfile.csv",header=TRUE)
> mydata


# answer 1 -a , plot data
> with(mydata, plot(X,Y),col=Class,pch=as.numeric(Class))

> library(rpart)
> library(e1071)

> str(data)
function (..., list = character(), package = NULL, lib.loc = NULL, verbose = getOption("verbose"), 
          envir = .GlobalEnv)  
  > str(data)
#'data.frame':	400 obs. of  3 variables:
# $ X    : num  0.05049 0.54442 0.00141 0.29348 0.31342 ...
#  $ Y    : num  0.6438 0.6511 0.0106 0.465 0.2203 ...
#  $ Class: Factor w/ 2 levels "a","b": 1 1 1 1 1 1 1 1 1 1 ...
> summary(mydata)
# X                    Y             Class  
#Min.   :-0.9934500   Min.   :-0.977040   a:200  
#1st Qu.:-0.3968300   1st Qu.:-0.386622   b:200  
#Median :-0.0005582   Median :-0.002997          
#Mean   :-0.0018795   Mean   : 0.016042          
#3rd Qu.: 0.4092075   3rd Qu.: 0.389570          
#Max.   : 0.9583000   Max.   : 0.981050          

#build svm model
> svm.model <- svm(Class ~ ., data=mydata, cost=100, gamma=1)
> summary(svm.model)

## Call:
## svm(formula = Class ~ ., data = mydata, cost = 100, gamma = 1)

## Parameters:
## SVM-Type:  C-classification 
## SVM-Kernel:  radial 
## cost:  100 
## gamma:  1 

##Number of Support Vectors:  31

##( 16 15 )


## Number of Classes:  2 

#Levels: 
# a b

# answer 1-b
#svm
# radical


> svm_tune <- tune.svm (Class ~ . , data=mydata, kernel= "radial", cost=10^(-1:2), gamma=c (0.5,1,2))
> svm_tune
> plot(svm_tune,color.palette = terrain.colors)

##Parameter tuning of ‘svm’:

# - sampling method: 10-fold cross validation 

##- best parameters:
##  gamma cost
## 0.5  100

## - best performance: 0.02 

> svm_tune$performances

## gamma  cost  error dispersion
## 1    0.5   0.1 0.0650 0.05296750
## 2    1.0   0.1 0.0625 0.05303301
## 3    2.0   0.1 0.0625 0.05559027
## 4     0.5   1.0 0.0675 0.05143766
## 5    1.0   1.0 0.0550 0.04972145
## 6    2.0   1.0 0.0600 0.05163978
## 7    0.5  10.0 0.0525 0.03987829
## 8    1.0  10.0 0.0550 0.04533824
## 9    2.0  10.0 0.0400 0.03162278
## 10   0.5 100.0 0.0200 0.03073181
## 11   1.0 100.0 0.0250 0.02635231
## 12   2.0 100.0 0.0375 0.04124790

> svm_tune$best.parameters

## gamma cost
## 10   0.5  100

# Tune the best model 

> svm_tune <- tune.svm (Class ~ . , data=mydata, kernel= "radial", cost=10^(-1:2), gamma=c (10,0.5,100))

> svm_tune

## Parameter tuning of ‘svm’:

## - sampling method: 10-fold cross validation 

## - best parameters:
##   gamma cost
## 0.5  100

## - best performance: 0.0125 

> plot(svm_tune,color.palette = terrain.colors)
> svm.model <- svm(Class ~ ., data=mydata, cost=100,gamma=0.5)
> svm.model

## Call:
> svm(formula = Class ~ ., data = mydata, cost = 100, gamma = 0.5)

## Parameters:
##SVM-Type:  C-classification 
## SVM-Kernel:  radial 
## cost:  100 
## gamma:  0.5 
## Number of Support Vectors:  39

# Plot the worst model 

> svm_tune <- tune.svm (Class ~ . , data=mydata, kernel= "radial", cost=10^(-1:2), gamma=c (12,2,100))
> svm_tune

## Parameter tuning of ‘svm’:
## - sampling method: 10-fold cross validation 

## - best parameters:
## gamma cost
## 2   100

## - best performance: 0.03

 > plot(svm_tune,color.palette = terrain.colors)

> svm.model <- svm(Class ~ ., data=mydata, cost=10,gamma=5)
> svm.model

## Call:
## svm(formula = Class ~ ., data = mydata, cost = 10, gamma = 5)

## Parameters:
## SVM-Type:  C-classification 
## SVM-Kernel:  radial 
## cost:  100 
## gamma:  2

## Number of Support Vectors:  34

##########################################

# Part b

> setwd("~/Desktop/School R Files")
> mydata <- read.csv("myfile.csv",header=TRUE)
> mydata

> # answer 1 -b

> library(rpart)
> library(e1071)

> obj= best.tune (svm, Class ~., data=mydata, kernel = "sigmoid")
> summary(obj)

Call:
best.tune(svm, Class ~ ., data = mydata, kernel = "sigmoid")


Parameters:
   SVM-Type:  C-classification 
 SVM-Kernel:  sigmoid 
       cost:  1 
      gamma:  0.5 
     coef.0:  0 

Number of Support Vectors:  251

 ( 125 126 )


Number of Classes:  2 

Levels: 
 a b



> svm_tune <- tune.svm (Class ~ . , data=mydata, kernel= "sigmoid", cost=10^(-1:2), gamma=c (0.5,1,2))
> svm_tune

Parameter tuning of ‘svm’:

- sampling method: 10-fold cross validation 

- best parameters:
 gamma cost
   0.5    1

- best performance: 0.475 

> plot(svm_tune,color.palette = terrain.colors)
> svm_tune$performances
   gamma  cost  error dispersion
1    0.5   0.1 0.6300 0.04830459
2    1.0   0.1 0.5700 0.08482007
3    2.0   0.1 0.4775 0.07016845
4    0.5   1.0 0.4750 0.05400617
5    1.0   1.0 0.4875 0.05803495
6    2.0   1.0 0.5000 0.05400617
7    0.5  10.0 0.4825 0.06129392
8    1.0  10.0 0.5050 0.05868939
9    2.0  10.0 0.5050 0.06540472
10   0.5 100.0 0.4925 0.05898446
11   1.0 100.0 0.5050 0.05986095
12   2.0 100.0 0.4925 0.06129392
> svm_tune$best.parameters
  gamma cost
4   0.5    1
> svm_tune <- tune.svm (Class ~ . , data=mydata, kernel= "sigmoid", cost=10^(-1:2), gamma=c (4,0.5,1))
> svm_tune

Parameter tuning of ‘svm’:

- sampling method: 10-fold cross validation 

- best parameters:
 gamma cost
     4  0.1

- best performance: 0.4525 

> plot(svm_tune,color.palette = terrain.colors)

> plot(svm_tune,color.palette = terrain.colors)

> svm.model <- svm(Class ~ ., data=mydata, kernel = " sigmoid " cost=1, gamma=0.5)

> svm.model <-svm (Class ~., data=mydata, kernel ="sigmoid", cost=1,gamma= 0.5)
> svm.model

Call:
svm(formula = Class ~ ., data = mydata, kernel = "sigmoid", cost = 1, gamma = 0.5)


Parameters:
   SVM-Type:  C-classification 
 SVM-Kernel:  sigmoid 
       cost:  1 
      gamma:  0.5 
     coef.0:  0 

Number of Support Vectors:  251

> svm_tune <- tune.svm (Class ~ . , data=mydata, kernel= "sigmoid", cost=10^(-1:2), gamma=c (9,2,10))
> svm_tune

Parameter tuning of ‘svm’:

- sampling method: 10-fold cross validation 

- best parameters:
 gamma cost
     9  0.1

- best performance: 0.51 

> svm.model <-svm (Class ~., data=mydata, kernel ="sigmoid", cost=10,gamma= 9)
> svm.model

Call:
svm(formula = Class ~ ., data = mydata, kernel = "sigmoid", cost = 10, gamma = 9)


Parameters:
   SVM-Type:  C-classification 
 SVM-Kernel:  sigmoid 
       cost:  10 
      gamma:  9 
     coef.0:  0 

Number of Support Vectors:  203