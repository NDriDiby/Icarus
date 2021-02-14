
"Problem.1"

#Data Set
SATimage = read.table("/Users/pm3796gb/Desktop/DSCI 425/Homework.7/SATimage.csv",header = T,sep=",")  
SATimage = data.frame(class=as.factor(SATimage$class),SATimage[,1:36])

#Test/Train
set.seed(888) 
testcases = sample(1:dim(SATimage)[1],1000,replace=F)
SATtest = SATimage[testcases,]
SATtrain = SATimage[-testcases,]

#Classification Tree
library(rpart)
#train
sat.rpart = rpart(class~.,data=SATtrain,control = rpart.control(cp=0.0001))
ypred = predict(sat.rpart,newdata = SATtrain,type="class")
misclass(ypred,SATtrain$class)
#test
ypred = predict(sat.rpart,newdata = SATtest,type="class")
misclass(ypred,SATtest$class)


#Bagging
library(ipred)
#train
sat.bag = bagging(class~.,data=SATtrain,coob=T,control=rpart.control(cp=0.0001,xval=0,minsplit = 10))
ypred = predict(sat.bag,newdata=SATtrain,type='class')
misclass(ypred$class,SATtrain$class)

#test
ypred =predict(sat.bag,newdata=SATtest,type='class')
misclass(ypred$class,SATtest$class)

#boosting
library(adabag)
#train
sat.boost = boosting(class~.,data = SATtrain,mfinal=50)
ypred = predict(sat.boost,newdata=SATtrain,type='class')
misclass(ypred$class,SATtrain$class)

#test
ypred = predict(sat.boost,newdata=SATtest,type='class')
misclass(ypred$class,SATtest$class)


#random forest
library(randomForest)
#train
sat.rdmfo = randomForest(class~.,data=SATtrain,importance=T)
ypred = predict(sat.rdmfo,newdata=SATtrain,type="class")
misclass(ypred,SATtrain$class)

#test
ypred = predict(sat.rdmfo,newdata=SATtest,type="class")
misclass(ypred,SATtest$class)


"Problem.2"

#data
Gtrain = read.csv("/Users/pm3796gb/Desktop/DSCI 425/Homework.7/GenreTrain.csv")
Gtest = read.csv("/Users/pm3796gb/Desktop/DSCI 425/Homework.7/GenreTest.csv")

#train/test
set.seed(1)
sam = sample(1:8000,5000,replace=F)
G.train = Gtrain[sam,]
G.valid = Gtrain[-sam,]

#classification
#train
g.rpart = rpart(GENRE~.,data=G.train,control = rpart.control(cp=0.0001,minsplit = 10,xval=0))
ypred = predict(g.rpart,newdata = G.train,type="class")
misclass(ypred,G.train$GENRE)
#test
ypred = predict(g.rpart,newdata = G.valid,type="class")
misclass(ypred,G.valid$GENRE)


#Bagging
#train
g.bag = bagging(GENRE~.,data=G.train,coob=T,control=rpart.control(cp=0.0001,xval=0,minsplit = 10))
ypred = predict(g.bag,newdata=G.train,type='class')
misclass(ypred,G.train$GENRE)

#test
ypred = predict(g.bag,newdata = G.valid,type="class")
misclass(ypred,G.valid$GENRE)

#random forest
library(randomForest)
#train
g.rdmfo = randomForest(GENRE~.,data=G.train,importance=T)
ypred = predict(g.rdmfo,newdata=G.train,type="class")
misclass(ypred,G.train$GENRE)

#test
ypred = predict(g.rdmfo,newdata=G.valid,type="class")
misclass(ypred,G.valid$GENRE)

#boosting
library(adabag)
#train
g.boost = boosting(GENRE~.,data = G.train,mfinal=50)
ypred = predict(g.boost,newdata=G.train,type='class')
misclass(ypred$GENRE,G.train$GENRE)

#test
ypred = predict(g.boost,newdata=G.valid,type='class')
misclass(ypred$GENRE,G.valid$GENRE)









