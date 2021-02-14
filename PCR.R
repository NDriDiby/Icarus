# load library
library(pls)

#import the data
data(gasoline)

#take a look at the data
head(gasoline)
dim(gasoline)

#plot
gasoline.x = gasoline$NIR
dim(gasoline.x)
matplot(t(gasoline.x),type="l",xlab="Variable",ylab="Spectral Intensity")
title(main="Spectral Readings for Gasoline Data")
pairs.plus(gasoline.x[,1:10])
pairs.plus(gasoline.x[,201:210])
pairs.plus(gasoline.x[,301:310]) 

#Build a principal component regression model
gas.pcr=pcr(octane~scale(NIR),data=gasoline,ncomp=40,validation='CV')
summary(gas.pcr)

#plot the model to get the optimal number component
validationplot(gas.pcr,legendpos="topright")

#using the optimal number of component. 
gas.pcr.op = pcr(octane~scale(NIR),data=gasoline,ncomp=5,validation='CV')
summary(gas.pcr.op)

#look at the loadings
loadingplot(gas.pcr.op,comps=1:2,lty=1:2,lwd=2,legendpos='topright')

#train/test split
gasoline.train = gasoline[1:50,]
gasoline.test = gasoline[51:60,]
attributes(gasoline.train)

#Build the model PCR
gas.train = pcr(octane~scale(NIR),data=gasoline.train,ncomp=6)
ypred = predict(gas.train,ncomp=6,newdata=gasoline.test)
yact = gasoline.test$octane
rmsep = sqrt(mean((yact-ypred)^2))
rmsep
PredAcc(ypred,yact)



#partital least square
gas.pls = plsr(octane~scale(NIR),data=gasoline,ncomp=40,validation='CV')
summary(gas.pls)

#plot the model to get the optimal number component
validationplot(gas.pls,legendpos="topright")

#optimal PC
gas.pls.op = plsr(octane~scale(NIR),data=gasoline,ncomp=6,validation='CV')
summary(gas.pls.op)

#
loadingplot(gas.pls,comps=1:2,legendpos='topright')

#build the model
gas.train = plsr(octane~scale(NIR),data=gasoline.train,ncomp=6)
ypred = predict(gas.train,ncomp=6,newdata=gasoline.test)
yact = gasoline.test$octane
rmsep = sqrt(mean((yact-ypred)^2))
rmsep
PredAcc(ypred,yact)

#Monte carlo for PLS
pls.cv(gasoline$NIR,gasoline$octane,p=.667,B=100,ncomp=6)

#Monte carlo for PCR
pcr.cv(gasoline$NIR,gasoline$octane,p=.667,B=100,ncomp=5)



      "--------------------------------------------------------------------"

#question 2

QSAR.melt = read.table(file.choose(),header=T,sep=',')
set.seed(1)
QSAR.melt = QSAR.melt[,-1] # remove Case column which is an ID
train = sample(nrow(QSAR.melt),3900)
test = -(train)
X = QSAR.melt[,-1]
Xs = scale(X)
QSAR = data.frame(MTP = QSAR.melt$MTP,Xs)
qsar.train = QSAR.melt[train,]
qsar.test = QSAR.melt[test,]

#Bilding PCR model
qsar.pcr = pcr(MTP~.,ncomp = 40,validation="CV",data=QSAR)
summary(qsar.pcr)
validationplot(qsar.pcr)

#PCR model
qsar.pcr.train = pcr(MTP~.,ncomp = 35,validation="CV",data=qsar.train)
ypred = predict(qsar.pcr.train,ncomp=35,newdata=qsar.test)
ytest = qsar.test$MTP
plot(ytest,ypred,xlab='Actual Test MTP',ylab='Predicted Test MTP')
Rsq.pred = 1 -(sum((ypred-ytest)^2,na.rm =TRUE )/sum((ytest- mean(ytest))^2,na.rm = TRUE))
Rsq.pred
summary(qsar.prc.train)

#Bilding PLS model
qsar.pls = plsr(MTP~.,ncomp = 40,validation="CV",data=QSAR)
summary(qsar.pls)
validationplot(qsar.pls)


#Bilding PLS model
qsar.pls.train = pcr(MTP~.,ncomp = 29,validation="CV",data=qsar.train)
validationplot(qsar.pls)
ypred = predict(qsar.pls.train,ncomp=29,newdata=qsar.test)
ytest = qsar.test$MTP
plot(ytest,ypred,xlab='Actual Test MTP',ylab='Predicted Test MTP')
Rsq.pred = 1 -(sum((ypred-ytest)^2,na.rm =TRUE )/sum((ytest- mean(ytest))^2,na.rm = TRUE))
Rsq.pred
summary(qsar.pls.train)

#Cross Validation using Monte Carlo 
pls.cv(Xs,QSAR.melt$MTP,p=.667,B=100)

pcr.cv(Xs,QSAR.melt$MTP,p=.667,B=100)



