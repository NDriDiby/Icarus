
"---------#Problem.1---------"

#Load the data
sat =  read.csv("/Users/pm3796gb/Desktop/DSCI 425/Homework.6/SATimage.csv")
satim = data.frame(class=as.factor(sat$class),sat[,1:36])
head(satim)

#Test/Train
set.seed(888)
sam = sample(1:dim(satim)[1],1000,replace=F)
sattest = satim[sam,]
sattrain = satim[-sam,]

#Load library
library(kknn)
library(class)
library(klaR)

#A-
#K-NN model
sat.sknn = sknn(class~.,data = sattrain,kn=3)
ypred = predict(sat.sknn,newdata = sattest)

#misclassification rate
misclass(ypred$class,sattest$class)


#Bayes Classification
sat.bayes = NaiveBayes(class~.,data=sattrain,usekerenl=T)
ypred = predict(sat.bayes,newdata = sattest)

#misclassification rate
misclass(ypred$class,sattest$class)

#B-
#Monte Carlo K-NN
sat.knn = kknn.sscv(satim,B=10,kmax=5,kernel = "triangular")
summary(sat.knn)

#Monte Carlo Bayes
sat.bay = NB.cv(satim[,-1],satim[,1],B=100)
summary(sat.bay)


"-------Problem2---------------"

genretest = read.csv("/Users/pm3796gb/Desktop/DSCI 425/Homework.6/GenreTest.csv")
genretrain = read.csv("/Users/pm3796gb/Desktop/DSCI 425/Homework.6/GenreTrain.csv")
dim(genretrain)
dim(genretest)

#Train
X.train =scale(genretrain[,-192])
Y.train = genretrain[,192]

train.genre = data.frame(Y.train,X.train)

#Test
X.test =scale(genretest[,-192])
Y.test = genretest[,192]

#A-
#K-NN Model

genre.knn = sknn(Y.train~.,data=train.genre,kn=5)
ypred = predict(genre.knn,newdata = as.data.frame(X.test))

#misclassification rate
misclass(ypred$class,Y.test)

#write csv.file



"----------Problem.3---------"
#load data
oil = read.csv("/Users/pm3796gb/Desktop/DSCI 425/Homework.6/Oils.csv")
summary(oil)





