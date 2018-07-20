setwd('C:/Users/Pradeeepkumar/Desktop/Product Classfication')

#Load train data
Whole<- read.csv('train.csv')

library(caret) 

train_data<-Whole[2:95]
#spliting 
set.seed(123)
smp_size <- floor(0.70 * nrow(train_data))
## set the seed to make your partition reproductible
train_ind <- sample(seq_len(nrow(train_data)), size = smp_size)
#train
train<- train_data[train_ind, ]
#test
test <- train_data[-train_ind, ]

#LDA
library(MASS)
trainlda <- lda(target ~., data = train,prior = c(1,1,1,1,1,1,1,1,1)/9)
predlda<-predict(trainlda,test[1:93])
#pred
Confusion_lda_nopca<-table(predlda$class,test$target)
confusionMatrix(Confusion_lda_nopca)

# library(randomForest)
# rf <- randomForest(target~ ., data=train)
# #varImpPlot(rf)
# #prediction using Decision Trees
# predrf<- predict(rf,test[1:93],type="class")
# #Confusion Matrix for Random Forest
# Confusion_rf_nopca<-table(predrf,test[,1])
# confusionMatrix(Confusion_rf_nopca)


#PCA
str(train_data)
prin_comp <- prcomp(train_data[-94], scale. = T)
plot(prin_comp, type = "l")
summary(prin_comp)
names(prin_comp)
#Eigen values
prin_comp$sdev^2
#Removing Unneccesarry components
Pc<-data.frame(prin_comp$x)
#Eigen greater than 1
PCA<-data.frame(Pc[1:26])

biplot(prin_comp, scale = 0)
std_dev <- prin_comp$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
points(x=26, y=0.60194,cex = 1.5, col = "dark red")



#preparing data
data<- data.frame(target_out = Whole$target, PCA)


#spliting 
set.seed(123)
smp_size <- floor(0.70 * nrow(data))
## set the seed to make your partition reproductible
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
#train
train<- data[train_ind, ]
#test
test <- data[-train_ind, ]

#nB
library(e1071)
model <- naiveBayes(as.factor(target_out) ~ ., data = train)
nbPred<-predict(model,test[2:27])
Confusion<-table(nbPred,test[,1])
Confusion
confusionMatrix(Confusion)

#Decision Tree Algorithm
library(rpart)
Dt<-rpart(target_out~.,data=train,method="class",control=rpart.control(minsplit=20, cp=0))
#prediction using Decision Tree
Pred<- predict(Dt,test[2:27],type="class")
#Confusion Matrix for Decision Trees
Confusion<-table(Pred,test[,1])
Confusion
library(caret) 
confusionMatrix(Confusion)



#multinomial Regression 
library(nnet)
trainnb <- multinom(target_out ~., data = train)
#trainnb$xlevels
aucpred<-predict(trainnb,test[2:27])
Confusion<-table(aucpred,test[,1])
confusionMatrix(Confusion)


#rm(list=ls())
#LDA
library(MASS)
trainlda <- lda(target_out ~., data = train,prior = c(1,1,1,1,1,1,1,1,1)/9)
pred_lda<-predict(trainlda,test[2:27])
Confusion<-table(pred_lda,test[,1])
confusionMatrix(Confusion)

#Random Forest Algorithm
library(randomForest)
rf <- randomForest(target_out ~ ., data=train)
#prediction using Decision Trees
Pred<- predict(rf,test[2:27],type="class")
#Confusion Matrix for Random Forest
Confusion<-table(Pred,test[,1])
confusionMatrix(Confusion)

#Kaggle Submission
p_comp <- prcomp(Kaggle, scale. = T)
Pca<-data.frame(p_comp$x)
PCA<-data.frame(Pca[1:26])
Pred<- predict(rf,PCA,type="class")
Pred
write.csv(Pred, file = "fo.csv")
