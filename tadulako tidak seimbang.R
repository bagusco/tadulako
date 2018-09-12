#membaca data
data <-read.csv("D:/bank-additional-full.csv",sep=";")
library(caret)
library(rpart)

nrow(data) #melihat banyaknya observasi

#melihat distribusi kelas variabel target
table(data$y)
prop.table(table(data$y))

#mempartisi data
set.seed(100)
test_idx <- createDataPartition(data$y, p=0.3, list=FALSE)

data.test <- data[test_idx,]  #membuat data testing
data.trn <- data[-test_idx,]  #membuat data training

nrow(data.test) #banyaknya observasi data training
nrow(data.trn) #banyaknya observasi data testing


#pemodelan TREE pada data training
#tidak ada perlakuan apa-apa terhadap data training
tree <- rpart(y~., data=data.trn, method="class")

#memprediksi data testing
prob_tree<-predict(tree,data.test)[,2]
pred_tree<-as.factor(ifelse(prob_tree<0.5,"no","yes"))

#mengevaluasi ketepatan prediksi dari model
eval.tree <-confusionMatrix(pred_tree, data.test$y, positive="yes")

eval.tree

#underbagging
set.seed(500)
k<-100
pred_tree_under1<-matrix(NA,nrow(data.test),k)
for(i in 1:k){
  down_train1 <- downSample(x = data.trn[, -ncol(data.trn)],y = data.trn$y)
  table(down_train1$Class)
  
  tree_under_mod1<-rpart(Class~., data=down_train1, method="class")
  prob_tree_under1<-predict(tree_under_mod1, data.test)[,2]
  pred_tree_under1[,i]<-ifelse(prob_tree_under1<0.5, 0, 1)
}

pred_tree_under1a <- apply(pred_tree_under1,1,sum)
pred_tree_under1a <- as.factor(ifelse(pred_tree_under1a<k/2,"no","yes"))

eval.underbag<-confusionMatrix(pred_tree_under1a, data.test$y, positive="yes")

eval.underbag


#overbagging
set.seed(600)
k<-100
pred_tree_over1<-matrix(NA,nrow(data.test),k)
for(i in 1:k){
  up_train1 <- upSample(x = data.trn[, -ncol(data.trn)],y = data.trn$y)
  table(up_train1$Class)
  
  tree_over_mod1<-rpart(Class~., data=up_train1, method="class")
  prob_tree_over1<-predict(tree_over_mod1, data.test)[,2]
  pred_tree_over1[,i]<-ifelse(prob_tree_over1<0.5, 0, 1)
}

pred_tree_over1a<-apply(pred_tree_over1,1,sum)
pred_tree_over1a<-as.factor(ifelse(pred_tree_over1a<k/2,"no","yes"))

eval.overbag<-confusionMatrix(pred_tree_over1a, data.test$y, positive="yes")

eval.overbag


#RUSBoost
library(ebmc)
set.seed(700)
train_rus<- data.trn
test_rus <- data.test
train_rus$y <- factor(train_rus$y, levels = c("no", "yes"), labels = c("0", "1"))
test_rus$y <- factor(test_rus$y, levels = c("no", "yes"), labels = c("0", "1"))

rus_boost_mod <- rus(y ~ ., data = train_rus, size = 10, 
                     alg = "c50", ir = 1)

prob_rus_boost<-predict(rus_boost_mod, newdata=test_rus, type="prob")
pred_rus_boost<-as.factor(ifelse(prob_rus_boost<0.5,"no","yes"))

eval.rusboost<-confusionMatrix(pred_rus_boost, data.test$y, positive="yes")
eval.rusboost

#easyensemble
library(ada)
library(ROSE)
set.seed(800)
easy_mod <- NULL
for (z in 1:10){
  easy_trn <- downSample(x = data.trn[, -ncol(data.trn)],y = data.trn$y)
  easy_mod[[z]] <- ada(Class~.,data=easy_trn,type="discrete")
  print(z)
}

pred_iter <- matrix(NA,nrow(data.test),length(easy_mod))
for (z in 1:length(easy_mod)){
  pred_iter[,z] <- predict(easy_mod[[z]],data.test, type="F")
}

pred_easy <- sign(apply(pred_iter,1,sum))
pred_easy <- as.factor(ifelse(pred_easy==-1,"no","yes"))
eval.easy <- confusionMatrix(pred_easy,data.test$y,positive="yes")
eval.easy
