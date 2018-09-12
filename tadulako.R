
#membaca data
alamat <- "D:/"

ion<-read.csv(paste0(alamat,"Ionosphere_ok.csv"))[,-1]
str(ion)
head(ion)

#membagi dataset menjadi dua dataset
library(caret)
set.seed(100)
idx<-createDataPartition(ion$Class, p=0.7, list=FALSE)
train<-ion[idx,]
test<-ion[-idx,]

#rpart
#membuat pohon klasifikasi dan memprediksi data testing
library(rpart)
mod.tree<-rpart(Class~.,data=train,method="class")
prob<-predict(mod.tree,test)[,2]
pred.tree<-as.factor(ifelse(prob>.5,"good","bad"))

#bagging
#bagging dan memprediksi data testing
k<-50
prediksi<-matrix(NA,nrow(test),k)
for(i in 1:k){
  resample <- sample(1:nrow(train), replace=TRUE)
  contoh.boot <- train[resample,]
  tree <-rpart(Class~., data=contoh.boot, method="class")
  prob <-predict(tree, test)[,2]
  prediksi[,i] <-ifelse(prob<0.5, 0, 1)
}
vote1 <-apply(prediksi,1,sum)
pred.bag <- as.factor(ifelse(vote1 < k/2,"bad", "good"))

library(caret)
kinerja.tree <- caret::confusionMatrix(pred.tree,test$Class,positive = "good")
kinerja.bagging <- caret::confusionMatrix(pred.bag,test$Class,positive = "good")

kinerja.tree
kinerja.bagging

#rf
#pemodelan random forest dan memprediksi data testing
library(randomForest)
model.forest <- randomForest(Class~.,data=train, importance=TRUE,
                             ntree=200, mtry=3)
pred.rf <- predict(model.forest, test)

kinerja.rf <-caret::confusionMatrix(pred.rf,test$Class,positive = "good")
kinerja.rf


#boosting
#menjalankan algoritma boosting dan menilai kinerjanya
library(ada)
model.boost <- ada(Class~.,data=train,type="discrete")
pred.boost<-predict(model.boost,test,type="vector")

kinerja.boosting <-caret::confusionMatrix(pred.boost,test$Class,positive = "good")
kinerja.boosting



#evaluasi
evaluasi<-matrix(NA,4,3)
row.names(evaluasi)<-c("tree","bagging","boosting","rf")
colnames(evaluasi)<-c("akurasi","sensitivitas","spesifisitas")

library(caret)
kinerja<-caret::confusionMatrix(pred.tree,test$Class,positive = "good")
evaluasi[1,1] <- kinerja$overall[1]
evaluasi[1,2] <- kinerja$byClass[1]
evaluasi[1,3] <- kinerja$byClass[2]

kinerja<-caret::confusionMatrix(pred.bag,test$Class,positive = "good")
evaluasi[2,1] <- kinerja$overall[1]
evaluasi[2,2] <- kinerja$byClass[1]
evaluasi[2,3] <- kinerja$byClass[2]

kinerja<-caret::confusionMatrix(pred.boost,test$Class,positive = "good")
evaluasi[3,1] <- kinerja$overall[1]
evaluasi[3,2] <- kinerja$byClass[1]
evaluasi[3,3] <- kinerja$byClass[2]

kinerja<-caret::confusionMatrix(pred.rf,test$Class,positive = "good")
evaluasi[4,1] <- kinerja$overall[1]
evaluasi[4,2] <- kinerja$byClass[1]
evaluasi[4,3] <- kinerja$byClass[2]

evaluasi