options(warn=-1)
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(data.table)
library(pROC)

set.seed(999)
path <- "C:\\Users\\ankit.talele\\Desktop\\Elsavier Git Repo"
setwd(path)
Data <- read.csv("Data_1_0_Mail_Split.csv")
Data$Decision.Maker <- as.factor(Data$Decision.Maker)
Data$Target_Var <- as.factor(Data$Target_Var)
Data$Split_Mail_Flag <- as.factor(Data$Split_Mail_Flag)
Data <- Data[,-c(2,3,6,8,9,10,13,14,16)]
names(Data)
table(Data$Target_Var)

#8606--Total
#7229--0
#877---1

#7729/6
#1205---0 && 877---1

ohe_fests <- c('Sales.Division','Acc.Secondary.Sector','Acc.Type','Oppty.Offering', 'Resp.Type')
dummies <- dummyVars(~ Sales.Division + Acc.Secondary.Sector + Acc.Type + Oppty.Offering + Resp.Type, data = Data)
df_all_ohe <- as.data.frame(predict(dummies, newdata = Data))
df_all_combined <- cbind(Data[,-c(which(colnames(Data) %in% ohe_fests))],df_all_ohe)
names(df_all_combined)

#split=0.70
split=0.70
trainIndex <- createDataPartition(df_all_combined$Target_Var, p=split, list=FALSE)
data_train <- df_all_combined[ trainIndex,]
data_test <- df_all_combined[-trainIndex,]
Original<- data_test$Target_Var
table(data_train$Target_Var)
table(data_test$Target_Var)
data_test <- data_test[,-4]


target_Ones <- subset(data_train, Target_Var == '1')
target_Zeros <- subset(data_train, Target_Var == '0')
splt <- split(target_Zeros, sample(1:6, nrow(target_Zeros), replace=T))
#dim(asd)
dim(target_Ones)
dim(target_Zeros)
class(splt)
#asd <- as.data.frame(splt[1])
for (i in 1:6)
{
  #i=1
  #paste("target_Zero_",i) <- as.data.frame(splt[i]) 
  asd<- as.data.frame(splt[i])
  assign(paste0("target_Zeros_",i), asd)
  #assign(names(paste0("target_Zeros_",i)),substring(names(paste0("target_Zeros_",i)), 4))
  #assign((names(paste0("target_Zeros_",i))),(substring(names(paste0("target_Zeros_",i)),4))
}

names(target_Zeros_1) <- substring(names(target_Zeros_1), 4)
names(target_Zeros_2) <- substring(names(target_Zeros_2), 4)
names(target_Zeros_3) <- substring(names(target_Zeros_3), 4)
names(target_Zeros_4) <- substring(names(target_Zeros_4), 4)
names(target_Zeros_5) <- substring(names(target_Zeros_5), 4)
names(target_Zeros_6) <- substring(names(target_Zeros_6), 4)

names(target_Ones)
names(target_Zeros_1)
dim(target_Zeros_1)
dim(target_Zeros_3)
dim(target_Zeros_4)
dim(target_Zeros_5)
dim(target_Zeros_6)
names(target_Ones)

which(names(target_Ones) == names(target_Zeros_1))

names(target_Ones)<-gsub(" ",".",names(target_Ones))
names(target_Ones)<-gsub("/",".",names(target_Ones))
names(target_Ones)<-gsub(",",".",names(target_Ones))
names(target_Ones)<-gsub("&",".",names(target_Ones))
names(target_Ones)<-gsub("-",".",names(target_Ones))

MData1 <- rbind(target_Ones,target_Zeros_1)
MData2 <- rbind(target_Ones,target_Zeros_2)
MData3 <- rbind(target_Ones,target_Zeros_3)
MData4 <- rbind(target_Ones,target_Zeros_4)
MData5 <- rbind(target_Ones,target_Zeros_5)
MData6 <- rbind(target_Ones,target_Zeros_6)

y <- MData1$Target_Var 
MData1 = MData1[-grep('Target_Var', colnames(MData1))]
bst1 <- xgboost(data = data.matrix(MData1), label = y, max.depth = 15, eta = 0.1, nthread = 3, nround = 25, subsample = 0.5,
               colsample_bytree = 0.5, seed = 1, eval_metric = "merror", objective = "multi:softmax", num_class = 12)
pred1 <- predict(bst1, data.matrix(data_test))
#pred <- as.factor(pred)
pred1[pred1 == "1"] <- 0
pred1[pred1 == "2"] <- 1
y <- MData2$Target_Var 
MData2 = MData2[-grep('Target_Var', colnames(MData2))]
bst2 <- xgboost(data = data.matrix(MData2), label = y, max.depth = 15, eta = 0.1, nthread = 3, nround = 25, subsample = 0.5,
                colsample_bytree = 0.5, seed = 1, eval_metric = "merror", objective = "multi:softmax", num_class = 12)
pred2 <- predict(bst2, data.matrix(data_test))
#pred <- as.factor(pred)
pred2[pred2 == "1"] <- 0
pred2[pred2 == "2"] <- 1
y <- MData3$Target_Var 
MData3 = MData3[-grep('Target_Var', colnames(MData3))]
bst3 <- xgboost(data = data.matrix(MData3), label = y, max.depth = 15, eta = 0.1, nthread = 3, nround = 25, subsample = 0.5,
                colsample_bytree = 0.5, seed = 1, eval_metric = "merror", objective = "multi:softmax", num_class = 12)
pred3 <- predict(bst3, data.matrix(data_test))
#pred <- as.factor(pred)
pred3[pred3 == "1"] <- 0
pred3[pred3 == "2"] <- 1
y <- MData4$Target_Var 
MData4 = MData4[-grep('Target_Var', colnames(MData4))]
bst4 <- xgboost(data = data.matrix(MData4), label = y, max.depth = 15, eta = 0.1, nthread = 3, nround = 25, subsample = 0.5,
                colsample_bytree = 0.5, seed = 1, eval_metric = "merror", objective = "multi:softmax", num_class = 12)
pred4 <- predict(bst4, data.matrix(data_test))
#pred <- as.factor(pred)
pred4[pred4 == "1"] <- 0
pred4[pred4 == "2"] <- 1
y <- MData5$Target_Var 
MData5 = MData5[-grep('Target_Var', colnames(MData5))]
bst5 <- xgboost(data = data.matrix(MData5), label = y, max.depth = 15, eta = 0.1, nthread = 3, nround = 25, subsample = 0.5,
                colsample_bytree = 0.5, seed = 1, eval_metric = "merror", objective = "multi:softmax", num_class = 12)
pred5 <- predict(bst5, data.matrix(data_test))
#pred <- as.factor(pred)
pred5[pred5 == "1"] <- 0
pred5[pred5 == "2"] <- 1
y <- MData6$Target_Var 
MData6 = MData6[-grep('Target_Var', colnames(MData6))]
bst6 <- xgboost(data = data.matrix(MData6), label = y, max.depth = 15, eta = 0.1, nthread = 3, nround = 25, subsample = 0.5,
                colsample_bytree = 0.5, seed = 1, eval_metric = "merror", objective = "multi:softmax", num_class = 12)
pred6 <- predict(bst6, data.matrix(data_test))
#pred <- as.factor(pred)
pred6[pred6 == "1"] <- 0
pred6[pred6 == "2"] <- 1

pred<- cbind(pred1,pred2,pred3,pred4,pred5,pred6)
head(pred)
class(pred)
pred <- as.data.frame(pred)
pred$mean <- rowMeans(pred)
pred<- cbind(pred,Original)
pred$Predicted <- 0
pred$Predicted[pred$mean > 0.49] <- 1
confusionMatrix(pred$Original,pred$Predicted)
table(Original)


library('C50')

pred$Predicted=as.factor(pred$Predicted)

data_train1=data_train

names(data_train1)<-sapply(1:105,function(x) paste0('V',x,sep = ''))

Model_c50=C5.0(V4~.,data = data_train1[,-2])

summary(Model_c50)

View(pred)
