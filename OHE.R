library(readxl)
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
set.seed(999)
path <- "C:\\Users\\ankit.talele\\Desktop\\Elsavier Git Repo"
setwd(path)
Data <- read.csv("DataVariable.csv")
names(Data)
Data$Decision.Maker <- as.factor(Data$Decision.Maker)
Data$New_Target_Var <- as.factor(Data$New_Target_Var)


######################## OHE ###############
ohe_fests <- c('Sales.Division','Acc.Country','Acc.Secondary.Sector','Acc.Type','Oppty.Offering','Oppty.Sales.Rep.Mgr', 
               'Resp.Type','Oppty.Product.Type','role','industry')

dummies <- dummyVars(~ Sales.Division + Acc.Country + Acc.Secondary.Sector + Acc.Type + Oppty.Offering + Oppty.Sales.Rep.Mgr
                     + Resp.Type + Oppty.Product.Type + role + industry, data = Data)

df_all_ohe <- as.data.frame(predict(dummies, newdata = Data))
dim(df_all_ohe)
df_all_combined <- cbind(Data[,-c(which(colnames(Data) %in% ohe_fests))],df_all_ohe)
sink('ot.txt')
str(df_all_combined)
sink()
########################### 70:30 on data for train and test

split=0.70
trainIndex <- createDataPartition(df_all_combined$New_Target_Var, p=split, list=FALSE)
data_train <- df_all_combined[ trainIndex,]
data_test <- df_all_combined[-trainIndex,]

################################################################################################
model <- glm(New_Target_Var ~.,family=binomial(link='logit'),data=data_train)
predict <- plogis(predict(model, type = 'response'))

fitted.results <- predict(model,newdata=data_test,type='response')
library(InformationValue)
optCutOff <- optimalCutoff(data_test$New_Target_Var, predict)[1] 

fitted.results <- ifelse(fitted.results > 0.5,1,0)
write.csv(fitted.results,"Fdata_test_fu.csv")
write.csv(data_test,"data_test.csv")

sink("Summary_model.txt")
summary(model)
sink()
vif(model)
alias(model)
as <- cor(data_test)
head(data_test)
test1<-apply(data_test,2,as.numeric)
cortest=cor(test1)
###########################################################################################################
###########
#install.packages("pROC")
library(pROC)
g <- roc(New_Target_Var ~ predict, data = data_train)
plot(g)
auc(g, min = 0, max = 1)


Auc <- auc(predict$, predictions$pred)
plot_pred_type_distribution(predict, 0.7)


