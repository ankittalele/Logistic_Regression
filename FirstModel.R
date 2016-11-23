library(readxl)
path <- "C:\\Users\\ankit.talele\\Desktop\\Elvasier\\New Data"
setwd(path)
Data <- read_excel("Export_Final_File.xls")
names(Data)
length(table(Data[18]))
colnames(Data)
unique(Data[14])

#Converting Charater to Factor variables

Data$New_Target_Var <- 0
Data$New_Target_Var[Data$Target_Var == "3"]<-0
Data$New_Target_Var[Data$Target_Var == "1"]<-1
Data$New_Target_Var[Data$Target_Var == "2"]<-2



Data_0to6Stage<- subset(Data, Target_Var == "2")
Data_W_L_Stage <- subset(Data, New_Target_Var != "2")
dim(Data)
dim(Data_0to6Stage)
dim(Data_W_L_Stage)
Data_W_L_Stage$New_Target_Var = as.factor(Data_W_L_Stage$New_Target_Var)
levels(Data_W_L_Stage$New_Target_Var)
names(Data)
names(Data_W_L_Stage)
Data_W_L_Stage[,-c(3,6,10,12,13,18,19)]=lapply(Data_W_L_Stage[,-c(3,6,10,12,13,18,19)],FUN = function(x) as.factor(x))
colnames(DataVariable)
head(DataVariable)
names(Data_W_L_Stage)

sapply(Data_W_L_Stage, function(x) sum(is.na(x)))

DataVariable <- Data_W_L_Stage[,-c(3,6,8,10,13,20)]
names(DataVariable)
DataVariable <- DataVariable[,-13]
sapply(DataVariable, function(x) sum(is.na(x)))
table(DataVariable$New_Target_Var)
table(Data_W_L_Stage$New_Target_Var)
#####Removing all Na

Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}

for (var in 1:ncol(DataVariable)) {
  if (class(DataVariable[,var])=="numeric") {
    DataVariable[is.na(DataVariable[,var]),var] <- mean(DataVariable[,var], na.rm = TRUE)
  } else if (class(DataVariable[,var]) %in% c("character", "factor")) {
    DataVariable[is.na(DataVariable[,var]),var] <- Mode(DataVariable[,var], na.rm = TRUE)
  }
}

#dim(DataVariable)
## 75% of the sample size
#smp_size <- floor(0.75 * nrow(DataVariable))
## set the seed to make your partition reproductible
#set.seed(123)
#train_ind <- sample(seq_len(nrow(DataVariable)), size = smp_size, replace = FALSE)
#train <- DataVariable[train_ind, ]
#test <- DataVariable[-train_ind, ]
write.csv(DataVariable,"DataVariable.csv")
#################################################Startfied Sampling

names(DataVariable)[2]='Acc_Country'
names(DataVariable)[6]='Oppty_Sales_Rep_Mgr'
names(DataVariable)[2]='Acc_Country'

set.seed(123)
split=0.70
library("caret")
trainIndex <- createDataPartition(DataVariable$New_Target_Var, p=split, list=FALSE)
data_train <- DataVariable[ trainIndex,]
data_test <- DataVariable[-trainIndex,]

dim(data_train)
dim(data_test)
#which(data_test$`Oppty Offering` == 'E-Select')
#data_test = data_test[-6211,]
sapply(data_train, function(x) length(levels(x)))
sapply(data_test, function(x) length(levels(x)))
colnames(data_test)
model <- glm(New_Target_Var ~.,family=binomial(link='logit'),data=data_train)
predict <- predict(model, type = 'response')

#################################################
table(data_train$New_Target_Var)

class(data_train$`Acc Country`)
class(data_test$`Acc Country`)

levels(data_train$`Acc Country`)
levels(data_test$`Acc Country`)

model$xlevels[['Acc_Country']]=levels(data_train$'Acc_Country')
model$xlevels[['Oppty_Sales_Rep_Mgr']]=levels(data_train$'Oppty_Sales_Rep_Mgr')
model$xlevels[['Acc_Country']]=levels(data_train$'Acc_Country')
'Oppty Sales Rep Mgr'
'Acc_Country'
#################################


which(data_test$`Acc Country` == 'Réunion')
#data_test = data_test[-c(325),]
which(data_test$`Oppty Sales Rep Mgr` == 'Mizuno, Atsuko')
#data_test = data_test[-c(433),]

which(data_test$`Oppty Product Type` == 'MDC Global (HS)')
#data_test = data_test[-c(298),]
#table(data_test$Target_Var)
#table(data_train$Target_Var)

#################################################
fitted.results <- predict(model,newdata=data_test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
data
write.csv(fitted.results,"Fdata_test_full.csv")
write.csv(cbind(data_test,fitted.results),"Consolidated.csv")
table(data_train$Target_Var)
table(data_test$Target_Var)
levels(data_train$`Acc Secondary Sector`)

####################################################################

str(train)
#logistic regression model
##model <- glm (Target_Var ~ ., data = train, family = binomial)
model <- glm(Target_Var ~.,family=binomial(link='logit'),data=train)
#model$`Sales Division`[["AG APAC Agents"]] <- union(model$`Sales Division`[["AG APAC Agents"]], levels(train$`Sales Division`))
#sink('out.txt')
summary(model)
#sink()
predict <- predict(model, type = 'response')

length(test)
anova(model,test = "Chisq")

test$`Acc Secondary Sector`

str(test)
fitted.results <- predict(model,newdata=subset(test,select=c(1:13)),type='response')
fitted.results <- ifelse(fitted.results < 0.26,0,ifelse((fitted.results>0.25 & fitted.results<0.51),1,ifelse((fitted.results>0.50 & fitted.results<0.76),2,3)))
#fitted.results <- ifelse(fitted.results < 0.26,0,ifelse(findInterval(fitted.results, c(0.25,0.51)),1,ifelse(findInterval(fitted.results, (0.50 & fitted.results<0.76),2,3)))
#indInterval(-score, -c(7,5))

table(fitted.results)
misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))

