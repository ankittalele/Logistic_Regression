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
###################################################################3