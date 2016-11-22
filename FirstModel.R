

library(readxl)
path <- "C:\\Users\\ankit.talele\\Desktop\\New Data"
setwd(path)
Data <- read_excel("Export_Final_File.xls")
str(Data)
Data <- Data[c(1:17,19,20,18)]

length(table(Data[18]))
colnames(Data)
unique(Data[14])

#Converting Charater to Factor variables

Data[,-c(2,3,4,6,8,9,10,12,13,14,18,19)]=lapply(Data[,-c(2,3,4,6,8,9,10,12,13,14,18,19)],FUN = function(x) as.factor(x))
str(Data)

sapply(Data, function(x) sum(is.na(x)))

unique(Data$Target_Var)
colnames(DataVariable)
sapply(DataVariable, function(x) sum(is.na(x)))


DataVariable <- Data[,-c(2,3,6,8,10,13,19)]
colnames(DataVariable)


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

dim(DataVariable)
## 75% of the sample size
smp_size <- floor(0.75 * nrow(DataVariable))

## set the seed to make your partition reproductible
set.seed(123)

train_ind <- sample(seq_len(nrow(DataVariable)), size = smp_size)
train <- DataVariable[train_ind, ]
test <- DataVariable[-train_ind, ]
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
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))

