install.packages("readxl")
install.packages("caTools")
install.packages("ROCR")
library("readxl")
library("caTools")
library("ROCR")
mydata = read_excel("C:/Users/Soumyajoy/OneDrive/Desktop/Lifestyle.xlsx")
mydata
View(mydata)

#Logistic on whole Data
LR_OS=glm(mydata$C_Opinion ~ mydata$Income+mydata$C_Avail+mydata$Age+mydata$Gender+mydata$Profession,data=mydata,family="binomial")
LR_OS
summary(LR_OS)

# Splitting into trained and test data

set.seed(1234)
data1<-sample(2, nrow(mydata), 
              replace = T, 
              prob = c(0.75, 0.25))
train<-mydata[data1 == 1,]
test<-mydata[data1 == 2,]

# Logistic Model for trained data
LR_OS_Train=glm(C_Opinion ~ Income+C_Avail+Age+Gender+Profession,data=train,family="binomial")
LR_OS_Train
summary(LR_OS_Train)

# Note that in the above model, as Gender, Profession and Income has 
# all its coefficient = 0 (Rejected), thus we eliminate the variables

LR_OS_Train=glm(C_Opinion ~ Income+C_Avail,data=train,family="binomial")
LR_OS_Train
summary(LR_OS_Train)

# Predict test data based on model
predict_reg <- predict(LR_OS_Train,test, type = "response")
head(predict_reg)
?predict

# Changing probabilities
pre1 <- ifelse(predict_reg >0.5, 1, 0)
pre1
# Evaluating model accuracy
# using confusion matrix
table(Prediction=pre1,Actual=test$OS_Opinion)

missing_classerr <- mean(pre1 != test$OS_Opinion)
print(paste('Accuracy =', 1 - missing_classerr))

# ROC-AUC Curve
ROCPred <- prediction(test$OS_Opinion, pre1) 
ROCPer <- performance(ROCPred, measure = "tpr", 
                      x.measure = "fpr")

auc <- performance(ROCPred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Plotting curve
plot(ROCPer)
plot(ROCPer, colorize = TRUE, 
     print.cutoffs.at = seq(0.1, by = 0.1), 
     main = "ROC CURVE")
abline(a = 0, b = 1)

auc <- round(auc, 4)
legend(.6, .4, auc, title = "AUC", cex = 1)
