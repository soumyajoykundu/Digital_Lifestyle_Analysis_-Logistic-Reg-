install.packages("xlsx")
library("xlsx")
print(getwd())
setwd("C:\\users\\Soumyajoy\\Desktop")
data <- read.csv("C:/Users/Soumyajoy/OneDrive/Documents/Project/abc.csv")
head(data)
View(data)

conTable=table(data$Income,data$OS_Binary,data$Gender,data$Location)
print(conTable)

 
t<-table(data$Gender,data$OS_Avail,data$Income,data$OS_Binary)
ftable(t)

t<-table(data$X.OS_App_Pref_1,data$OS_Item_Pref_1)
ftable(t)

t<-table(data$Location,data$Gender,data$OS_Binary)
ftable(t)

#Logistic Model Validation 

##      ONLINE SHOPPING
### Age
T3.1=matrix(data=c(144,121,15,16,24,19,32,20,87,40),byrow=T,nrow=5,ncol=2)
T3.1
chisq.test(T3.1)

### Gender
T3.2=matrix(data=c(180,122,122,94),byrow=T,nrow=2,ncol=2)
T3.2
chisq.test(T3.2)

### Profession
T3.3=matrix(data=c(221,145,46,36,14,13,13,14,8,8),byrow=T,nrow=5,ncol=2)
T3.3
chisq.test(T3.3)

### Income
T3.4=matrix(data=c(63,60,78,41,88,80,73,35),byrow=T,nrow=4,ncol=2)
T3.4
chisq.test(T3.4)

### Avail
T3.5=matrix(data=c(127,19,138,76,37,121),byrow=T,nrow=3,ncol=2)
T3.5
chisq.test(T3.5)

#Other works
##    ONLINE SHOPPING 
# Values are manually inputed from Excel Pivot Tables
install.packages("lsr")
library("lsr")
?matrix
T1=matrix(data=c(287,137,51,116,222,86,68,81,156,26,57,204),byrow=T,nrow=4,ncol=3)
T1
chisq.test(T1)
fisher.test(T1)
cramersV(T1)


T2=matrix(data=c(73,46,5,109,91,12,72,78,8),byrow=T,nrow=3,ncol=3)
T2
chisq.test(T2)
fisher.test(T2)
cramersV(T2)
install.packages("MESS")
library("MESS")
gkgamma(T2)

?gkgamma
?cramersV
