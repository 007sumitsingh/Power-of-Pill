#importing data from csv file
data <- read.csv("/home/sunny/Downloads/usa_00001.csv", head = TRUE)
#Filtering the dataset for the women who were born in between 1935 to 1957
library(dplyr)
library(dummies)
data<- data%>%filter(BIRTHYR>=1935 & BIRTHYR<=1957) 
#Loading the Laws dataset
data2<-read.csv("/home/sunny/Downloads/Laws_US.csv", head = TRUE)
#Merging the two datasets
data3<-merge(data, data2, by.x = "BPL", by.y = "STATEFIP", all.x = TRUE)
#Creating the dummy variable for marriage 
AGEMARR23<-ifelse(data3$AGEMARR<23,1,0)
#Dummy variable for pill accessibility at the age of 18
PillDummy<-ifelse(data3$BIRTHYR+18 <= data3$GKYearConsent16, 1, 0) 
#Race dummies 
race<-ifelse(data3$RACE==2, 1, 0) 
race<-factor(data3$RACE) 
#Racemat<-model.matrix(~race -1) 
#State of birth dummy variables 
birth<-factor(data3$state1) 
Statemat<-model.matrix(~birth -1)
#Year of birth dummy variables 
year<-factor(data3$BIRTHYR) 
Yearmat<-model.matrix(~year -1) 
#Abortion dummy, was abortion legal in the state when the person was 18? 
AbortionDummy<- ifelse(data3$BIRTHYR+18 <=data3$Abortion ,1,0)


#Applying the regression models
y<-AGEMARR23
X<-cbind(PillDummy, race, Statemat[,-ncol(Statemat)], Yearmat[,-ncol(Yearmat)]) 
#Here -(last column)  is done to remove the last column of the matrix to avoid the multicollinearity
#X<-cbind(PillDum, Racemat[,-ncol(Racemat)], Statemat[,-ncol(Statemat)], Yearmat[,-ncol(Yearmat)]) 
model1<- lm(y~X)
summary(model1) 

#Second Regression 
model2<-lm(y~AbortionDummy+X) 
summary(model2)


