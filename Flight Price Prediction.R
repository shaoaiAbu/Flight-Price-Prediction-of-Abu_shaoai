#Load the packages needed or install them if you have not done it.
if(!require(readr)) install.packages("readr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(stringr)) install.packages("stringr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(dslabs)) install.packages("dslabs")
if(!require(data.table)) install.packages("data.table")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(magrittr)) install.packages("magrittr")
if(!require(caret)) install.packages("caret")
if(!require(openxlsx)) install.packages("openxlsx")
if(!require(randomForest)) install.packages("randomForest")
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(dslabs)
library(data.table) # for fread if required
library(ggrepel) # for some plots it modifies the position of labels 
library(ggthemes) # cool ggplot themes, check it out
library(tidyr)
library(tidyverse)# The pipe sign %>% is in this package
library(magrittr) #The pipe sign %<>% and %$% are in this package
library(lubridate)#The function "as_datetime"
library(caret)#The function "createDataPartition.
library(openxlsx) #The function read.xlsx
library(randomForest) 
#The xlsx file can be downloaded from the github repository.
#First we load the data file into R.
dl<-("./Data_Train.xlsx") #This is the relative path for R to find the file.
#Make sure you have put the xlsx file in your current working directory.
#Use "getwd()"to get your current working directory.
Raw<-read.xlsx(dl)
#To see the first several rows
head(Raw)
#The Airline 
#To see the general information of the dataset.
summary(Raw)
#The target is to make a model for predict the price.
#Then we make the dataset cleaner and transform the format for some preditors like Date_of_Journey
Data<-Raw%>%mutate(Airline=as.factor(Airline),Date=dmy(Date_of_Journey),
               Source=as.factor(Source),Destination=as.factor(Destination),
               Total_Stops=sapply(Raw$Total_Stops,function(TS) ifelse(TS=="non-stop",0,
              ifelse(TS=="1 stop",1,ifelse(TS=="2 stops",2,ifelse(TS=="3 stops",3,4))))))%>%
select(Airline,Date,Source,Destination,Duration,Total_Stops,Price)

#Then we convert the factor into numeric.

Data%<>%
mutate(Airline=sapply(Data$Airline, function(Airline) which(Airline==levels(Data$Airline))))%>%
mutate(Source=sapply(Data$Source,function(Sc) which(Sc==levels(Data$Source))))%>%
mutate(Destination=sapply(Data$Destination,function(Ds) which(Ds==levels(Data$Destination))))%>%
mutate(Duration_hour=as.numeric(str_extract(str_extract(Duration,"^\\d+[h]"),"\\d+"))+
ifelse(nchar(Duration)<=3,0,as.numeric(str_extract(str_extract(Duration,"\\d+[m]$"),"\\d+"))/60))%>%
select(-Duration)

#To check the basic information of the dataset 
summary(Data)
#We see one NA in Duration and Total_Stops
na1<-which(is.na(Data$Duration))
na2<-which(is.na(Data$Total_Stops))
#Check the Raw dataset
Raw[na1,]
Raw[na2,]
#The time for a flight is impossible to be 5 minutes so we will remove these two rows
index_delete<-c(na1,na2)
Data_cor<-Data%>%slice(-index_delete)

#Now we have finished the data cleaning.Then we need to split the dataset into training set
#and Testing set
set.seed(1,sample.kind = "Rounding")
test_index<-createDataPartition(Data_cor$Price,times = 1,p = 0.3,list = FALSE)
Train_set<-Data_cor%>%slice(-test_index)
Test_set<-Data_cor%>%slice(test_index)
#make some visualization to search for relation 
#between price and other predictors.
#The Airline and Price
Train_set%>%boxplot(Price~Airline,data = .)
#The Date and Price
Train_set%>%boxplot(Price~Date,data = .)
#The Source and Price
Train_set%>%boxplot(Price~Source,data = .)
#The destination and Price
Train_set%>%boxplot(Price~Destination,data = .)
#The Total_Stops and Price
Train_set%>%boxplot(Price~Total_Stops,data = .)
#The Duration_hour and Price
Train_set%>%ggplot(aes(Duration_hour,Price))+geom_point()+geom_smooth()

model_knn<-train(Price~.,method="knn",data =Data_cor,TuneGrid=data.frame(k=seq(1,20,1)))
pred_knn<-predict(model_knn,Test_set)
sqrt(mean((pred_knn-Test_set$Price)^2))

model_glm<-train(Price~.,method="glm",data = Data_cor)
pred_glm<-predict(model_glm,Test_set)
sqrt(mean((pred_glm-Test_set$Price)^2))

set.seed(123,sample.kind = "Rounding")
model_rf<-randomForest(Price ~ ., data = Train_set, importance = TRUE)
pred_rf<-predict(model_rf,Test_set)
sqrt(mean((pred_rf-Test_set$Price)^2))
sd(Test_set$Price)
#Now we will ensemble these models
#We simply compute the average of every model because we can not use the result generated 
#from the testing set.
pred_ensemble<-(pred_knn+pred_glm+pred_rf)/3
sqrt(mean((pred_ensemble-Test_set$Price)^2))


