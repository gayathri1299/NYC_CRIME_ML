#read the data 

library(ggplot2)
library(dplyr)
library(tidyverse) 
library(lubridate)
library(plotly)
library(knitr)
library(naivebayes)
library(psych)
library(klaR)
library(caret)
library(e1071)
library(plotROC)
library(pROC)
library(rpart)
library(purrr)
library(tidyr)
library(ROCR)
library(class)
library(MLmetrics)
library(Metrics)
library(multiROC)
library(xgboost)
library(h2o)

data <- read.csv("NYC_complaints.csv")
head(data)

#printing dimension of the dataset

glimpse(data)


#dropping columns that are not in use
df <- data[ -c(2,6,7,9,10,11,13,15,17,18,19,20,23,27,31,32,36) ]
head(df)

#renaming the columns for better understanding
names(df) <- c("ID","Borough","Date","Time","Crime Status","Jurisdiction","Level of offense", "Offense", "Premise" , "Report Date", " Suspect age", "Suspect race","Suspect sex", "Victim age", " Victim race", " Victim sex","Latitude", "Longtitude", "Cordinates")
head(df)

#printing dimension of the new dataset
glimpse(df)

#formatting date
df$Date <- as.Date(as.character(df$Date), format = "%m/%d/%y")
df$date2 <- df$Date
df <- separate(df, col = date2, into = c("year","month","day"), sep ="-")

# replacing empty cells with "NOT REPORTED" as information is unknown to assign value
df <- df%>%mutate_if(is.character, list(~na_if(.,"")))
df[is.na(df)]<- "NOT REPORTED"
head(df)

# crime in each borough
cf <- ggplot(df, aes(x = Borough, fill=as.factor(Borough))) + geom_bar(width=0.9, stat="count") + theme(legend.position="none") + coord_flip()
print(cf)



#count of crimes per month and per day

mf <- ggplot(df, aes(x = month, fill=as.factor(month))) + geom_bar(width=0.9, stat="count") + theme(legend.position= "none")

dff <- ggplot(df, aes(x = day, fill=as.factor(day))) + geom_bar(width=0.9, stat="count") + theme(legend.position= "none")

grid.arrange(dff, mf)

#count of crimes per month by borough
mb <- ggplot(df, aes(x = month, fill=as.factor(Borough))) + geom_bar(width=0.9, stat="count") + theme(legend.position= "right")

db <- ggplot(df, aes(x = day, fill=as.factor(Borough))) + geom_bar(width=0.9, stat="count") + theme(legend.position= "right")

grid.arrange(db, mb)


# complaints per borough

cnt <-pie(table(df$`Borough`))
print(cnt)

#type of crime category

lc <- pie(table(df$`Level of offense`))
print(lc)

# departments solving crime per month
pd<-ggplot(data=df,aes(x=`month`,fill=`Jurisdiction`))+geom_histogram(stat="count")+ scale_fill_discrete(name="DEPARTMENT")
print(pd)

# count of each crime
oc <- ggplot(df, aes(x=`Offense`))+geom_histogram(stat ="count")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
print(oc)

# Number of victims in each age group
ag <- table(df$`Victim age`)
# We can see that a  14 age group values do not align with reality, but this is a very small inconsistency comapred to the whole data. 
# But what is concering is the 116573 unknown values(almost 1/3 of the data), this will affect the accuracy. 

# Age group most likely to be a victim
ag[ag==max(ag)]

# converting time column from character to time stamp
df$Time <- as.POSIXct(df$Time, format = "%H:%M:%S")
df$Time <- format(df$Time, "%H:%M:%S")

# crimes happening between 12am to 6am and the count
df1 <- df %>% filter(Time < "06:00:00" & Time > "00:00:00")
print(df1)
print(nrow(df1))

# percentage
print(nrow(df1)*100/nrow(df))

# Murders happening between 12 am to 6am 
df2 <- df1 %>% filter(df1$`Offense` == "MURDER & NON-NEGL. MANSLAUGHTER")
head(df2)

print(nrow(df2))

# Murders happening by age groups between 12 am to 6am
mg<-ggplot(df2, aes(x =`Victim age`, fill=as.factor(`Victim age`))) + geom_bar(width=0.9, stat="count") + theme(legend.position="none") + coord_flip()
print(mg)

# scatterplot of the above crime's cordinates:
x<- df2$Latitude
y<- df2$Longtitude
sp<-plot( x,y, main = "scatter plot", xlab = "latitude", ylab = "longitude", pch = 19)
print(sp)

# street crimes
df3 <- df %>% filter(df$`Premise` == "STREET")
print(df3)
print(nrow(df3))

# percent of street crime
print(nrow(df3)*100/nrow(df))

# histogram of street crimes by borough
sb<-ggplot(df3, aes(x = `Borough`, fill=as.factor(`Borough`))) + geom_bar(width=0.9, stat="count") + theme(legend.position="none") + coord_flip()
print(sb)

#street crimes indicating the race of the victim 
sr<- ggplot(data=df3,aes(x=`Borough`,fill=` Victim race`))+geom_histogram(stat="count")+ scale_fill_discrete(name="RACE")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
print(sr)

# Homework 2:

# Classification:

# Removing Values which are entered as NOT REPORTED

df$PD_Code <- data$PD_CD 
df_c <- subset(df, Borough!= "NOT REPORTED" & Latitude !="NOT REPORTED" & Longtitude != "NOT REPORTED")
print(nrow(df_c))

# Now the dataframe has 448,355 rows, initially it was  449,506

# Checks for any null values in each columns and returns the sum
colSums(is.na(df_c))

# Assigning class values to each borough as 1,2,3,4,5 for classification
df_c$class1 <- as.factor(df_c$Borough)
df_c$class <- df_c$Borough

data_c <- dplyr::select(df_c, c("Borough","Latitude","Longtitude","PD_Code", "class"))
data_nb <-dplyr::select(df_c, c("Borough","Latitude","Longtitude","PD_Code", "class1"))

data_split <- createDataPartition(y = data_c$class, p = 0.75, list = FALSE)
training <- data_c[data_split,]
testing <- data_c[-data_split, ]
prop.table(table(data_c$class)) * 100
prop.table(table(testing$class)) * 100
prop.table(table(training$class)) * 100

data_split_nb <- createDataPartition(y = data_nb$class1, p = 0.75, list = FALSE)
training_nb <- data_nb[data_split_nb,]
testing_nb <- data_nb[-data_split_nb, ]
prop.table(table(data_nb$class1)) * 100
prop.table(table(testing_nb$class1)) * 100
prop.table(table(training_nb$class1)) * 100

x <- training[,1:4]
y <- training$class

x_nb <- training_nb[,1:4]
y_nb <- training_nb$class1


train_subset <- training_nb[1:10000,] %>% drop_na()
test_subset <- testing_nb[1:10000,] %>% drop_na()
y1 <- train_subset$class1



y1 <- factor(y1, labels = make.names(levels(y1)))
train_subset$class1 <-y1
levels(train_subset$class1) <- c("x1", "x2",'x3','x4','x5')
levels(training_nb$class1) <- c("x1", "x2",'x3','x4','x5')



nb.m1 <- train( x = x_nb,y = y_nb,method = "nb", trControl = trainControl(method = "cv", number = 3))

predi<- data.frame(matrix(ncol = 1, nrow = 112087))

predi$class1 <- predict(nb_cv, newdata = testing_nb)
pred = subset(predi, select = -c(1))
CM<-confusionMatrix(pred$class1, testing_nb$class1)
roc.nb <- multiclass.roc(as.numeric(testing_nb$class1), as.numeric(pred$class1))
auc(roc.nb)
f1_nb <- F1_Score(pred$class1, testing_nb$class1)
f1_nb
bias(as.numeric(testing_nb$class1), as.numeric(pred$class1))
var(as.numeric(pred$class1))
CM

# Support Vector Machine

svmm <- train(class1~. ,data = train_subset , method = 'svmLinear')
y_pred = predict(svmm, newdata = test_subset)
y_pre<- data.frame(matrix(ncol = 1, nrow = 10000))
y_pre$class <- y_pred
cm = table(test_subset$class1, y_pred)
cm
f1_svm <- F1_Score(test_subset$class1, y_pred)
f1_svm
bias(as.numeric(test_subset$class1), as.numeric(y_pred))
var(as.numeric(y_pred))



# decision tree

dtt<- train(class1~. ,data = train_subset , method = "rpart")
y_pred = predict(dtt, newdata = test_subset)
y_pre<- data.frame(matrix(ncol = 1, nrow = 10000))
y_pre$class <- y_pred
cm = table(test_subset$class1, y_pred)
cm
f1_svm <- F1_Score(test_subset$class1, y_pred)
f1_svm
bias(as.numeric(test_subset$class1), as.numeric(y_pred))
var(as.numeric(y_pred))

table_mat=table(test_subset$class1, pred_dt)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

cm_dt <- confusionMatrix(as.factor(test_subset$class1), as.factor(pred_dt), mode ="prec_recall")

f1_dt <- F1_Score(test_subset$class1, pred_dt)

cm_dt
f1_dt
bias(as.numeric(testing_nb$class1), as.numeric(pred_dt))
var(as.numeric(pred_dt))


# KNN 




set.seed(340)
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3, classProbs = TRUE)

knnf <- train(class1~., data = train_subset, method = "knn", trControl = trainControl(method = "cv", number = 3), preProcess = c("center","scale"), tuneLength = 20, na.action="na.omit")
pred <- predict(knnf, newdata = test_subset)
dataRev <- table(actualclass=test_subset$class1, predictedclass= pred)
accuracy<- sum(diag(dataRev)) / sum(dataRev)
cm <- ConfusionMatrix(as.factor(test_subset$class1), as.factor(pred))
f1<- F1_Score(as.numeric(pred),as.numeric(test_subset$class1))
accuracy
cm
f1
var(as.numeric(pred))
bias(as.numeric(test_subset$class1), as.numeric(pred))


# ensemble techniques:

# cross validation:
train_control <- trainControl(method = "cv", number = 10)
ctrl <- trainControl(method = "cv", savePred=T, classProb=T)

# NB + CV
nb_cv <- train( x = x_nb,y = y_nb,method = "nb",trControl = train_control)
predi$class1 <- predict(nb_cv, newdata = testing_nb)
pred = subset(predi, select = -c(1))
CM<-confusionMatrix(pred$class1, testing_nb$class1)
roc.nb <- multiclass.roc(as.numeric(testing_nb$class1), as.numeric(pred$class1))
auc(roc.nb)
f1_nb <- F1_Score(pred$class1, testing_nb$class1)
f1_nb
bias(as.numeric(testing_nb$class1), as.numeric(pred$class1))
var(as.numeric(pred$class1))
CM

# SVM + CV
svm_cv<-train(class1~. ,data = train_subset , method = 'svmLinear',trControl = ctrl)
pred <- predict(svm_cv, test_subset)
dataRev <- table(actualclass=test_subset$class1, predictedclass= pred)
accuracy<- sum(diag(dataRev)) / sum(dataRev)
cm <- ConfusionMatrix(as.factor(test_subset$class1), as.factor(pred))
f1<- F1_Score(as.numeric(pred),as.numeric(test_subset$class1))
accuracy
cm
f1
var(as.numeric(pred))
bias(as.numeric(test_subset$class1), as.numeric(pred))

# KNN+CV
knn_cv <- train(class1~., data = train_subset, method = "knn", trControl = train_control, preProcess = c("center","scale"), tuneLength = 20, na.action="na.omit")
pred <- predict(knn_cv, test_subset)
dataRev <- table(actualclass=test_subset$class1, predictedclass= pred)
accuracy<- sum(diag(dataRev)) / sum(dataRev)
cm <- ConfusionMatrix(as.factor(test_subset$class1), as.factor(pred))
f1<- F1_Score(as.numeric(pred),as.numeric(test_subset$class1))
accuracy
cm
f1
var(as.numeric(pred))
bias(as.numeric(test_subset$class1), as.numeric(pred))

# decision tree + cross validation
dtt_cv <- train(class1~. ,data = train_subset , method = "rpart", trControl = train_control)
pred <- predict(dtt_cv, test_subset)
dataRev <- table(actualclass=test_subset$class1, predictedclass= pred)
accuracy<- sum(diag(dataRev)) / sum(dataRev)
cm <- ConfusionMatrix(as.factor(test_subset$class1), as.factor(pred))
f1<- F1_Score(as.numeric(pred),as.numeric(test_subset$class1))
accuracy
cm
f1
var(as.numeric(pred))
bias(as.numeric(test_subset$class1), as.numeric(pred))


#stacking ensemble technique

h2o.init()
# create the train and test h2o data frames
train_df_h2o<-as.h2o(train_subset)
test_df_h2o<-as.h2o(test_subset)
# Identify predictors and response
y <- "class1"
x <- setdiff(names(train_df_h2o), y)

nfolds <- 5

my_gbm <- h2o.gbm(x = x,
                  y = y,
                  training_frame = train_df_h2o,
                  nfolds = nfolds,
                  keep_cross_validation_predictions = TRUE,
                  seed = 5)
# Train & Cross-validate a RF
my_rf <- h2o.randomForest(x = x,
                          y = y,
                          training_frame = train_df_h2o,
                          nfolds = nfolds,
                          keep_cross_validation_predictions = TRUE,
                          seed = 5)
# Train & Cross-validate a LR
my_lr <- h2o.glm(x = x,
                 y = y,
                 training_frame = train_df_h2o,
                 family = c("binomial"),
                 nfolds = nfolds,
                 keep_cross_validation_predictions = TRUE,
                 seed = 5)

ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                metalearner_algorithm="drf",
                                training_frame = train_df_h2o,
                                base_models = list(my_gbm, my_rf, my_lr))

# random forest 
rf <- randomForest(y_nb~. ,data = x_nb, ntree = 250,trControl = train_control)

pred_rf <-predict(nb.m1, testing_nb, type = 'class')
table_mat=table(testing_nb$class1, pred_rf)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

cm_dt <- confusionMatrix(as.factor(testing_nb$class1), as.factor(pred_rf), mode ="prec_recall")
cm_dt

f1_rf <- F1_Score(as.factor(testing_nb$class1), as.factor(pred_rf))
f1_rf

bias(as.numeric(testing_nb$class1), as.numeric(pred_rf))
var(as.numeric(pred_rf))

# xgboosting
gbmm <- train(class1 ~., data = train_subset, method = "xgbTree",trControl = train_control)

gbmm <- train(y1~ ., data = train_subset, method = "gbm", trControl = train_control,verbose = FALSE)
pred <- predict(gbmm, test_subset)
dataRev <- table(actualclass=test_subset$class1, predictedclass= pred)
accuracy<- sum(diag(dataRev)) / sum(dataRev)
cm <- ConfusionMatrix(as.factor(test_subset$class1), as.factor(pred))
f1<- F1_Score(as.numeric(pred),as.numeric(test_subset$class1))

accuracy
cm
f1
var(as.numeric(pred))
bias(as.numeric(test_subset$class1), as.numeric(pred))

