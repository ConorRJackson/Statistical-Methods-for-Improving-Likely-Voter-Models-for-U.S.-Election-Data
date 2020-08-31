### Break Down CCES 2008 Data ###


#install.packages("Amelia")
library(Amelia)

load("D:/Dissertation/R Code/CCES/2008/cces_2008_common.RData")

dim(x)

x$result[x$vote_gen08=="validated record of voting in general election"]<-1
x$result[x$vote_gen08=="did not vote - verified record of not voting"]<-0
# vote_gen08 is the variable name for 'Vote history for 2008 general'
# set to 1 if did vote and 0 if did not vote

x<-x[-which(is.na(x$result)==TRUE),]
# removes 'result' observations with NA (only results validated in record)

x$result<-factor(x$result,levels=c(0,1))
# encodes vector 'result' as a factor with two levels 0 and 1

x<-x[c("V245","CC324_1","CC326","V207","V208","V209","V211","V213",
       "V214","V216","V242","V243","V246","CC332","CC333","CC334","CC402",
       "result")]
colnames(x)<-c("thought","before","intend","birthyear","gender",
               "employment","race","education","marital","religion","child",
               "ideology","income","citizen","own_or_rent","residence","party",
               "result") 
# set x to explanatory variables only
# rename the extracted explanatory variables

dim(x) # 27683 obs. of 18 variables
# new dimensions of data set x (original 32800 obs. of 477 variables)

x$birthyear<-2008-as.numeric(levels(x$birthyear)[as.integer(x$birthyear)])
colnames(x)[4]<-"agec"
# change birth year variable into age

x<-na.omit(x) 

dim(x) # 12515 obs. of 18 variables
# new dimensions of data set x (before 27683 obs. of 20 variables)

x<-droplevels(x,except = "agec")

x<-na.omit(x)
# delete all observations containing missing values

dim(x) 
# 12515 18

#################################################################

#################################################################

#### Actual turnout rate from the data set ####


(length(which(x$intend=="Yes, definitely"))/nrow(x))*100
# 87.38% of respondents said 'yes definitely'  when asked if they intend to vote

(length(which(x$result=="1" & x$intend=="Yes, definitely"))/nrow(x))*100
# 76.89% of respondents who said 'yes definitely' actually voted

table(x$intend)
# 2008 Voter's Intention VS Validated Turnout

#################################################################

#################################################################

#### Separate 2008 training set and test set ####


set.seed(123)

N.x <-nrow(x)

indtrain<- sample(1:N.x, replace=TRUE)
indtrain <- sort(indtrain)
# sample training set with boostrap

indtest <- setdiff(1:N.x, indtrain)

dim(x[indtrain,]) 

dim(x[indtest,]) 

#################################################################

#################################################################

#### CCES 2008 Logistic Regression ####


fit.glm<-glm(result ~ ., data=x[indtrain, ], family="binomial")
summary(fit.glm)

## tau of 0.5 

tau <- 0.5
glm.probs <- predict(fit.glm,
                     newdata = x[indtest,],
                     type = "response")
prediction.for.table.glm <- ifelse(glm.probs > tau, 1, 0)

table.glm <- table(observed=x[indtest, 18],predicted=prediction.for.table.glm)
table.glm

misclass.glm <- 1 - sum(diag(table.glm)) / sum(table.glm)
misclass.glm
#caculate misclassification rate


## optimal tau 

#install.packages("ROCR")
library(ROCR)

predObj <- prediction(glm.probs, x[indtest,18])

perf <- performance(predObj, "tpr", "fpr")

acc <- performance(predObj, "acc")
tau.optimal <- acc@x.values[[1]]
Accuracy <- acc@y.values[[1]]
best <- which.max(Accuracy)

tau.optimal[best]

prediction.for.table.glm.optimal <- ifelse(glm.probs > tau.optimal[best], 1, 0)
table.glm.optimal <- table(observed=x[indtest, 18],predicted=prediction.for.table.glm.optimal)
table.glm.optimal

misclass.glm.optimal <- 1 - sum(diag(table.glm.optimal)) / sum(table.glm.optimal)
misclass.glm.optimal

#################################################################

## insignificant variables omitted ##

x.lr.new <- x[c("before","intend","gender",
                "employment","education","marital",
                "ideology","income","residence",
                "result")]

fit.glm.lr.new<-glm(result ~ ., data=x.lr.new[indtrain, ], family="binomial")
summary(fit.glm.lr.new)


## tau of 0.5 

tau.lr.new <- 0.5
glm.probs.lr.new <- predict(fit.glm.lr.new,
                     newdata = x.lr.new[indtest,],
                     type = "response")

prediction.for.table.glm.lr.new <- ifelse(glm.probs.lr.new > tau.lr.new, 1, 0)

table.glm.lr.new <- table(observed=x.lr.new[indtest, 10],predicted=prediction.for.table.glm.lr.new)
table.glm.lr.new

misclass.glm.lr.new <- 1 - sum(diag(table.glm.lr.new)) / sum(table.glm.lr.new)
misclass.glm.lr.new


## optimal tau 

#install.packages("ROCR")
library(ROCR)

predObj.lr.new <- prediction(glm.probs.lr.new, x.lr.new[indtest,10])

perf.lr.new <- performance(predObj.lr.new, "tpr", "fpr")

acc.lr.new <- performance(predObj.lr.new, "acc")
tau.optimal.lr.new <- acc@x.values[[1]]
Accuracy.lr.new <- acc@y.values[[1]]
best.lr.new <- which.max(Accuracy.lr.new)

tau.optimal.lr.new[best.lr.new]

prediction.for.table.glm.optimal.lr.new <- ifelse(glm.probs.lr.new > tau.optimal.lr.new[best.lr.new], 1, 0)
table.glm.optimal.lr.new <- table(observed=x.lr.new[indtest, 10],predicted=prediction.for.table.glm.optimal.lr.new)
table.glm.optimal.lr.new

misclass.glm.optimal.lr.new <- 1 - sum(diag(table.glm.optimal.lr.new)) / sum(table.glm.optimal.lr.new)
misclass.glm.optimal.lr.new


#################################################################

## Logistic Regression Subsets Forecast

# Party Subset

#Democratic Party
indtrain.dem<-which(x[indtrain,"party"]=="Democratic")
length(indtrain.dem) 
indtest.dem<-which(x[indtest,"party"] =="Democratic")
length(indtest.dem) 

fit.glm.dem<-glm(result ~ ., data=x[indtrain.dem, -17], family="binomial")
summary(fit.glm.dem)

tau<- 0.5
glm.probs.dem <- predict(fit.glm.dem,
                         newdata = x[indtest.dem,],
                         type = "response")
prediction.for.table.glm.dem <- ifelse(glm.probs.dem > tau, 1, 0)

table.glm.dem <- table(observed=x[indtest.dem, 18],predicted=prediction.for.table.glm.dem)
table.glm.dem
misclass.glm.dem <- 1 - sum(diag(table.glm.dem)) / sum(table.glm.dem)
misclass.glm.dem

#Republican Party
indtrain.rep<-which(x[indtrain,"party"]=="Republican")
length(indtrain.rep) 
indtest.rep<-which(x[indtest,"party"] =="Republican")
length(indtest.rep) 

fit.glm.rep<-glm(result ~ ., data=x[indtrain.rep, -17], family="binomial")
summary(fit.glm.rep)

tau<- 0.5
glm.probs.rep <- predict(fit.glm.rep,
                         newdata = x[indtest.rep,],
                         type = "response")
prediction.for.table.glm.rep <- ifelse(glm.probs.rep > tau, 1, 0)

table.glm.rep <- table(observed=x[indtest.rep, 18],predicted=prediction.for.table.glm.rep)
table.glm.rep
misclass.glm.rep <- 1 - sum(diag(table.glm.rep)) / sum(table.glm.rep)
misclass.glm.rep


# Gender subset

#Male
indtrain.male<-which(x[indtrain,"gender"]=="Male")
length(indtrain.male) # 5722
indtest.male<-which(x[indtest,"gender"]=="Male")
length(indtest.male) # 2115

fit.glm.male<-glm(result ~ ., data=x[indtrain.male, -5], family="binomial")
summary(fit.glm.male)

tau<- 0.5
glm.probs.male <- predict(fit.glm.male,
                          newdata = x[indtest.male,],
                          type = "response")
prediction.for.table.glm.male <- ifelse(glm.probs.male > tau, 1, 0)

table.glm.male <- table(observed=x[indtest.male, 18],predicted=prediction.for.table.glm.male)
table.glm.male
misclass.glm.male <- 1 - sum(diag(table.glm.male)) / sum(table.glm.male)
misclass.glm.male

#Female
indtrain.female<-which(x[indtrain,"gender"]=="Female")
length(indtrain.female)
indtest.female<-which(x[indtest,"gender"]=="Female")
length(indtest.female) 

fit.glm.female<-glm(result ~ ., data=x[indtrain.female, -5], family="binomial")
summary(fit.glm.female)

tau<- 0.5
glm.probs.female <- predict(fit.glm.female,
                            newdata = x[indtest.female,],
                            type = "response")
prediction.for.table.glm.female <- ifelse(glm.probs.female > tau, 1, 0)

table.glm.female <- table(observed=x[indtest.female, 18],predicted=prediction.for.table.glm.female)
table.glm.female
misclass.glm.female <- 1 - sum(diag(table.glm.female)) / sum(table.glm.female)
misclass.glm.female


# Age Subset

#young (18 to 34)
indtrain.young<-which(x[indtrain,"agec"]>=18 & x[indtrain,"agec"]<=34)
length(indtrain.young)
indtest.young<-which(x[indtest,"agec"]>=18 & x[indtest,"agec"]<=34)
length(indtest.young)

fit.glm.young<-glm(result ~ ., data=x[indtrain.young, -4], family="binomial")
summary(fit.glm.young)

tau<- 0.5
glm.probs.young <- predict(fit.glm.young,
                           newdata = x[indtest.young,],
                           type = "response")
prediction.for.table.glm.young <- ifelse(glm.probs.young > tau, 1, 0)

table.glm.young <- table(observed=x[indtest.young, 18],predicted=prediction.for.table.glm.young)
table.glm.young
misclass.glm.young <- 1 - sum(diag(table.glm.young)) / sum(table.glm.young)
misclass.glm.young


#middle (35 to 54)
indtrain.middle<-which(x[indtrain,"agec"]>=35 & x[indtrain,"agec"]<=54)
length(indtrain.middle)
indtest.middle<-which(x[indtest,"agec"]>=35 & x[indtest,"agec"]<=54)
length(indtest.middle) 

fit.glm.middle<-glm(result ~ ., data=x[indtrain.middle, -4], family="binomial")
summary(fit.glm.middle)

tau<- 0.5
glm.probs.middle <- predict(fit.glm.middle,
                            newdata = x[indtest.middle,],
                            type = "response")
prediction.for.table.glm.middle <- ifelse(glm.probs.middle > tau, 1, 0)

table.glm.middle <- table(observed=x[indtest.middle, 18],predicted=prediction.for.table.glm.middle)
table.glm.middle
misclass.glm.middle <- 1 - sum(diag(table.glm.middle)) / sum(table.glm.middle)
misclass.glm.middle


#old (55+)
indtrain.old<-which(x[indtrain,"agec"] >= 55)
length(indtrain.old) # 4365
indtest.old<-which(x[indtest,"agec"] >= 55)
length(indtest.old) # 1596

fit.glm.old<-glm(result ~ ., data=x[indtrain.old, -4], family="binomial")
summary(fit.glm.old)

tau<- 0.5
glm.probs.old <- predict(fit.glm.old,
                         newdata = x[indtest.old,],
                         type = "response")
prediction.for.table.glm.old <- ifelse(glm.probs.old > tau, 1, 0)

table.glm.old <- table(observed=x[indtest.old, 18],predicted=prediction.for.table.glm.old)
table.glm.old
misclass.glm.old <- 1 - sum(diag(table.glm.old)) / sum(table.glm.old)
misclass.glm.old


#################################################################

#################################################################

#### CCES 2008 Random Forest ####


#install.packages("randomForest")
library(randomForest)

fit.rf<-randomForest(result ~ ., data=x[indtrain,],
                     importance=TRUE)
fit.rf

importance(fit.rf, type=1)
importance(fit.rf, type=2)
varImpPlot(fit.rf,main=NULL)

prediction.for.table.rf <- predict(fit.rf,x[indtest,])
table.rf <- table(observed=x[indtest, 18],predicted=prediction.for.table.rf)
table.rf

misclass.rf <- 1 - sum(diag(table.rf)) / sum(table.rf)
misclass.rf


#################################################################

#################################################################

#### CCES 2008 Support Vector Machines ####


#install.packages("e1071")
library(e1071)

fit.svm <- svm(result ~ ., data=x[indtrain,])
fit.svm

prediction.for.table.svm <- predict(fit.svm, x[indtest,])
table.svm <- table(observed=x[indtest, 18],predicted=prediction.for.table.svm)
table.svm

misclass.svm <- 1 - sum(diag(table.svm)) / sum(table.svm)
misclass.svm



