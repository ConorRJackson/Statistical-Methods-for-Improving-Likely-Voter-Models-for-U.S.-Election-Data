### 2008 Key State subset Logistic Regression ###


#install.packages("Amelia")
library(Amelia)
load("D:/Dissertation/R Code/CCES/2008/cces_2008_common.RData")

x.state<-x
x.state$result[x.state$vote_gen08=="validated record of voting in general election"]<-1
x.state$result[x.state$vote_gen08=="did not vote - verified record of not voting"]<-0
x.state<-x.state[-which(is.na(x.state$result)==TRUE),]
x.state$result<-factor(x.state$result,levels=c(0,1))
x.state<-x.state[c("V245","CC324_1","CC326","V207","V208","V209","V211","V213",
      "V214","V216","V242","V243","V246","CC332","CC333","CC334","CC402",
      "V206","result")]
colnames(x.state)<-c("thought","before","intend","birthyear","gender",
      "employment","race","education","marital","religion","child",
      "ideology","income","citizen","own_or_rent","residence","party",
      "state","result") 
x.state$birthyear<-2008-as.numeric(levels(x.state$birthyear)[as.integer(x.state$birthyear)])
colnames(x.state)[4]<-"agec"
x.state<-droplevels(x.state,except = "agec")

#install.packages("sperrorest")
library(sperrorest)
# function remove_missing_levels
#to remove levels missing in the training set from test set in states which require it

set.seed(123)
N.x.state<-nrow(x.state)
indtrain.s<-sample(1:N.x.state, replace=TRUE)
indtrain.s<-sort(indtrain.s)
indtest.s<-setdiff(1:N.x.state, indtrain.s)
dim(x.state[indtrain.s,]) 
dim(x.state[indtest.s,]) 

#Florida
indtrain.s.Florida<-which(x.state[indtrain.s,"state"]=="Florida")
length(indtrain.s.Florida)
indtest.s.Florida<-which(x.state[indtest.s,"state"]=="Florida")
length(indtest.s.Florida)

fit.glm.Florida<-glm(result ~ ., data=x.state[indtrain.s.Florida, -18], family="binomial")
summary(fit.glm.Florida)
tau<- 0.5
glm.probs.Florida <- predict(fit.glm.Florida,
                         newdata = x.state[indtest.s.Florida,],
                         type = "response")
prediction.for.table.glm.Florida <- ifelse(glm.probs.Florida > tau, 1, 0)

table.glm.Florida <- table(observed=x.state[indtest.s.Florida, 19],
                           predicted=prediction.for.table.glm.Florida)
table.glm.Florida
misclass.glm.Florida <- 1 - sum(diag(table.glm.Florida)) / sum(table.glm.Florida)
misclass.glm.Florida

#Michigan
indtrain.s.Michigan<-which(x.state[indtrain.s,"state"]=="Michigan")
length(indtrain.s.Michigan)
indtest.s.Michigan<-which(x.state[indtest.s,"state"]=="Michigan")
length(indtest.s.Michigan)

fit.glm.Michigan<-glm(result ~ ., data=x.state[indtrain.s.Michigan, -18], family="binomial")
summary(fit.glm.Michigan)
tau<- 0.5
glm.probs.Michigan <- predict(fit.glm.Michigan,
                             newdata =remove_missing_levels(fit=fit.glm.Michigan,test_data= x.state[indtest.s.Michigan, -18]),
                             type = "response")
prediction.for.table.glm.Michigan <- ifelse(glm.probs.Michigan > tau, 1, 0)

table.glm.Michigan <- table(observed=x.state[indtest.s.Michigan, 19],
                           predicted=prediction.for.table.glm.Michigan)
table.glm.Michigan
misclass.glm.Michigan <- 1 - sum(diag(table.glm.Michigan)) / sum(table.glm.Michigan)
misclass.glm.Michigan

#Ohio
indtrain.s.Ohio<-which(x.state[indtrain.s,"state"]=="Ohio")
length(indtrain.s.Ohio)
indtest.s.Ohio<-which(x.state[indtest.s,"state"]=="Ohio")
length(indtest.s.Ohio)

fit.glm.Ohio<-glm(result ~ ., data=x.state[indtrain.s.Ohio, -18], family="binomial")
summary(fit.glm.Ohio)
tau<- 0.5
glm.probs.Ohio <- predict(fit.glm.Ohio,
                          newdata =remove_missing_levels(fit=fit.glm.Ohio,test_data= x.state[indtest.s.Ohio, -18]),
                          type = "response")
prediction.for.table.glm.Ohio <- ifelse(glm.probs.Ohio > tau, 1, 0)

table.glm.Ohio <- table(observed=x.state[indtest.s.Ohio, 19],
                        predicted=prediction.for.table.glm.Ohio)
table.glm.Ohio
misclass.glm.Ohio <- 1 - sum(diag(table.glm.Ohio)) / sum(table.glm.Ohio)
misclass.glm.Ohio

#Pennsylvania
indtrain.s.Pennsylvania<-which(x.state[indtrain.s,"state"]=="Pennsylvania")
length(indtrain.s.Pennsylvania)
indtest.s.Pennsylvania<-which(x.state[indtest.s,"state"]=="Pennsylvania")
length(indtest.s.Pennsylvania)

fit.glm.Pennsylvania<-glm(result ~ ., data=x.state[indtrain.s.Pennsylvania, -18], family="binomial")
summary(fit.glm.Pennsylvania)
tau<- 0.5
glm.probs.Pennsylvania <- predict(fit.glm.Pennsylvania,
                          newdata = x.state[indtest.s.Pennsylvania,],
                          type = "response")
prediction.for.table.glm.Pennsylvania <- ifelse(glm.probs.Pennsylvania > tau, 1, 0)

table.glm.Pennsylvania <- table(observed=x.state[indtest.s.Pennsylvania, 19],
                        predicted=prediction.for.table.glm.Pennsylvania)
table.glm.Pennsylvania
misclass.glm.Pennsylvania <- 1 - sum(diag(table.glm.Pennsylvania)) / sum(table.glm.Pennsylvania)
misclass.glm.Pennsylvania

#Wisconsin
indtrain.s.Wisconsin<-which(x.state[indtrain.s,"state"]=="Wisconsin")
length(indtrain.s.Wisconsin)
indtest.s.Wisconsin<-which(x.state[indtest.s,"state"]=="Wisconsin")
length(indtest.s.Wisconsin)

fit.glm.Wisconsin<-glm(result ~ ., data=x.state[indtrain.s.Wisconsin, -18], family="binomial")
summary(fit.glm.Wisconsin)
tau<- 0.5
glm.probs.Wisconsin <- predict(fit.glm.Wisconsin,
                               newdata =remove_missing_levels(fit=fit.glm.Wisconsin,test_data= x.state[indtest.s.Wisconsin, -18]),
                                  type = "response")
prediction.for.table.glm.Wisconsin <- ifelse(glm.probs.Wisconsin > tau, 1, 0)

table.glm.Wisconsin <- table(observed=x.state[indtest.s.Wisconsin, 19],
                                predicted=prediction.for.table.glm.Wisconsin)
table.glm.Wisconsin
misclass.glm.Wisconsin <- 1 - sum(diag(table.glm.Wisconsin)) / sum(table.glm.Wisconsin)
misclass.glm.Wisconsin












