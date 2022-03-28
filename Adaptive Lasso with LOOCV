library(tidyverse)
library(magrittr)
library(glmnet)
library(pROC)
library(mgcv)

rm(list=ls())
gc()

#Read the data
dat<-read.table("*.csv",sep="",header=T)

vars<-dat[,-dim(dat)[2]]*0

PRED<-rep(0,dim(dat)[1])

for (i in 1:(dim(dat)[1])){

  train<-dada[-i,]
  val<-dada[i,]

  set.seed(5)
  ## Ridge regression with Leave-one-out Cross Validation (LOOCV)
  ridge1_cv <- cv.glmnet(x = as.matrix(train[,-dim(train)[2]]), y = sqrt(as.matrix(train$AGB)),
                         ## type.measure: loss to use for cross-validation.
                         type.measure = "mse",
                         ## 
                         nfold = dim(train)[1],
                         ## ‘alpha = 1’ is the lasso penalty, and ‘alpha = 0’ the ridge penalty.
                         alpha = 0)
  
  #plot(ridge1_cv)  
  
  ## The intercept estimate should be dropped.
  best_ridge_coef <- as.numeric(coef(ridge1_cv, s = ridge1_cv$lambda.min))[-1]
  
  set.seed(5)
  
  ## Perform adaptive LASSO with LOO CV
  
  alasso1_cv <- cv.glmnet(x = as.matrix(train[,-dim(train)[2]]), y = sqrt(as.matrix(train$AGB)),
                          ## type.measure: loss to use for cross-validation.
                          type.measure = "mse",
                          ##
                          nfold = dim(train)[1],
                          ## ‘alpha = 1’ is the lasso penalty, and ‘alpha = 0’ the ridge penalty.
                          alpha = 1,
                          ##
                          ## penalty.factor: Separate penalty factors can be applied to each
                          ##           coefficient. This is a number that multiplies ‘lambda’ to
                          ##           allow differential shrinkage. Can be 0 for some variables,
                          ##           which implies no shrinkage, and that variable is always
                          ##           included in the model. Default is 1 for all variables (and
                          ##           implicitly infinity for variables listed in ‘exclude’). Note:
                          ##           the penalty factors are internally rescaled to sum to nvars,
                          ##           and the lambda sequence will reflect this change.
                          penalty.factor = 1 / abs(best_ridge_coef),
                          ## prevalidated array is returned
                          keep = TRUE)
                          
  ## s: Value(s) of the penalty parameter ‘lambda’ at which
  ##    predictions are required. Default is the entire sequence used
  ##    to create the model.
  
  best_alasso_coef1 <- coef(alasso1_cv, s = alasso1_cv$lambda.min)
  #best_alasso_coef1
    
  vars[i,which(as.numeric(coef(alasso1_cv, s = alasso1_cv$lambda.min)[,1])[-1]!=0)]<-1

  va<-names(vars)[which(as.numeric(coef(alasso1_cv, s = alasso1_cv$lambda.min)[,1])!=0)][-1]

  P<-predict(alasso1_cv, as.matrix(train[,-dim(train)[2]]), s = alasso1_cv$lambda.min,type='response')^2

  CF<-mean(train$AGB)/mean(P)

  PRED[i]<-(predict(alasso1_cv, as.matrix(val[,-dim(val)[2]]), s = alasso1_cv$lambda.min,type='response')^2)*CF
  
}

# Frequency of number of varaibles in the models

table(rowSums(vars))

varsProp<-colSums(vars)/dim(dada)[1]
varsProp

varsPropSE<-sqrt(varsProp*(1-varsProp))

which(varsProp!=0)

varsPropSE[which(varsProp!=0)] ## Selected Variables

write.table(PRED,"*.csv",sep=";",col.names=T,row.names=F,quote=F)
