#ADA obj 2#
#data processing#
load("C:/Users/Owner/Desktop/ADA/Proj/need.RData")
flood<-need[need$EVENT_TYPE=="Flood"|need$EVENT_TYPE=="Flash Flood",]
flood<-na.omit(flood)
weather<-read.csv("C:/Users/Owner/Desktop/ADA/Proj/weather.txt")
weather<-na.omit(weather)
flood<-flood[flood$BEGIN_TIME>=as.POSIXct(weather$EST)[1],]
label<-vector(length=dim(weather)[1])
for(i in 1:length(label))
{ if(sum(as.Date(weather$EST)[i]==as.Date(flood$BEGIN_TIME))==0)
 {label[i]=0}
  else{label[i]=1}
}
flooddata<-data.frame(weather$Mean.TemperatureF,weather$MeanDew.PointF,weather$Mean.Humidity,
                      weather$Mean.Sea.Level.PressureIn,weather$Mean.VisibilityMiles,
                      weather$Mean.Wind.SpeedMPH,weather$CloudCover,weather$WindDirDegrees,label)

#train & test index#
TPR<-vector(length=100)
results<-vector(length=100)
for(k in 1:100)
{
ind.test<-sample(dim(flooddata)[1],0.2*dim(flooddata)[1])
ind.train<-c(1:dim(flooddata)[1])[-ind.test]

#gbm#
weights<-vector(length=length(ind.train))
for(i in 1:length(weights))
{ if(label[ind.train][i]==1)
{ weights[i]<-3}
  else{weights[i]<-1}}
weights<-weights/sum(weights)


library(gbm)
fit.gbm<-gbm(label~.,data=flooddata[ind.train,],distribution="bernoulli",n.trees=100,cv.folds=5,interaction.depth=2,weights=weights,shrinkage=0.2)
yhat.gbm<-predict(fit.gbm,data.frame(flooddata[ind.test,-9]),type="response")
yhat.gbm<-ifelse(yhat.gbm>=0.5,1,0)
#sum(yhat.gbm==1&label[ind.test]==1)
#mean(yhat.gbm==label[ind.test])
#sum(yhat.gbm)


#nnet#
#library(h2o)
#localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)
#dat_h2o <- as.h2o(data.frame(flooddata[ind.train,]))
#model <- 
#    h2o.deeplearning(x = 1:8,  # column numbers for predictors
#                     y = 9,   # column number for label
#                     dat_h2o, # data in H2O format
#                     activation = "MaxoutWithDropout", # or 'Tanh'
#                     input_dropout_ratio = 0.2, # % of inputs dropout                     
#                     hidden_dropout_ratios = c(0.2,0.2,0.2), # % for nodes dropout
#                     balance_classes = FALSE, 
#                     hidden = c(20,20,20), # five layers of 50 nodes
#                     epochs = 20,
                     #initial_weights = weights,
                     #distribution="bernoulli",
#                     nfolds=5,  #cross-valid part
#                     fold_assignment =c("Random"),
#                     keep_cross_validation_predictions = TRUE,
#                     keep_cross_validation_fold_assignment = TRUE)
#test_h2o<-as.h2o(data.frame(flooddata[ind.test,-9]))
#h2o_yhat_test<-h2o.predict(model,test_h2o)
#test_error_dl<-as.data.frame(h2o_yhat_test)
#yhat.dl<-ifelse(test_error_dl>=0.5,1,0)
#mean(yhat.dl==label[ind.test])


#RM#
library(randomForest)
fit.rm<-randomForest(x=data.frame(flooddata[ind.train,-9]),y=as.factor(label[ind.train]),ntree=100,mty=74,importance=TRUE,weights=weights)
yhat.rm<-predict(fit.rm,data.frame(flooddata[ind.test,-9]),ntree=100,type="response")
#mean(yhat.rm==label[ind.test])


#logistic#
library(glmnet)
fit.log<-glm(label~.,data=data.frame(flooddata[ind.train,]),family=binomial(link='logit'),weights=weights)
yhat.log<-predict(fit.log,flooddata[ind.test,-9],type="response")
yhat.log<-ifelse(yhat.log>=0.5,1,0)
#mean(yhat.log==label[ind.test])

#VAR#
#library(vars)
#yhat.var<-vector(length=1001)
#for(i in 4496:5496)
#{fit.var<-VAR(flooddata[1:i,],p=5,type="both")
# y.fit<-predict(fit.var,n.ahead=1,ci=0.95)
# yhat.var[i-4495]<-y.fit$fcst$label[1]}
#yhat.var<-ifelse(yhat.var>=0.5,1,0)
#mean(yhat.var==label[4496:5496])

#Adaboost#


fit.ada<-gbm(label~.,data=flooddata[ind.train,],distribution="adaboost",n.trees=100,cv.folds=5,interaction.depth=2,shrinkage=0.2,weights=weights)
yhat.ada<-predict(fit.ada,data.frame(flooddata[ind.test,-9]),type="response")
yhat.ada<-ifelse(yhat.ada>=0.5,1,0)
#sum(yhat.ada==1&label[ind.test]==1)
#mean(yhat.ada==label[ind.test])
#sum(yhat.ada)

#Combine all#
yhat<-vector(length=length(ind.test))
for(i in 1:length(yhat))
 { if(sum(yhat.ada[i],yhat.log[i],(as.numeric(yhat.rm[i])-1),yhat.gbm[i])>0)
   {yhat[i]=1}
   else {yhat[i]=0}
}
TPR[k]<-sum(yhat==1&label[ind.test]==1)/sum(label[ind.test]==1)
results[k]<-mean(yhat==label[ind.test])
}





TPR.gbm<-vector(length=100)
TPR.rm<-vector(length=100)
TPR.log<-vector(length=100)
TPR.ada<-vector(length=100)
TPR<-vector(length=100)
FPR.gbm<-vector(length=100)
FPR.rm<-vector(length=100)
FPR.log<-vector(length=100)
FPR.ada<-vector(length=100)
FPR<-vector(length=100)
for(k in 1:100)
{
  ind.test<-sample(dim(flooddata)[1],0.2*dim(flooddata)[1])
  ind.train<-c(1:dim(flooddata)[1])[-ind.test]
  
  #gbm#
  weights<-vector(length=length(ind.train))
  for(i in 1:length(weights))
  { if(label[ind.train][i]==1)
  { weights[i]<-3}
    else{weights[i]<-1}}
  weights<-weights/sum(weights)
  
  
  library(gbm)
  fit.gbm<-gbm(label~.,data=flooddata[ind.train,],distribution="bernoulli",n.trees=100,cv.folds=5,interaction.depth=2,weights=weights,shrinkage=0.2)
  yhat.gbm<-predict(fit.gbm,data.frame(flooddata[ind.test,-9]),type="response")
  yhat.gbm<-ifelse(yhat.gbm>=0.5,1,0)
  #sum(yhat.gbm==1&label[ind.test]==1)
  #mean(yhat.gbm==label[ind.test])
  #sum(yhat.gbm)
  TPR.gbm[k]<-sum(yhat.gbm==1&label[ind.test]==1)/sum(label[ind.test]==1)
  FPR.gbm[k]<-sum(yhat.gbm==1&label[ind.test]==0)/sum(label[ind.test]==0)
  #nnet#
  #library(h2o)
  #localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)
  #dat_h2o <- as.h2o(data.frame(flooddata[ind.train,]))
  #model <- 
  #    h2o.deeplearning(x = 1:8,  # column numbers for predictors
  #                     y = 9,   # column number for label
  #                     dat_h2o, # data in H2O format
  #                     activation = "MaxoutWithDropout", # or 'Tanh'
  #                     input_dropout_ratio = 0.2, # % of inputs dropout                     
  #                     hidden_dropout_ratios = c(0.2,0.2,0.2), # % for nodes dropout
  #                     balance_classes = FALSE, 
  #                     hidden = c(20,20,20), # five layers of 50 nodes
  #                     epochs = 20,
  #initial_weights = weights,
  #distribution="bernoulli",
  #                     nfolds=5,  #cross-valid part
  #                     fold_assignment =c("Random"),
  #                     keep_cross_validation_predictions = TRUE,
  #                     keep_cross_validation_fold_assignment = TRUE)
  #test_h2o<-as.h2o(data.frame(flooddata[ind.test,-9]))
  #h2o_yhat_test<-h2o.predict(model,test_h2o)
  #test_error_dl<-as.data.frame(h2o_yhat_test)
  #yhat.dl<-ifelse(test_error_dl>=0.5,1,0)
  #mean(yhat.dl==label[ind.test])
  
  
  #RM#
  library(randomForest)
  fit.rm<-randomForest(x=data.frame(flooddata[ind.train,-9]),y=as.factor(label[ind.train]),ntree=100,mty=74,importance=TRUE,weights=weights)
  yhat.rm<-predict(fit.rm,data.frame(flooddata[ind.test,-9]),ntree=100,type="response")
  #mean(yhat.rm==label[ind.test])
  TPR.rm[k]<-sum(yhat.rm==1&label[ind.test]==1)/sum(label[ind.test]==1)
  FPR.rm[k]<-sum(yhat.rm==1&label[ind.test]==0)/sum(label[ind.test]==0)
  #logistic#
  library(glmnet)
  fit.log<-glm(label~.,data=data.frame(flooddata[ind.train,]),family=binomial(link='logit'),weights=weights)
  yhat.log<-predict(fit.log,flooddata[ind.test,-9],type="response")
  yhat.log<-ifelse(yhat.log>=0.5,1,0)
  #mean(yhat.log==label[ind.test])
  TPR.log[k]<-sum(yhat.log==1&label[ind.test]==1)/sum(label[ind.test]==1)
  FPR.log[k]<-sum(yhat.log==1&label[ind.test]==0)/sum(label[ind.test]==0)
  #VAR#
  #library(vars)
  #yhat.var<-vector(length=1001)
  #for(i in 4496:5496)
  #{fit.var<-VAR(flooddata[1:i,],p=5,type="both")
  # y.fit<-predict(fit.var,n.ahead=1,ci=0.95)
  # yhat.var[i-4495]<-y.fit$fcst$label[1]}
  #yhat.var<-ifelse(yhat.var>=0.5,1,0)
  #mean(yhat.var==label[4496:5496])
  
  #Adaboost#
  
  
  fit.ada<-gbm(label~.,data=flooddata[ind.train,],distribution="adaboost",n.trees=100,cv.folds=5,interaction.depth=2,shrinkage=0.2,weights=weights)
  yhat.ada<-predict(fit.ada,data.frame(flooddata[ind.test,-9]),type="response")
  yhat.ada<-ifelse(yhat.ada>=0.5,1,0)
  TPR.ada[k]<-sum(yhat.gbm==1&label[ind.test]==1)/sum(label[ind.test]==1)
  FPR.ada[k]<-sum(yhat.ada==1&label[ind.test]==0)/sum(label[ind.test]==0)
  
  yhat<-vector(length=length(ind.test))
  for(i in 1:length(yhat))
  { if(sum(yhat.ada[i],yhat.log[i],(as.numeric(yhat.rm[i])-1),yhat.gbm[i])>0)
  {yhat[i]=1}
    else {yhat[i]=0}
  }
  TPR[k]<-sum(yhat==1&label[ind.test]==1)/sum(label[ind.test]==1)
  FPR[k]<-sum(yhat==1&label[ind.test]==0)/sum(label[ind.test]==0)
}
library(ggplot2)
df_gbm<-data.frame(TPR.gbm,FPR.gbm)
df_rm<-data.frame(TPR.rm,FPR.rm)
df_log<-data.frame(TPR.log,FPR.log)
df_ada<-data.frame(TPR.ada,FPR.ada)
df_all<-data.frame(TPR,FPR)
ggplot(df_gbm,aes(FPR.gbm,TPR.gbm))+geom_smooth(size = 2, alpha = 0.7)+
  labs(title= "ROC GBM curve", 
       x = "False Positive Rate (1-Specificity)", 
       y = "True Positive Rate (Sensitivity)")

ggplot(df_rm,aes(FPR.rm,TPR.rm))+geom_smooth(size = 2, alpha = 0.7)+
  labs(title= "ROC RM curve", 
       x = "False Positive Rate (1-Specificity)", 
       y = "True Positive Rate (Sensitivity)")

ggplot(df_log,aes(FPR.log,TPR.log))+geom_smooth(size = 2, alpha = 0.7)+
  labs(title= "ROC Logistic curve", 
       x = "False Positive Rate (1-Specificity)", 
       y = "True Positive Rate (Sensitivity)")

ggplot(df_ada,aes(FPR.ada,TPR.ada))+geom_smooth(size = 2, alpha = 0.7)+
  labs(title= "ROC Adaboost curve", 
       x = "False Positive Rate (1-Specificity)", 
       y = "True Positive Rate (Sensitivity)")

ggplot(df_all,aes(FPR,TPR))+geom_smooth(size = 2, alpha = 0.7)+
  labs(title= "ROC Combine all curve", 
       x = "False Positive Rate (1-Specificity)", 
       y = "True Positive Rate (Sensitivity)")
