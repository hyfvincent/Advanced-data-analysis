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

#object 3
#####________seperate plot_________________##################
library(ggplot2)
library(ggmap)
library(mapproj)
library(nycmaps)
library(maps)
library(mapdata)
library(maptools)
library(gpclib)
library(sp)


counties=map_data("county")
nydata=subset(counties,region=='new york')
unique(nydata$subregion)

nydata_bronx=subset(nydata,subregion=='bronx')
myMap+geom_polygon(data=nydata_bronx,aes(x=long,y=lat,group=group),fill=NA,color='red')
data=flood[flood$cz_name_str %in% c('BRONX(ZONE)','BRONX CO.'),]

data$death_ind=ifelse(data$deaths_direct==0&data$injuries_direct==0&data$injuries_indirect==0&data$deaths_indirect==0
                      ,"","death-cause")
data$damage_ind=ifelse(data$damage_property_num==0&data$damage_crops_num==0,'','damage-cause')

path_data1=na.omit(data.frame(data$begin_lon,data$begin_lat,data$event_type)) %>% mutate(group=seq(19))
path_data2=na.omit(data.frame(data$end_lon,data$end_lat,data$event_type)) %>% mutate(group=seq(19))
nrow(path_data1)
nrow(path_data2)
path_data1[20:38,]=path_data2
path_data=path_data1 %>% rename(lon=data.begin_lon,lat=data.begin_lat,event_type=data.event_type)
path_data
data2=data[data$color_ind!=1,]
ggmap(get_googlemap(center=c(lon=mean(nydata_bronx$long),lat=mean(nydata_bronx$lat)),zoom=12))+
  geom_polygon(data=nydata_bronx,aes(x=long,y=lat,group=group),fill=NA,color='red')+
  geom_point(data=data,aes(x=begin_lon,y=begin_lat,size=begin_range+1),color='red',alpha=0.5)+
  geom_text(data =data, aes(x = begin_lon, y = begin_lat, label = death_ind), size = 3, vjust = 0, hjust = -0.5)+
  geom_point(data=data,aes(x=end_lon,y=end_lat,size=end_range+1),color='blue',alpha=0.5)+
  geom_text(data =data, aes(x =end_lon, y =end_lat, label = death_ind), size = 3, vjust = 0, hjust = -0.5)+
  geom_path(data=path_data,aes(x=lon,y=lat,group=group),lineend = 'round',size=1.5,color='pink')+
  xlab("latitude")+ylab("longtitue")+ggtitle("Bronx")+
  theme(legend.position="none")

areaPlot=function(area,varname,zoom=12){
  nydata_bronx=subset(nydata,subregion==area)
  data=flood[flood$cz_name_str %in% varname,]
  #whether damage
  data$death_ind=ifelse(data$deaths_direct==0&data$injuries_direct==0&data$injuries_indirect==0&data$deaths_indirect==0
                        ,"","death-cause")
  data$damage_ind=ifelse(data$damage_property_num==0&data$damage_crops_num==0,'','damage-cause')
  
  path_data1=na.omit(data.frame(data$begin_lon,data$begin_lat,data$event_type)) %>% mutate(group=seq(length(na.omit(data$begin_lat))))
  path_data2=na.omit(data.frame(data$end_lon,data$end_lat,data$event_type)) %>% mutate(group=seq(length(na.omit(data$begin_lat))))
  x1=nrow(path_data1)
  x2=nrow(path_data2)
  path_data1[x1+1:x1+x2,]=path_data2
  path_data=path_data1 %>% rename(lon=data.begin_lon,lat=data.begin_lat,event_type=data.event_type)
  
  ggmap(get_googlemap(center=c(lon=mean(nydata_bronx$long),lat=mean(nydata_bronx$lat)),zoom))+
    geom_polygon(data=nydata_bronx,aes(x=long,y=lat,group=group),fill=NA,color='red')+
    geom_point(data=data,aes(x=begin_lon,y=begin_lat,size=begin_range+1),color="red",alpha=0.5)+
    geom_text(data =data, aes(x = begin_lon, y = begin_lat, label = death_ind), size = 3, vjust = 0, hjust = -0.5)+
    geom_point(data=data,aes(x=end_lon,y=end_lat,size=end_range+1),color="blue",alpha=0.5)+
    geom_text(data =data, aes(x = end_lon, y = end_lat, label = damage_ind), color='red',size = 3, vjust = 0, hjust = -0.5)+
    geom_path(data=path_data,aes(x=lon,y=lat,group=group),lineend = 'round',size=1.1,color='pink')+
    xlab("latitude")+ylab("longtitue")+ggtitle(area)+
    theme(legend.position='none')
}
areaPlot("kings",c("KINGS(BROOKLYN)(ZONE)","KINGS CO."))
areaPlot("new york",c("NEW YORK(MANHATTAN(ZONE)","NEW YORK CO."))
areaPlot("queens",c("QUEENS CO.","NORTHERN QUEENS (ZONE)","SOUTHERN QUEENS (ZONE)"),zoom=11)
areaPlot("richmond",c("RICHMOND CO.","RICHMOND (STATEN IS.) (ZONE)"),zoom=11)


#dir.create("./presentation")

devtools::install_github("zachcp/nycmaps") 
library(mapproj)
library(nycmaps)
library(maps)
library(mapdata)
library(maptools)
library(ggmap)
library(ggplot2)
png('c:/users/an/desktop/presentation/position.png')
map(database="nyc",col="white",fill=TRUE, projection="gilbert", orientation= c(90,0,225))
lon_start=na.omit(flood$begin_lon)
lat_start=na.omit(flood$begin_lat)
lon_end=na.omit(flood$end_lon)
lat_end=na.omit(flood$end_lat)
coord_start=mapproject(lon_start,lat_start,proj='gilbert',orientation = c(90,0,225))
points(coord_start,pch=20,cex=1,col=seq(lat_end))
text(coord_start,labels = seq(lat_end),cex=0.5)
coord_end=mapproject(lon_end,lat_end,proj='gilbert',orientation = c(90,0,225))
points(coord_end,pch=10,cex=1,col=seq(lat_end))
text(coord_end,labels = seq(lat_end),cex=0.5)
legend('topleft',pch=c(20,10),legend=c('Start Position','End Position'))
title("Flood in NYC: Position Visulization")
dev.off()
  ## Graphics
  #bar plot
  used=select(flood,c(cz_name_str,event_type))
ggplot(used%>%group_by(event_type),aes(event_type))+geom_bar(aes(fill=used$cz_name_str))
ggplot(used%>%group_by(event_type),aes(event_type))+geom_bar(aes(fill=used$cz_name_str),position='dodge')
## pie plot
#data
names(flood)
used=select(flood,c(event_type,cz_name_str,flood_cause))
ggplot(used,aes(x=event_type,fill=flood_cause))+geom_bar(width = 1)+
  coord_polar(theta="y")+xlab("Freq'")+ylab("type")+ggtitle("flood casue vs. flood type")

#scatterplot
data=flood
data$death_ind=ifelse(data$deaths_direct==0&data$injuries_direct==0&data$injuries_indirect==0&data$deaths_indirect==0
                      ,"","death-cause")
data$damage_ind=ifelse(data$damage_property_num==0&data$damage_crops_num==0,'','damage-cause')
used=select(data,c(event_type,begin_date,cz_name_str,death_ind,damage_ind))
library(lubridate)
used$begin_date=as.character(used$begin_date)
used$begin_date=mdy(used$begin_date)
used$begin_date=as.POSIXct(used$begin_date,origin="2006/01/01")
library(scales)
ggplot(used,aes(x=begin_date,y=event_type,color=cz_name_str,shape=as.factor(death_ind),size=as.factor(damage_ind)))+geom_point()+
  scale_x_datetime(breaks=date_breaks("1 month"))+theme(axis.text.x=element_text(angle=90))+
  geom_text(data=used,aes(x=begin_date,y=event_type,label=as.character(death_ind)),size=3,color="red",hjust=0,vjust=1)+
  geom_text(data=used,aes(x=begin_date,y=event_type,label=as.character(damage_ind)),size=3,color="red",hjust=0,vjust=1)+
  ggtitle("Event-type & Event-time & Event-effect")+xlab("2006-2016(unit:1 month)")+ylab("Event type")


##_________analysis issuo________#
nrow(data)
nrow(data[data$death_ind!='',])
nrow(data[data$damage_ind!='',])
library(vcd)
vcd::mosaic(~event_type+cz_name_str,data=flood,shade=T,legend=TRUE,direction="v",rot_labels=c(0,90,0,0),main="event type and dist.")
data$flood_cause=as.character(data$flood_cause)
for (i in seq(data$flood_cause)){
  if(!((as.character(data$flood_cause[i]) %in% c("Heavy Rain","Heavy Rain / Tropical System")))){
    data$flood_cause[i]='Not Specified'
  }
}
fix(data)
mosaic(~event_type+flood_cause,data=data,shade=T,legend=TRUE,direction="v",rot_labels=c(90,0,0,0),main="event type and casue")

mosaic(~event_type+flood_cause,data=flood[flood$flood_cause %in%c("Heavy Rain","Heavy Rain / Tropical System"),],shade=T,legend=TRUE,direction="v",rot_labels=c(90,0,0,0),main="event type and casue")

eucDis=function(x1,y1,x2,y2){
  return(sqrt((x1-x2)^2+(y1-y2)^2))
}
names(data)
data$l_move=eucDis(data$begin_lat,data$begin_lon,data$end_lat,data$end_lon)
data$l_move_ind=cut(data$l_move,3)
mosaic(~cz_name_str+l_move_ind,data=data,shade=T,legend=T,direction="v",rot_labels=c(30,30,30,30))
mosaic(~cz_name_str+begin_range+event_type,data=data,shade=T,legend=T,direction="v",rot_labels=c(90,90,90,90))
mosaic(~cz_name_str+begin_range,data=data,shade=T,legend=T,direction="v",rot_labels=c(90,90,90,90))
cor(na.omit(data.frame(data$begin_range,data$end_range)))
mosaic(cz_name_str~begin_range|event_type,data=data,shade=T,direction="v",rot_labels=c(90,90,90,90))
for (i in seq(data$event_type)){
  if (data$damage_ind[i]=='damage-cause' & data$death_ind[i] != 'death-cause') {
    data$effect[i]='damage'
  }else if (data$damage_ind[i]!='damage-cause' & data$death_ind[i] == 'death-cause') {
    data$effect[i]='death'
  }else if (data$damage_ind[i]=='damage-cause' & data$death_ind[i] == 'death-cause') {
    data$effect[i]='damage'
  }else{
    data$effect[i]='None'
  } 
}
mosaic(~cz_name_str+effect,data=data,shade=T,direction="v",rot_labels=c(90,90,90,90))

# bayes
setwd("/users/andy/desktop/presentation")
list.files()
flood=read.csv("./flooding_nyc5borough.csv",stringsAsFactors = F)
names(flood)=tolower(names(flood))
data=flood
data$death_ind=ifelse(data$deaths_direct==0&data$injuries_direct==0&data$injuries_indirect==0&data$deaths_indirect==0
                      ,"","death-cause")
data$damage_ind=ifelse(data$damage_property_num==0&data$damage_crops_num==0,'','damage-cause')

for (i in seq(data$event_type)){
  if (data$damage_ind[i]=='damage-cause' & data$death_ind[i] != 'death-cause') {
    data$effect[i]=1
  }else if (data$damage_ind[i]!='damage-cause' & data$death_ind[i] == 'death-cause') {
    data$effect[i]=1
  }else if (data$damage_ind[i]=='damage-cause' & data$death_ind[i] == 'death-cause') {
    data$effect[i]=1
  }else{
    data$effect[i]=0
  }
  
}
eucDis=function(x1,y1,x2,y2){
  return(sqrt((x1-x2)^2+(y1-y2)^2))
}
data$l_move=eucDis(data$begin_lat,data$begin_lon,data$end_lat,data$end_lon)

data$region_code=as.numeric(data$cz_name_str)
data$type_code=as.numeric(data$event_type)
library(dplyr)
used=select(data,c(effect,region_code,type_code,l_move,begin_range,end_range))
nna.used=na.omit(used)
stan.code="
data{
int<lower=0> N;//number of observation
int<lower=0> p;//number of parameters
int effect[N];
int<lower=0> region[N];
int<lower=0> type[N];
int<lower=0> brange[N];
int<lower=0> erange[N];
real<lower=0> move[N];
}
parameters{
real beta[p];
}

transformed parameters {
real<lower=0> odds[N];
real<lower=0,upper=1> prob[N];

for (i in 1:N){
odds[i] <- exp(beta[1]+beta[2]*region[i]+beta[3]*type[i]+beta[4]*brange[i]+beta[5]*erange[i]+beta[6]*move[i]);
prob[i] <- odds[i]/(odds[i]+1);
}
}
model{
effect~bernoulli(prob);
}
"
dat=list(
  N=nrow(nna.used),
  p=6,
  effect=nna.used$effect,
  region=nna.used$region_code,
  type=nna.used$type_code,
  brange=nna.used$begin_range,
  erange=nna.used$end_range,
  move=nna.used$l_move
)

resstan=stan(model_code=stan.code,data=dat,iter=2000,chains=5)

