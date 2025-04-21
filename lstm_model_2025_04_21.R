library(sdtm.oak)
time.now<-Sys.time()
time.now.cha<-as.character(time.now)

Crudington.url.flow_3lag<-paste0("https://eu.datasphere.online/external/timeSeries/download?tsId=52832c88-0e41-4596-8fe4-aa9e6b02d750&from=",paste0(as.Date(time.now.cha)-3,'T00:00:00'),"Z&to=",paste0(as.Date(time.now.cha)-1,'T23:59:59'),"&format=csv")

kisters.crudington.flow_3lag<-GET(Crudington.url.flow_3lag,authenticate("Jaehun.Shin@cranfield.ac.uk", PW.datasphere)) # where ID & PW of datasphere is required

kisters.crudington.cont.flow_3lag<-content(kisters.crudington.flow_3lag)

flow_lag.df<-data.frame(kisters.crudington.cont.flow_3lag)

crudington.lag.list<-strsplit(flow_lag.df$X.timestamp.value.quality.code.remark, ";")

crudington.lag.df.NA<-data.frame(matrix(nrow=length(crudington.lag.list),ncol=length(crudington.lag.list[[1]])))

for (i in 1:length(crudington.lag.df.NA$X1)){
  crudington.lag.df.NA[i,]<-crudington.lag.list[[i]]
  print(i/length(crudington.lag.df.NA$X1)*100)}

crudington.lag.df<-na.omit(crudington.lag.df.NA)
names(crudington.lag.df)<-c('time','crud.flow','x')

crudington.lag.df1<-data.frame(crudington.lag.df$time,crudington.lag.df$crud.flow)
names(crudington.lag.df1)<-c('time','crud.flow')

crudington.lag.df1$time<-as.POSIXct(crudington.lag.df1$time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

crudington.lag.df1$crud.flow<-as.numeric(crudington.lag.df1$crud.flow)

crudington.lag.df1$date<-as.Date(crudington.lag.df1$time)

lag6.flow.df<-subset(crudington.lag.df1,crudington.lag.df1$date==unique(as.Date(crudington.lag.df1$time))[1])

flow.lag6<-mean(as.numeric(lag6.flow.df$crud.flow))

lag5.flow.df<-subset(crudington.lag.df1,crudington.lag.df1$date==unique(as.Date(crudington.lag.df1$time))[2])

flow.lag5<-mean(as.numeric(lag5.flow.df$crud.flow))
flow.lag5

lag4.flow.df<-subset(crudington.lag.df1,crudington.lag.df1$date==unique(as.Date(crudington.lag.df1$time))[3])

flow.lag4<-mean(as.numeric(lag4.flow.df$crud.flow))
flow.lag4

predictors<-data.frame(flow.lag6,flow.lag5,flow.lag4,0,4,5,6,1,2,3,as.numeric(strftime(Sys.Date(), format = "%j"))) # here 0~6 will be substituted to Hydromaster rainfall value

names(predictors)<-c("Flow_lag4","Flow_lag5","Flow_lag6","Rain","Rain_lag4","Rain_lag5","Rain_lag6","Rain_lag1","Rain_lag2","Rain_lag3","day_number")

x_train <- array(data = as.matrix(predictors), dim = c(1,1, length(predictors)))

df.check<-as.data.frame(x_train)

loaded.model<-load_model_tf("D:/1_Cranfield project/4_Shropshire project/crudington LSTM daily model/crudington.d.model.20250403_3day_NRFA") # where the 3day Crudington model is located
library(keras)
predictions.using.crud.d.model <- predict(loaded.model, x_train,batch_size=1)

if(predictions.using.crud.d.model<0){predictions.using.crud.d.model<-0}
plot(predictions.using.crud.d.model)
