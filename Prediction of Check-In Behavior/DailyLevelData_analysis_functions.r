# deploy of the functions used for the analysis

extractweather=function(dataset,mindate=min(dataset$date),maxdate=max(dataset$date),
                        latrange=range(dataset$business_lat),longrange=range(dataset$business_long),
                        resol=.5,getdata=FALSE,
                        wear=ifelse("weatherPRCPSNWDSNOWTMAXTMINTOBS.RData"%in%list.files(),"available",NULL)){
  if(wear=="available"){
    load("weatherPRCPSNWDSNOWTMAXTMINTOBS.RData")
  }
  wdatacond=is.null(wear)
  if(getdata | wdatacond){
    require("doParallel")
  
    cl <- makeCluster(detectCores())
    registerDoParallel(cl)
    
    # read the station names
    stations=read.delim(url("https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt"),header = F,quote="",sep="")[,1:3]
    colnames(stations)=c("Station","lat","long")
    stations=stations[strtrim(stations$Station,2)=="US",]
    stations$lat=as.numeric(stations$lat)
    stations$long=as.numeric(stations$long)
    stations=stations[!is.na(stations$lat)|!is.na(stations$long),]
    
    #mindate=min(dataset$date)#"2016-05-01"#
    #maxdate=max(dataset$date)#"2016-05-02"#
    #latrange=range(dataset$business_lat)
    #longrange=range(dataset$business_long)
    
    latseq=c(seq(latrange[1],latrange[2],by=resol),latrange[2])
    longseq=c(seq(longrange[1],longrange[2],by=resol),longrange[2])

    wear=NULL
    k=0
    torunlist=NULL
    for(lat in 1:(length(latseq)-1)){#(length(latseq)-1)
      for(lon in 1:(length(longseq)-1)){
        k=k+1
        torunlist=rbind(torunlist,c(lat,lon))
      }
    }
    wear=foreach(i=1:k,.noexport=ls(),.export=c("latseq","longseq","stations","torunlist","mindate","maxdate"))%dopar%
      {  
        # find the station(s) within the boxes
        lat=torunlist[i,1]
        lon=torunlist[i,2]
        rangelat=c(latseq[lat+1],latseq[lat])
        rangelong=c(longseq[lon],longseq[lon+1])
        indx=(stations$lat>rangelat[2])&(stations$lat<rangelat[1])&(stations$long>rangelong[1])&(stations$long<rangelong[2])
        stations_temp=stations[indx,]
        stations_t=paste(stations_temp$Station,collapse=",")
        temp=paste0("dataset=daily-summaries&dataTypes=PRCP,SNWD,SNOW,TMAX,TMIN,TOBS",
                    "&stations=",stations_t,"&startDate=",mindate,"","&endDate=",maxdate)#,
        #"","&boundingBox=",paste(latseq[lat+1],longseq[lon],latseq[lat],longseq[lon+1],sep=","))##90,-180,-90,180
        valid_url <- TRUE
        a=tryCatch(read.csv(url(paste0("https://www.ncei.noaa.gov/access/services/data/v1?",temp))),error=function(e) {valid_url<<-FALSE})
        toreturn=NULL
        if(valid_url)
          toreturn=list(range=cbind(rangelat,rangelong),data=read.csv(url(paste0("https://www.ncei.noaa.gov/access/services/data/v1?",temp))))
        return(toreturn)
        #print(c(lat,lon,valid_url))
      }
    
    
    stopCluster(cl)
    save(file="weatherPRCPSNWDSNOWTMAXTMINTOBS.RData",list=c("wear"))
  }
  return(wear)
}


weardailyavg=function(wear){
  if("weather_data.RData"%in%list.files()){
    load(file="weather_data.RData")
  }else{
    require("doParallel")
    
    cl <- makeCluster(detectCores())
    registerDoParallel(cl)
    
    wear_avg=NULL
    k=0
    wear_avg=foreach(i=1:length(wear),.noexport=ls(),.export=c("wear"),.packages = c("dplyr"))%dopar%
      {
        if(is.null(wear[[i]])){
          temp=NULL
        }else{
          temp=wear[[i]]$data %>%
            group_by(DATE) %>%
            summarize(PRCP=mean(PRCP,na.rm = T),SNOW=mean(SNOW,na.rm = T),SNWD=mean(SNWD,na.rm = T),
                      TMAX=mean(TMAX,na.rm = T),TMIN=mean(TMIN,na.rm = T),TOBS=mean(TOBS,na.rm = T))
          temp=list(range=wear[[i]]$range,data=temp)}
        return(temp)
        
      }
    stopCluster(cl)
    weather=NULL
    k=0
    for(i in 1:length(wear_avg)){
      if(is.null(wear[[i]]))
        next
      k=k+1
      weather[[k]]=wear_avg[[i]]
      weather[[k]]$data$DATE=as.Date(weather[[k]]$data$DATE)
    }
    save(file="weather_data.RData",list=c("weather"))
  }
  return(weather)
}


makeLiftPlot <- function(Prediction, Evaluate, ModelName){
  iPredictionsSorted <- sort(Prediction,index.return=T,decreasing=T)[2]$ix #extract the index order according to predicted retention
  CustomersSorted <- Evaluate$ch_in_string[iPredictionsSorted] #sort the true behavior of customers according to predictions
  SumChurnReal<- sum(Evaluate$ch_in_string == "Noch_in") #total number of real churners in the evaluation set
  CustomerCumulative=seq(nrow(Evaluate))/nrow(Evaluate) #cumulative fraction of customers
  ChurnCumulative=apply(matrix(CustomersSorted=="Noch_in"),2,cumsum)/SumChurnReal #cumulative fraction of churners
  ProbTD = sum(CustomersSorted[1:floor(nrow(Evaluate)*.1)]=="Noch_in")/floor(nrow(Evaluate)*.1) #probability of churn in 1st decile
  ProbOverall = SumChurnReal / nrow(Evaluate) #overall churn probability
  TDL = ProbTD / ProbOverall
  GINI = sum((ChurnCumulative-CustomerCumulative)/(t(matrix(1,1,nrow(Evaluate))-CustomerCumulative)),na.rm=T)/nrow(Evaluate)
  plot(CustomerCumulative,ChurnCumulative,type="l",main=paste("Lift curve of", ModelName),xlab="Cumulative fraction of No-check-ins (sorted by predicted check-in probability)",ylab="Cumulative fraction of No-check-ins")
  lines(c(0,1),c(0,1),col="blue",type="l",pch=22, lty=2)
  legend(.66,.2,c("According to model","Random selection"),cex=0.8,  col=c("black","blue"), lty=1:2)
  text(0.15,1,paste("TDL = ",round(TDL,2), "; GINI = ", round(GINI,2) ))
  return(data.frame(TDL,GINI))
}
