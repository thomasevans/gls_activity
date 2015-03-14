#SET WORKING DIRECTORY, AND SET VECTOR OF COLUMN NAMES 
setwd("F:/Documents/Work/GLS_DATA/Guillemots/")  #select working directory   #win
file.in.light <- "F:/Documents/Work/GLS_DATA/Guillemots/2010/gl12013/gl12013_000_thresh10_both_0340_all.trj"
file.in.activity <-  "F:/Documents/Work/GLS_DATA/Guillemots/2010/gl12013/gl12013_000_aak962_daily_summary.csv"

start.2009 <- as.POSIXct("2009-06-21", tz="GMT") 
end.2011 <- as.POSIXct("2010-06-20", tz="GMT")


headers.light = c("fix_type","dd_mm_yy","mid_value","mid_value_secs","transition_1","transition_2","stationery_latitude","compensated_latitude","longitude","distance","heading","velocity","confidence")     #names for column headers

headers.activity = c("day.number2", "day.number" , "day.dry.number",	"day.wet.number",	"day.mix.number",	"day.max.wet",	"day.min.wet"	,"day.max.dry",	"day.min.dry",	"day.mean.dry",	"day.med.dry",	"day.mean.wet",	"day.med.wet",	"day.date",	"median.value",	"mean.value",	"median.value.between",	"n.dry",	"day.prop.wet",	"mean.value.day",	"mean.value.night","date.text")


#READ IN DATA
light.data <- read.table(file.in.light, header=F, sep=",", skip=1, col.names= headers.light)
activity.data <- read.table(file.in.activity, header=F, sep=",", skip=1, col.names= headers.activity)

#MAKES DATES A DATE-TIME OBJECT
dates.light.data <- strptime(light.data$dd_mm_yy,format="%d/%m/%Y")
dates.activity.data <-  as.POSIXct(activity.data$day.date*24*60*60,origin="1900-01-01", tz="GMT")

#start.2009 <- as.POSIXct("2009-06-21", tz="GMT") 
end.2010 <- as.POSIXct("2010-06-20", tz="GMT")
start.2010 <-as.POSIXct("2010-06-21", tz="GMT")
#end.2011 <- as.POSIXct("2011-06-20", tz="GMT")

se <- function(x) sqrt(var(x)/length(x))

#names(activity.data)
#bi-weekly averages for light.data
long.mean<-obs <- long.sd<- long.med<- lat.mean<-lat.sd<- lat.med<- week<- date_real<- date_normal <- long.ci <- long.se <- lat.ci <- lat.se <- obs2 <- prop.day.wet.se <- t.val2 <-prop.day.wet.ci <- mean.prop.day.wet <- biweek.mean.value.night <- biweek.mean.value.night.ci<- biweek.mean.value.night.se<- biweek.mean.value.night<-biweek.mean.value.day <- biweek.mean.value.day.ci<- biweek.mean.value.day.se<- biweek.mean.value.day <-    mean.max.dry <-  dry.number <- c(1:26)
#26 weeks in year
i <- 1
x <- start.2009
for(i in 1:26){
  obs[i] <- length(light.data$stationery_latitude[dates.light.data<=(x+(14*24*60*60)) & dates.light.data>=x])
  long.mean[i] <- mean( light.data$longitude[dates.light.data<=(x+(14*24*60*60)) & dates.light.data>=x],na.rm=T)
  long.sd[i] <- sd( light.data$longitude[dates.light.data<=(x+(14*24*60*60)) & dates.light.data>=x],na.rm=T)
  long.med[i] <- median( light.data$longitude[dates.light.data<=(x+(14*24*60*60)) & dates.light.data>=x],na.rm=T)
  t.val <- qt(0.975,obs[i]-1)
  long.se[i] <- se( light.data$longitude[dates.light.data<=(x+(14*24*60*60)) & dates.light.data>=x])
  long.ci[i] <- long.se[i]*t.val
  lat.mean[i] <- mean( light.data$stationery_latitude[dates.light.data<=(x+(14*24*60*60)) & dates.light.data>=x],na.rm=T)
  lat.sd[i] <- sd( light.data$stationery_latitude[dates.light.data<=(x+(14*24*60*60)) & dates.light.data>=x],na.rm=T)
  lat.med[i] <- median( light.data$stationery_latitude[dates.light.data<=(x+(14*24*60*60)) & dates.light.data>=x],na.rm=T)  
  lat.se[i] <- se( light.data$stationery_latitude[dates.light.data<=(x+(14*24*60*60)) & dates.light.data>=x])
  #i <- 200
  mean.prop.day.wet[i] <- mean(activity.data$day.prop.wet[dates.activity.data<=(x+(14*24*60*60)) & dates.activity.data>=x],na.rm = T)
    obs2[i] <- length(activity.data$day.prop.wet[dates.activity.data<=(x+(14*24*60*60)) & dates.activity.data>=x])
    t.val2 <- qt(0.975,obs2[i]-1)
  prop.day.wet.se[i] <- se(activity.data$day.prop.wet[dates.activity.data<=(x+(14*24*60*60)) & dates.activity.data>=x])
  prop.day.wet.ci[i] <- prop.day.wet.se[i]*t.val
   mean.max.dry[i] <-  mean(activity.data$day.max.dry[dates.activity.data<=(x+(14*24*60*60)) & dates.activity.data>=x],na.rm = T)
    biweek.mean.value.night[i] <- mean(activity.data$mean.value.night[dates.activity.data<=(x+(14*24*60*60)) & dates.activity.data>=x],na.rm = T)
  biweek.mean.value.night.se[i] <- se(activity.data$mean.value.night[dates.activity.data<=(x+(14*24*60*60)) & dates.activity.data>=x])
  biweek.mean.value.night.ci[i] <-   biweek.mean.value.night[i]*t.val
  
      biweek.mean.value.day[i] <- mean(activity.data$mean.value.day[dates.activity.data<=(x+(14*24*60*60)) & dates.activity.data>=x],na.rm = T)
  biweek.mean.value.day.se[i] <- se(activity.data$mean.value.day[dates.activity.data<=(x+(14*24*60*60)) & dates.activity.data>=x])
  biweek.mean.value.day.ci[i] <-   biweek.mean.value.day[i]*t.val
  #repeat above bit for daytime - then add these graph panels to bottom to complete an individual summary
  dry.number[i] <- mean(activity.data$day.dry.number[dates.activity.data<=(x+(14*24*60*60)) & dates.activity.data>=x])
  lat.ci[i] <- lat.se[i]*t.val
  week[i] <- i
  date_real[i] <- x+(7*24*60*60)
  date_normal[i] <- x+(7*24*60*60)
x <- x+(14*24*60*60)
  }


light.data.00 <- as.data.frame(cbind(long.mean, long.sd, long.med,long.ci, long.se, lat.mean,lat.sd, lat.med,lat.ci,lat.se, week, date_real, date_normal, obs))



#weekly averages for light.data - year two
long.mean<-obs <- long.sd<- long.med<- lat.mean<-lat.sd<- lat.med<- week<- date_real<- date_normal <- long.ci <- long.se <- lat.ci <- lat.se <- c(1:26)
#26 weeks in year
i <- 1
x <- start.2010
for(i in 1:26){
  obs[i] <- length(light.data$stationery_latitude[dates.light.data<=(x+(14*24*60*60)) & dates.light.data>=x])
  long.mean[i] <- mean( light.data$longitude[dates.light.data<=(x+(14*24*60*60)) & dates.light.data>=x],na.rm=T)
  long.sd[i] <- sd( light.data$longitude[dates.light.data<=(x+(14*24*60*60)) & dates.light.data>=x],na.rm=T)
  long.med[i] <- median( light.data$longitude[dates.light.data<=(x+(14*24*60*60)) & dates.light.data>=x],na.rm=T)
  t.val <- qt(0.975,obs[i]-1)
  long.se[i] <- se( light.data$longitude[dates.light.data<=(x+(14*24*60*60)) & dates.light.data>=x])
  long.ci[i] <- long.se[i]*t.val
  lat.mean[i] <- mean( light.data$stationery_latitude[dates.light.data<=(x+(14*24*60*60)) & dates.light.data>=x],na.rm=T)
  lat.sd[i] <- sd( light.data$stationery_latitude[dates.light.data<=(x+(14*24*60*60)) & dates.light.data>=x],na.rm=T)
  lat.med[i] <- median( light.data$stationery_latitude[dates.light.data<=(x+(14*24*60*60)) & dates.light.data>=x],na.rm=T)  
  lat.se[i] <- se( light.data$stationery_latitude[dates.light.data<=(x+(14*24*60*60)) & dates.light.data>=x])
  lat.ci[i] <- lat.se[i]*t.val
  week[i] <- i
  date_real[i] <- x+(7*24*60*60)
  date_normal[i] <- x-(365*24*60*60)+(7*24*60*60)
x <- x+(14*24*60*60)
  }

light.data.01 <- as.data.frame(cbind(long.mean, long.sd, long.med,long.ci, long.se, lat.mean,lat.sd, lat.med,lat.ci,lat.se, week, date_real, date_normal, obs))















#Big multi-panel graph massive!!
#For averages with CI error lines:
 par(mfrow=c(5,1))
par(mar=c(5,7,0.5,1))

 plot(date_normal,(light.data.00$lat.mean)  ,pch="",las = 1,ylim=c(51,63),col="green",ylab="",xlab="Month",xaxt="n",cex.lab=2,cex.axis=1.7)
 abline(h= 57.289,lty=6, lwd=3, col="gray20")
lines(date_normal,(light.data.00$lat.mean)  ,lwd=2,col="red")
points( date_normal,(light.data.00$lat.mean))
lines(light.data.01$date_normal,(light.data.01$lat.mean) ,lwd=2,col="blue")
points( light.data.01$date_normal,(light.data.01$lat.mean))
 abline(h= 57.289,lty=6, lwd=3, col="gray20")
r <- as.POSIXct(round(range(as.POSIXct( date_normal, origin="1970-01-01")), "days"))
axis.POSIXct(1, at=seq(r[1], r[2], by="month"), format="%b",cex.axis=1.7)
lines( light.data.01$date_normal,((light.data.01$lat.mean-light.data.01$lat.ci)),lwd=2,lty=3,col="blue")
lines( light.data.01$date_normal,((light.data.01$lat.mean+light.data.01$lat.ci)),lwd=2,lty=3,col="blue")
lines( date_normal,((light.data.00$lat.mean-light.data.00$lat.ci)),lwd=2,lty=3,col="red")
lines( date_normal,((light.data.00$lat.mean+light.data.00$lat.ci)),lwd=2,lty=3,col="red")
mtext("Latitude",side=2, line=4.0, cex=1.4)


par(mar=c(5,7,0.5,1))

plot(date_normal,(light.data.00$lat.mean)  ,pch="",las = 1, ylim=c(13.5,22),col="green",ylab="",xlab="Month",xaxt="n",cex.lab=2,cex.axis=1.7)
abline(h= 17.960,lty=6, lwd=3, col="gray20")
lines(  date_normal,(-1*light.data.00$long.mean)   ,lwd=2,col="red")
points( date_normal,(-1*light.data.00$long.mean))
lines(  light.data.01$date_normal,(-1*light.data.01$long.mean)   ,lwd=2,col="blue")
points( light.data.01$date_normal,(-1*light.data.01$long.mean))
r <- as.POSIXct(round(range(as.POSIXct( date_normal, origin="1970-01-01")), "days"))
axis.POSIXct(1, at=seq(r[1], r[2], by="month"), format="%b",cex.axis=1.7)
lines( light.data.01$date_normal,(-1*(light.data.01$long.mean-light.data.01$long.ci)),lwd=2,lty=3,col="blue")
lines( light.data.01$date_normal,(-1*(light.data.01$long.mean+light.data.01$long.ci)),lwd=2,lty=3,col="blue")
lines( date_normal,(-1*(light.data.00$long.mean-light.data.00$long.ci)),lwd=2,lty=3,col="red")
lines( date_normal,(-1*(light.data.00$long.mean+light.data.00$long.ci)),lwd=2,lty=3,col="red")
mtext("Longitude",side=2, line=4.0, cex=1.4)



par(mar=c(5,7,0.5,1))
plot(date_normal,(mean.prop.day.wet)  ,pch="",las = 1, ylim=c(0,1),col="green",ylab="",xlab="Month",xaxt="n",cex.lab=2,cex.axis=1.7)
#abline(h= 17.960,lty=6, lwd=3, col="gray20")
lines(date_normal,(mean.prop.day.wet) ,lwd=2,col="red")
points( date_normal,(mean.prop.day.wet))
lines(  light.data.01$date_normal,(-1*light.data.01$long.mean)   ,lwd=2,col="blue")
#points( light.data.01$date_normal,(-1*light.data.01$long.mean))
r <- as.POSIXct(round(range(as.POSIXct( date_normal, origin="1970-01-01")), "days"))
axis.POSIXct(1, at=seq(r[1], r[2], by="month"), format="%b",cex.axis=1.7)
#lines( light.data.01$date_normal,(mean.prop.day.wet-prop.day.wet.ci),lwd=2,lty=3,col="red")
#lines( light.data.01$date_normal,(mean.prop.day.wet+prop.day.wet.ci),lwd=2,lty=3,col="red")
#lines( date_normal,(-1*(light.data.00$long.mean-prop.day.wet.ci)),lwd=2,lty=3,col="red")
#lines( date_normal,(-1*(light.data.00$long.mean+prop.day.wet.ci)),lwd=2,lty=3,col="red")
mtext("Proportion of",side=2, line=5.5, cex=1.4)
mtext("day wet",side=2, line=4, cex=1.4)
points(dates.activity.data,activity.data$day.prop.wet,col="red")

par(mar=c(5,7,0.5,1))
plot(date_normal,biweek.mean.value.day  ,pch="",las = 1, ylim=c(0,220),col="green",ylab="",xlab="Month",xaxt="n",cex.lab=2,cex.axis=1.7)
#abline(h= 17.960,lty=6, lwd=3, col="gray20")
lines(date_normal,(biweek.mean.value.day) ,lwd=2,col="red")
lines(date_normal,(biweek.mean.value.night) ,lwd=2,col="black")
points( date_normal,(biweek.mean.value.day))
#lines(  light.data.01$date_normal,(-1*light.data.01$long.mean)   ,lwd=2,col="blue")
points( date_normal,(biweek.mean.value.night))
r <- as.POSIXct(round(range(as.POSIXct( date_normal, origin="1970-01-01")), "days"))
axis.POSIXct(1, at=seq(r[1], r[2], by="month"), format="%b",cex.axis=1.7)
#lines(date_normal,  dry.number*3,col="green")
points(dates.activity.data,activity.data$mean.value.day,col="red")
points(dates.activity.data,activity.data$mean.value.night,col="black")
mtext("Mean activity",side=2, line=5.5, cex=1.4)
mtext("value",side=2, line=4, cex=1.4)
#?mtext
#lines( light.data.01$date_normal,(biweek.mean.value.day-biweek.mean.value.day.se),lwd=2,lty=3,col="red")
#lines( light.data.01$date_normal,(biweek.mean.value.day+biweek.mean.value.day.se),lwd=2,lty=3,col="red")
#lines( date_normal,(-1*(light.data.00$long.mean-prop.day.wet.ci)),lwd=2,lty=3,col="black")
#lines( date_normal,(-1*(light.data.00$long.mean+prop.day.wet.ci)),lwd=2,lty=3,col="black")

#biweek.mean.value.day.ci<- biweek.mean.value.day.se<- biweek.mean.value.day

mean.max.dry.new <- c(0,1,2)
#names(activity.data)
for(i in 1:length(mean.max.dry)){
  if(mean.max.dry[i] == -Inf)mean.max.dry.new[i] = 0
 # else if(mean.max.dry[i] == NaN)  mean.max.dry.new[i] = NaN
  else    mean.max.dry.new[i] = mean.max.dry[i]
  }


mean.max.dry.day.new <- c(0,1,2)
#names(activity.data)
for(i in 1:length(activity.data$day.max.dry)){
  if(activity.data$day.max.dry[i] == -Inf)mean.max.dry.day.new[i] = 0
 # else if(mean.max.dry[i] == NaN)  mean.max.dry.new[i] = NaN
  else    mean.max.dry.day.new[i] =activity.data$day.max.dry[i]
  }
#mean.max.dry
par(mar=c(5,7,0.5,1))
#plot(date_normal[1:length(mean.max.dry.new)],mean.max.dry.new*10/60  ,pch="",las = 1, ylim=c(-1,8),col="green",ylab="",xlab="Month",xaxt="n",cex.lab=2,cex.axis=1.7)
plot(date_normal,biweek.mean.value.day  ,pch="",las = 1, ylim=c(0,12),col="green",ylab="",xlab="Month",xaxt="n",cex.lab=2,cex.axis=1.7)
#abline(h= 17.960,lty=6, lwd=3, col="gray20")
lines(date_normal[1:length(mean.max.dry.new)],(mean.max.dry.new*10/60) ,lwd=2,col="red")
#lines(date_normal,(biweek.mean.value.night) ,lwd=2,col="black")
points(date_normal[1:length(mean.max.dry.new)],(mean.max.dry.new*10/60))
#lines(  light.data.01$date_normal,(-1*light.data.01$long.mean)   ,lwd=2,col="blue")
#points( date_normal,(biweek.mean.value.night))
r <- as.POSIXct(round(range(as.POSIXct( date_normal, origin="1970-01-01")), "days"))
axis.POSIXct(1, at=seq(r[1], r[2], by="month"), format="%b",cex.axis=1.7)
points(dates.activity.data,mean.max.dry.day.new*10/60,col="red")
mtext("Mean maximum ",side=2, line=5.5, cex=1.4)
mtext("daily dry period",side=2, line=4, cex=1.4)

