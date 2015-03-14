#SET WORKING DIRECTORY, AND SET VECTOR OF COLUMN NAMES 
setwd("F:/Documents/Work/GLS_DATA/Guillemots/")  #select working directory   #win
 headers = c("fix_type","dd_mm_yy","mid_value","mid_value_secs","transition_1","transition_2","stationery_latitude","compensated_latitude","longitude","distance","heading","velocity","confidence")     #names for column headers

#READ IN DATA
 aak969 <- read.table("F:/Documents/Work/GLS_DATA/Guillemots/2011/AAK969/2011-09-22/GLS12018_2011_06_19_AAK969_000_thresh10_both_0340_all.trj", header=F, sep=",", skip=1, col.names= headers)
 aak970 <- read.table("F:/Documents/Work/GLS_DATA/Guillemots/2011/AAK970/2011-09-21/20110615_aak970_gls_download_000_thresh10_both_0340_all.trj", header=F, sep=",", skip=1, col.names= headers)
 aak968 <- read.table("F:/Documents/Work/GLS_DATA/Guillemots/2011/AAK968/2011-09-22/GLS12017_2011_06_27_AAK968_edited_000_thresh10_both_0340_all.trj", header=F, sep=",", skip=1, col.names= headers)
 aak962 <- read.table("F:/Documents/Work/GLS_DATA/Guillemots/2010/gl12013/gl12013_000_thresh10_both_0340_all.trj", header=F, sep=",", skip=1, col.names= headers)
 aak966 <- read.table("F:/Documents/Work/GLS_DATA/Guillemots/2010/gl12016/gl12016_000_thresh10_both_elev_0340_all.trj", header=F, sep=",", skip=1, col.names= headers)
  cal01 <- read.table("F:/Documents/Work/GLS_DATA/Calibration_data/Calibration_01/time_adjusted/calibration_GLS_01_adj_000_both_0340_all.trj", header=F, sep=",", skip=1, col.names= headers)
  cal02 <- read.table("F:/Documents/Work/GLS_DATA/Calibration_data/Calibration_02/clock_adjustment/calibration_GLS_02_adj_000_both_0340_all.trj", header=F, sep=",", skip=1, col.names= headers) 
                       
#MAKES DATES A DATE-TIME OBJECT
dates.aak969 <- strptime(aak969$dd_mm_yy,format="%d/%m/%Y") 
dates.aak970 <- strptime(aak970$dd_mm_yy,format="%d/%m/%Y") 
dates.aak968 <- strptime(aak968$dd_mm_yy,format="%d/%m/%Y") 
dates.aak962 <- strptime(aak962$dd_mm_yy,format="%d/%m/%Y") 
dates.aak966 <- strptime(aak966$dd_mm_yy,format="%d/%m/%Y") 
dates.cal01 <- strptime(cal01$dd_mm_yy,format="%d/%m/%Y")
dates.cal02 <- strptime(cal02$dd_mm_yy,format="%d/%m/%Y")




start.2009 <- as.POSIXct("2009-06-21", tz="GMT") 
end.2010 <- as.POSIXct("2010-06-20", tz="GMT")
start.2010 <-as.POSIXct("2010-06-21", tz="GMT")
end.2011 <- as.POSIXct("2011-06-20", tz="GMT")

se <- function(x) sqrt(var(x)/length(x))


#weekly averages for aak966
long.mean<-obs <- long.sd<- long.med<- lat.mean<-lat.sd<- lat.med<- week<- date_real<- date_normal <- long.ci <- long.se <- lat.ci <- lat.se <- c(1:26)
#26 weeks in year
i <- 1
x <- start.2009
for(i in 1:26){
  obs[i] <- length(aak966$stationery_latitude[dates.aak966<=(x+(14*24*60*60)) & dates.aak966>=x])
  long.mean[i] <- mean( aak966$longitude[dates.aak966<=(x+(14*24*60*60)) & dates.aak966>=x],na.rm=T)
  long.sd[i] <- sd( aak966$longitude[dates.aak966<=(x+(14*24*60*60)) & dates.aak966>=x],na.rm=T)
  long.med[i] <- median( aak966$longitude[dates.aak966<=(x+(14*24*60*60)) & dates.aak966>=x],na.rm=T)
  t.val <- qt(0.975,obs[i]-1)
  long.se[i] <- se( aak966$longitude[dates.aak966<=(x+(14*24*60*60)) & dates.aak966>=x])
  long.ci[i] <- long.se[i]*t.val
  lat.mean[i] <- mean( aak966$stationery_latitude[dates.aak966<=(x+(14*24*60*60)) & dates.aak966>=x],na.rm=T)
  lat.sd[i] <- sd( aak966$stationery_latitude[dates.aak966<=(x+(14*24*60*60)) & dates.aak966>=x],na.rm=T)
  lat.med[i] <- median( aak966$stationery_latitude[dates.aak966<=(x+(14*24*60*60)) & dates.aak966>=x],na.rm=T)  
  lat.se[i] <- se( aak966$stationery_latitude[dates.aak966<=(x+(14*24*60*60)) & dates.aak966>=x])
  lat.ci[i] <- lat.se[i]*t.val
  week[i] <- i
  date_real[i] <- x+(7*24*60*60)
  date_normal[i] <- x+(7*24*60*60)
x <- x+(14*24*60*60)
  }

aak966.00 <- as.data.frame(cbind(long.mean, long.sd, long.med,long.ci, long.se, lat.mean,lat.sd, lat.med,lat.ci,lat.se, week, date_real, date_normal, obs))


#weekly averages for aak969 - year one
long.mean<-obs <- long.sd<- long.med<- lat.mean<-lat.sd<- lat.med<- week<- date_real<- date_normal <- long.ci <- long.se <- lat.ci <- lat.se <- c(1:26)
#26 weeks in year
i <- 1
x <- start.2009
for(i in 1:26){
  obs[i] <- length(aak969$stationery_latitude[dates.aak969<=(x+(14*24*60*60)) & dates.aak969>=x])
  long.mean[i] <- mean( aak969$longitude[dates.aak969<=(x+(14*24*60*60)) & dates.aak969>=x],na.rm=T)
  long.sd[i] <- sd( aak969$longitude[dates.aak969<=(x+(14*24*60*60)) & dates.aak969>=x],na.rm=T)
  long.med[i] <- median( aak969$longitude[dates.aak969<=(x+(14*24*60*60)) & dates.aak969>=x],na.rm=T)
  t.val <- qt(0.975,obs[i]-1)
  long.se[i] <- se( aak969$longitude[dates.aak969<=(x+(14*24*60*60)) & dates.aak969>=x])
  long.ci[i] <- long.se[i]*t.val
  lat.mean[i] <- mean( aak969$stationery_latitude[dates.aak969<=(x+(14*24*60*60)) & dates.aak969>=x],na.rm=T)
  lat.sd[i] <- sd( aak969$stationery_latitude[dates.aak969<=(x+(14*24*60*60)) & dates.aak969>=x],na.rm=T)
  lat.med[i] <- median( aak969$stationery_latitude[dates.aak969<=(x+(14*24*60*60)) & dates.aak969>=x],na.rm=T)  
  lat.se[i] <- se( aak969$stationery_latitude[dates.aak969<=(x+(14*24*60*60)) & dates.aak969>=x])
  lat.ci[i] <- lat.se[i]*t.val
  week[i] <- i
  date_real[i] <- x+(7*24*60*60)
  date_normal[i] <- x+(7*24*60*60)
x <- x+(14*24*60*60)
  }

aak969.00 <- as.data.frame(cbind(long.mean, long.sd, long.med,long.ci, long.se, lat.mean,lat.sd, lat.med,lat.ci,lat.se, week, date_real, date_normal, obs))


#weekly averages for aak969 - year two
long.mean<-obs <- long.sd<- long.med<- lat.mean<-lat.sd<- lat.med<- week<- date_real<- date_normal <- long.ci <- long.se <- lat.ci <- lat.se <- c(1:26)
#26 weeks in year
i <- 1
x <- start.2010
for(i in 1:26){
  obs[i] <- length(aak969$stationery_latitude[dates.aak969<=(x+(14*24*60*60)) & dates.aak969>=x])
  long.mean[i] <- mean( aak969$longitude[dates.aak969<=(x+(14*24*60*60)) & dates.aak969>=x],na.rm=T)
  long.sd[i] <- sd( aak969$longitude[dates.aak969<=(x+(14*24*60*60)) & dates.aak969>=x],na.rm=T)
  long.med[i] <- median( aak969$longitude[dates.aak969<=(x+(14*24*60*60)) & dates.aak969>=x],na.rm=T)
  t.val <- qt(0.975,obs[i]-1)
  long.se[i] <- se( aak969$longitude[dates.aak969<=(x+(14*24*60*60)) & dates.aak969>=x])
  long.ci[i] <- long.se[i]*t.val
  lat.mean[i] <- mean( aak969$stationery_latitude[dates.aak969<=(x+(14*24*60*60)) & dates.aak969>=x],na.rm=T)
  lat.sd[i] <- sd( aak969$stationery_latitude[dates.aak969<=(x+(14*24*60*60)) & dates.aak969>=x],na.rm=T)
  lat.med[i] <- median( aak969$stationery_latitude[dates.aak969<=(x+(14*24*60*60)) & dates.aak969>=x],na.rm=T)  
  lat.se[i] <- se( aak969$stationery_latitude[dates.aak969<=(x+(14*24*60*60)) & dates.aak969>=x])
  lat.ci[i] <- lat.se[i]*t.val
  week[i] <- i
  date_real[i] <- x+(7*24*60*60)
  date_normal[i] <- x-(362*24*60*60)
x <- x+(14*24*60*60)
  }

aak969.01 <- as.data.frame(cbind(long.mean, long.sd, long.med,long.ci, long.se, lat.mean,lat.sd, lat.med,lat.ci,lat.se, week, date_real, date_normal, obs))



#weekly averages for aak970 - year one
long.mean<-obs <- long.sd<- long.med<- lat.mean<-lat.sd<- lat.med<- week<- date_real<- date_normal <- long.ci <- long.se <- lat.ci <- lat.se <- c(1:26)
#26 weeks in year
i <- 1
x <- start.2009
for(i in 1:26){
  obs[i] <- length(aak970$stationery_latitude[dates.aak970<=(x+(14*24*60*60)) & dates.aak970>=x])
  long.mean[i] <- mean( aak970$longitude[dates.aak970<=(x+(14*24*60*60)) & dates.aak970>=x],na.rm=T)
  long.sd[i] <- sd( aak970$longitude[dates.aak970<=(x+(14*24*60*60)) & dates.aak970>=x],na.rm=T)
  long.med[i] <- median( aak970$longitude[dates.aak970<=(x+(14*24*60*60)) & dates.aak970>=x],na.rm=T)
  t.val <- qt(0.975,obs[i]-1)
  long.se[i] <- se( aak970$longitude[dates.aak970<=(x+(14*24*60*60)) & dates.aak970>=x])
  long.ci[i] <- long.se[i]*t.val
  lat.mean[i] <- mean( aak970$stationery_latitude[dates.aak970<=(x+(14*24*60*60)) & dates.aak970>=x],na.rm=T)
  lat.sd[i] <- sd( aak970$stationery_latitude[dates.aak970<=(x+(14*24*60*60)) & dates.aak970>=x],na.rm=T)
  lat.med[i] <- median( aak970$stationery_latitude[dates.aak970<=(x+(14*24*60*60)) & dates.aak970>=x],na.rm=T)  
  lat.se[i] <- se( aak970$stationery_latitude[dates.aak970<=(x+(14*24*60*60)) & dates.aak970>=x])
  lat.ci[i] <- lat.se[i]*t.val
  week[i] <- i
  date_real[i] <- x+(7*24*60*60)
  date_normal[i] <- x+(7*24*60*60)
x <- x+(14*24*60*60)
  }

aak970.00 <- as.data.frame(cbind(long.mean, long.sd, long.med,long.ci, long.se, lat.mean,lat.sd, lat.med,lat.ci,lat.se, week, date_real, date_normal, obs))


#weekly averages for aak970 - year two
long.mean<-obs <- long.sd<- long.med<- lat.mean<-lat.sd<- lat.med<- week<- date_real<- date_normal <- long.ci <- long.se <- lat.ci <- lat.se <- c(1:26)
#26 weeks in year
i <- 1
x <- start.2010
for(i in 1:26){
  obs[i] <- length(aak970$stationery_latitude[dates.aak970<=(x+(14*24*60*60)) & dates.aak970>=x])
  long.mean[i] <- mean( aak970$longitude[dates.aak970<=(x+(14*24*60*60)) & dates.aak970>=x],na.rm=T)
  long.sd[i] <- sd( aak970$longitude[dates.aak970<=(x+(14*24*60*60)) & dates.aak970>=x],na.rm=T)
  long.med[i] <- median( aak970$longitude[dates.aak970<=(x+(14*24*60*60)) & dates.aak970>=x],na.rm=T)
  t.val <- qt(0.975,obs[i]-1)
  long.se[i] <- se( aak970$longitude[dates.aak970<=(x+(14*24*60*60)) & dates.aak970>=x])
  long.ci[i] <- long.se[i]*t.val
  lat.mean[i] <- mean( aak970$stationery_latitude[dates.aak970<=(x+(14*24*60*60)) & dates.aak970>=x],na.rm=T)
  lat.sd[i] <- sd( aak970$stationery_latitude[dates.aak970<=(x+(14*24*60*60)) & dates.aak970>=x],na.rm=T)
  lat.med[i] <- median( aak970$stationery_latitude[dates.aak970<=(x+(14*24*60*60)) & dates.aak970>=x],na.rm=T)  
  lat.se[i] <- se( aak970$stationery_latitude[dates.aak970<=(x+(14*24*60*60)) & dates.aak970>=x])
  lat.ci[i] <- lat.se[i]*t.val
  week[i] <- i
  date_real[i] <- x+(7*24*60*60)
  date_normal[i] <- x-(362*24*60*60)
x <- x+(14*24*60*60)
  }

aak970.01 <- as.data.frame(cbind(long.mean, long.sd, long.med,long.ci, long.se, lat.mean,lat.sd, lat.med,lat.ci,lat.se, week, date_real, date_normal, obs))




#weekly averages for aak962
long.mean<-obs <- long.sd<- long.med<- lat.mean<-lat.sd<- lat.med<- week<- date_real<- date_normal <- long.ci <- long.se <- lat.ci <- lat.se <- c(1:26)
#26 weeks in year
i <- 1
x <- start.2009
for(i in 1:26){
  obs[i] <- length(aak962$stationery_latitude[dates.aak962<=(x+(14*24*60*60)) & dates.aak962>=x])
  long.mean[i] <- mean( aak962$longitude[dates.aak962<=(x+(14*24*60*60)) & dates.aak962>=x],na.rm=T)
  long.sd[i] <- sd( aak962$longitude[dates.aak962<=(x+(14*24*60*60)) & dates.aak962>=x],na.rm=T)
  long.med[i] <- median( aak962$longitude[dates.aak962<=(x+(14*24*60*60)) & dates.aak962>=x],na.rm=T)
  t.val <- qt(0.975,obs[i]-1)
  long.se[i] <- se( aak962$longitude[dates.aak962<=(x+(14*24*60*60)) & dates.aak962>=x])
  long.ci[i] <- long.se[i]*t.val
  lat.mean[i] <- mean( aak962$stationery_latitude[dates.aak962<=(x+(14*24*60*60)) & dates.aak962>=x],na.rm=T)
  lat.sd[i] <- sd( aak962$stationery_latitude[dates.aak962<=(x+(14*24*60*60)) & dates.aak962>=x],na.rm=T)
  lat.med[i] <- median( aak962$stationery_latitude[dates.aak962<=(x+(14*24*60*60)) & dates.aak962>=x],na.rm=T)  
  lat.se[i] <- se( aak962$stationery_latitude[dates.aak962<=(x+(14*24*60*60)) & dates.aak962>=x])
  lat.ci[i] <- lat.se[i]*t.val
  week[i] <- i
  date_real[i] <- x+(7*24*60*60)
  date_normal[i] <- x+(7*24*60*60)
x <- x+(14*24*60*60)
  }

aak962.00 <- as.data.frame(cbind(long.mean, long.sd, long.med,long.ci, long.se, lat.mean,lat.sd, lat.med,lat.ci,lat.se, week, date_real, date_normal, obs))


#weekly averages for aak968 - year one
long.mean<-obs <- long.sd<- long.med<- lat.mean<-lat.sd<- lat.med<- week<- date_real<- date_normal <- long.ci <- long.se <- lat.ci <- lat.se <- c(1:26)
#26 weeks in year
i <- 1
x <- start.2009
for(i in 1:26){
  obs[i] <- length(aak968$stationery_latitude[dates.aak968<=(x+(14*24*60*60)) & dates.aak968>=x])
  long.mean[i] <- mean( aak968$longitude[dates.aak968<=(x+(14*24*60*60)) & dates.aak968>=x],na.rm=T)
  long.sd[i] <- sd( aak968$longitude[dates.aak968<=(x+(14*24*60*60)) & dates.aak968>=x],na.rm=T)
  long.med[i] <- median( aak968$longitude[dates.aak968<=(x+(14*24*60*60)) & dates.aak968>=x],na.rm=T)
  t.val <- qt(0.975,obs[i]-1)
  long.se[i] <- se( aak968$longitude[dates.aak968<=(x+(14*24*60*60)) & dates.aak968>=x])
  long.ci[i] <- long.se[i]*t.val
  lat.mean[i] <- mean( aak968$stationery_latitude[dates.aak968<=(x+(14*24*60*60)) & dates.aak968>=x],na.rm=T)
  lat.sd[i] <- sd( aak968$stationery_latitude[dates.aak968<=(x+(14*24*60*60)) & dates.aak968>=x],na.rm=T)
  lat.med[i] <- median( aak968$stationery_latitude[dates.aak968<=(x+(14*24*60*60)) & dates.aak968>=x],na.rm=T)  
  lat.se[i] <- se( aak968$stationery_latitude[dates.aak968<=(x+(14*24*60*60)) & dates.aak968>=x])
  lat.ci[i] <- lat.se[i]*t.val
  week[i] <- i
  date_real[i] <- x+(7*24*60*60)
  date_normal[i] <- x+(7*24*60*60)
x <- x+(14*24*60*60)
  }

aak968.00 <- as.data.frame(cbind(long.mean, long.sd, long.med,long.ci, long.se, lat.mean,lat.sd, lat.med,lat.ci,lat.se, week, date_real, date_normal, obs))


#weekly averages for aak968 - year two
long.mean<-obs <- long.sd<- long.med<- lat.mean<-lat.sd<- lat.med<- week<- date_real<- date_normal <- long.ci <- long.se <- lat.ci <- lat.se <- c(1:26)
#26 weeks in year
i <- 1
x <- start.2010
for(i in 1:26){
  obs[i] <- length(aak968$stationery_latitude[dates.aak968<=(x+(14*24*60*60)) & dates.aak968>=x])
  long.mean[i] <- mean( aak968$longitude[dates.aak968<=(x+(14*24*60*60)) & dates.aak968>=x],na.rm=T)
  long.sd[i] <- sd( aak968$longitude[dates.aak968<=(x+(14*24*60*60)) & dates.aak968>=x],na.rm=T)
  long.med[i] <- median( aak968$longitude[dates.aak968<=(x+(14*24*60*60)) & dates.aak968>=x],na.rm=T)
  t.val <- qt(0.975,obs[i]-1)
  long.se[i] <- se( aak968$longitude[dates.aak968<=(x+(14*24*60*60)) & dates.aak968>=x])
  long.ci[i] <- long.se[i]*t.val
  lat.mean[i] <- mean( aak968$stationery_latitude[dates.aak968<=(x+(14*24*60*60)) & dates.aak968>=x],na.rm=T)
  lat.sd[i] <- sd( aak968$stationery_latitude[dates.aak968<=(x+(14*24*60*60)) & dates.aak968>=x],na.rm=T)
  lat.med[i] <- median( aak968$stationery_latitude[dates.aak968<=(x+(14*24*60*60)) & dates.aak968>=x],na.rm=T)  
  lat.se[i] <- se( aak968$stationery_latitude[dates.aak968<=(x+(14*24*60*60)) & dates.aak968>=x])
  lat.ci[i] <- lat.se[i]*t.val
  week[i] <- i
  date_real[i] <- x+(7*24*60*60)
  date_normal[i] <- x-(362*24*60*60)
x <- x+(14*24*60*60)
  }

aak968.01 <- as.data.frame(cbind(long.mean, long.sd, long.med,long.ci, long.se, lat.mean,lat.sd, lat.med,lat.ci,lat.se, week, date_real, date_normal, obs))



#weekly averages for cal01 - year one
long.mean<-obs <- long.sd<- long.med<- lat.mean<-lat.sd<- lat.med<- week<- date_real<- date_normal <- long.ci <- long.se <- lat.ci <- lat.se <- c(1:26)
#26 weeks in year
i <- 1
x <- start.2009
for(i in 1:26){
  obs[i] <- length(cal01$stationery_latitude[dates.cal01<=(x+(14*24*60*60)) & dates.cal01>=x])
  long.mean[i] <- mean( cal01$longitude[dates.cal01<=(x+(14*24*60*60)) & dates.cal01>=x],na.rm=T)
  long.sd[i] <- sd( cal01$longitude[dates.cal01<=(x+(14*24*60*60)) & dates.cal01>=x],na.rm=T)
  long.med[i] <- median( cal01$longitude[dates.cal01<=(x+(14*24*60*60)) & dates.cal01>=x],na.rm=T)
  t.val <- qt(0.975,obs[i]-1)
  long.se[i] <- se( cal01$longitude[dates.cal01<=(x+(14*24*60*60)) & dates.cal01>=x])
  long.ci[i] <- long.se[i]*t.val
  lat.mean[i] <- mean( cal01$stationery_latitude[dates.cal01<=(x+(14*24*60*60)) & dates.cal01>=x],na.rm=T)
  lat.sd[i] <- sd( cal01$stationery_latitude[dates.cal01<=(x+(14*24*60*60)) & dates.cal01>=x],na.rm=T)
  lat.med[i] <- median( cal01$stationery_latitude[dates.cal01<=(x+(14*24*60*60)) & dates.cal01>=x],na.rm=T)  
  lat.se[i] <- se( cal01$stationery_latitude[dates.cal01<=(x+(14*24*60*60)) & dates.cal01>=x])
  lat.ci[i] <- lat.se[i]*t.val
  week[i] <- i
  date_real[i] <- x+(7*24*60*60)
  date_normal[i] <- x+(7*24*60*60)
x <- x+(14*24*60*60)
  }

cal01.00 <- as.data.frame(cbind(long.mean, long.sd, long.med,long.ci, long.se, lat.mean,lat.sd, lat.med,lat.ci,lat.se, week, date_real, date_normal, obs))


#weekly averages for cal01 - year two
long.mean<-obs <- long.sd<- long.med<- lat.mean<-lat.sd<- lat.med<- week<- date_real<- date_normal <- long.ci <- long.se <- lat.ci <- lat.se <- c(1:26)
#26 weeks in year
i <- 1
x <- start.2010
for(i in 1:26){
  obs[i] <- length(cal01$stationery_latitude[dates.cal01<=(x+(14*24*60*60)) & dates.cal01>=x])
  long.mean[i] <- mean( cal01$longitude[dates.cal01<=(x+(14*24*60*60)) & dates.cal01>=x],na.rm=T)
  long.sd[i] <- sd( cal01$longitude[dates.cal01<=(x+(14*24*60*60)) & dates.cal01>=x],na.rm=T)
  long.med[i] <- median( cal01$longitude[dates.cal01<=(x+(14*24*60*60)) & dates.cal01>=x],na.rm=T)
  t.val <- qt(0.975,obs[i]-1)
  long.se[i] <- se( cal01$longitude[dates.cal01<=(x+(14*24*60*60)) & dates.cal01>=x])
  long.ci[i] <- long.se[i]*t.val
  lat.mean[i] <- mean( cal01$stationery_latitude[dates.cal01<=(x+(14*24*60*60)) & dates.cal01>=x],na.rm=T)
  lat.sd[i] <- sd( cal01$stationery_latitude[dates.cal01<=(x+(14*24*60*60)) & dates.cal01>=x],na.rm=T)
  lat.med[i] <- median( cal01$stationery_latitude[dates.cal01<=(x+(14*24*60*60)) & dates.cal01>=x],na.rm=T)  
  lat.se[i] <- se( cal01$stationery_latitude[dates.cal01<=(x+(14*24*60*60)) & dates.cal01>=x])
  lat.ci[i] <- lat.se[i]*t.val
  week[i] <- i
  date_real[i] <- x+(7*24*60*60)
  date_normal[i] <- x-(362*24*60*60)
x <- x+(14*24*60*60)
  }

cal01.01 <- as.data.frame(cbind(long.mean, long.sd, long.med,long.ci, long.se, lat.mean,lat.sd, lat.med,lat.ci,lat.se, week, date_real, date_normal, obs))



#weekly averages for cal02 - year one
long.mean<-obs <- long.sd<- long.med<- lat.mean<-lat.sd<- lat.med<- week<- date_real<- date_normal <- long.ci <- long.se <- lat.ci <- lat.se <- c(1:26)
#26 weeks in year
i <- 1
x <- start.2009
for(i in 1:26){
  obs[i] <- length(cal02$stationery_latitude[dates.cal02<=(x+(14*24*60*60)) & dates.cal02>=x])
  long.mean[i] <- mean( cal02$longitude[dates.cal02<=(x+(14*24*60*60)) & dates.cal02>=x],na.rm=T)
  long.sd[i] <- sd( cal02$longitude[dates.cal02<=(x+(14*24*60*60)) & dates.cal02>=x],na.rm=T)
  long.med[i] <- median( cal02$longitude[dates.cal02<=(x+(14*24*60*60)) & dates.cal02>=x],na.rm=T)
  t.val <- qt(0.975,obs[i]-1)
  long.se[i] <- se( cal02$longitude[dates.cal02<=(x+(14*24*60*60)) & dates.cal02>=x])
  long.ci[i] <- long.se[i]*t.val
  lat.mean[i] <- mean( cal02$stationery_latitude[dates.cal02<=(x+(14*24*60*60)) & dates.cal02>=x],na.rm=T)
  lat.sd[i] <- sd( cal02$stationery_latitude[dates.cal02<=(x+(14*24*60*60)) & dates.cal02>=x],na.rm=T)
  lat.med[i] <- median( cal02$stationery_latitude[dates.cal02<=(x+(14*24*60*60)) & dates.cal02>=x],na.rm=T)  
  lat.se[i] <- se( cal02$stationery_latitude[dates.cal02<=(x+(14*24*60*60)) & dates.cal02>=x])
  lat.ci[i] <- lat.se[i]*t.val
  week[i] <- i
  date_real[i] <- x+(7*24*60*60)
  date_normal[i] <- x+(7*24*60*60)
x <- x+(14*24*60*60)
  }

cal02.00 <- as.data.frame(cbind(long.mean, long.sd, long.med,long.ci, long.se, lat.mean,lat.sd, lat.med,lat.ci,lat.se, week, date_real, date_normal, obs))


#weekly averages for cal02 - year two
long.mean<-obs <- long.sd<- long.med<- lat.mean<-lat.sd<- lat.med<- week<- date_real<- date_normal <- long.ci <- long.se <- lat.ci <- lat.se <- c(1:26)
#26 weeks in year
i <- 1
x <- start.2010
for(i in 1:26){
  obs[i] <- length(cal02$stationery_latitude[dates.cal02<=(x+(14*24*60*60)) & dates.cal02>=x])
  long.mean[i] <- mean( cal02$longitude[dates.cal02<=(x+(14*24*60*60)) & dates.cal02>=x],na.rm=T)
  long.sd[i] <- sd( cal02$longitude[dates.cal02<=(x+(14*24*60*60)) & dates.cal02>=x],na.rm=T)
  long.med[i] <- median( cal02$longitude[dates.cal02<=(x+(14*24*60*60)) & dates.cal02>=x],na.rm=T)
  t.val <- qt(0.975,obs[i]-1)
  long.se[i] <- se( cal02$longitude[dates.cal02<=(x+(14*24*60*60)) & dates.cal02>=x])
  long.ci[i] <- long.se[i]*t.val
  lat.mean[i] <- mean( cal02$stationery_latitude[dates.cal02<=(x+(14*24*60*60)) & dates.cal02>=x],na.rm=T)
  lat.sd[i] <- sd( cal02$stationery_latitude[dates.cal02<=(x+(14*24*60*60)) & dates.cal02>=x],na.rm=T)
  lat.med[i] <- median( cal02$stationery_latitude[dates.cal02<=(x+(14*24*60*60)) & dates.cal02>=x],na.rm=T)  
  lat.se[i] <- se( cal02$stationery_latitude[dates.cal02<=(x+(14*24*60*60)) & dates.cal02>=x])
  lat.ci[i] <- lat.se[i]*t.val
  week[i] <- i
  date_real[i] <- x+(7*24*60*60)
  date_normal[i] <- x-(362*24*60*60)
x <- x+(14*24*60*60)
  }

cal02.01 <- as.data.frame(cbind(long.mean, long.sd, long.med,long.ci, long.se, lat.mean,lat.sd, lat.med,lat.ci,lat.se, week, date_real, date_normal, obs))

#Graphs

#For averages with SD error lines:
 par(mfrow=c(2,1))
par(mar=c(5,6,0.5,1))

 plot(aak969.01$date_normal,(aak969.01$lat.mean)  ,pch="",las = 1,ylim=c(51,63),col="green",ylab="Latitude",xlab="Month",xaxt="n",cex.lab=2,cex.axis=1.7)
 abline(h= 57.289,lty=6, lwd=3, col="gray20")
lines(aak969.01$date_normal,(aak969.01$lat.mean)  ,lwd=2,col="red")
points( aak969.01$date_normal,(aak969.01$lat.mean))
lines(aak969.00$date_normal,(aak969.00$lat.mean) ,lwd=2,col="blue")
points( aak969.00$date_normal,(aak969.00$lat.mean))
 abline(h= 57.289,lty=6, lwd=3, col="gray20")
r <- as.POSIXct(round(range(as.POSIXct( aak969.01$date_normal, origin="1970-01-01")), "days"))
axis.POSIXct(1, at=seq(r[1], r[2], by="month"), format="%b",cex.axis=1.7)
lines( aak969.00$date_normal,((aak969.00$lat.mean-aak969.00$lat.ci)),lwd=2,lty=3,col="blue")
lines( aak969.00$date_normal,((aak969.00$lat.mean+aak969.00$lat.ci)),lwd=2,lty=3,col="blue")
lines( aak969.01$date_normal,((aak969.01$lat.mean-aak969.01$lat.ci)),lwd=2,lty=3,col="red")
lines( aak969.01$date_normal,((aak969.01$lat.mean+aak969.01$lat.ci)),lwd=2,lty=3,col="red")

par(mar=c(5,6,0.5,1))

plot(aak969.01$date_normal,(aak969.01$lat.mean)  ,pch="",las = 1, ylim=c(13.5,22),col="green",ylab="Longitude",xlab="Month",xaxt="n",cex.lab=2,cex.axis=1.7)
abline(h= 17.960,lty=6, lwd=3, col="gray20")
lines(lowess( aak969.01$date_normal,(-1*aak969.01$long.mean)  ,f=1/20),lwd=2,col="red")
points( aak969.01$date_normal,(-1*aak969.01$long.mean))
lines(lowess( aak969.00$date_normal,(-1*aak969.00$long.mean)  ,f=1/20),lwd=2,col="blue")
points( aak969.00$date_normal,(-1*aak969.00$long.mean))
r <- as.POSIXct(round(range(as.POSIXct( aak969.01$date_normal, origin="1970-01-01")), "days"))
axis.POSIXct(1, at=seq(r[1], r[2], by="month"), format="%b",cex.axis=1.7)
lines( aak969.00$date_normal,(-1*(aak969.00$long.mean-aak969.00$long.ci)),lwd=2,lty=3,col="blue")
lines( aak969.00$date_normal,(-1*(aak969.00$long.mean+aak969.00$long.ci)),lwd=2,lty=3,col="blue")
lines( aak969.01$date_normal,(-1*(aak969.01$long.mean-aak969.01$long.ci)),lwd=2,lty=3,col="red")
lines( aak969.01$date_normal,(-1*(aak969.01$long.mean+aak969.01$long.ci)),lwd=2,lty=3,col="red")


#For all


#plotting graphs to display all the data
 par(mfrow=c(2,1))
 plot(aak966.00$date_normal,(aak966.00$lat.mean)  ,pch="",las = 1, ylim=c(51,63),col="green",ylab="Latitude",xlab="Month",xaxt="n",cex.lab=2,cex.axis=1.7)
lines(aak966.00$date_normal,(aak966.00$lat.mean),lwd=2,col="red")
lines( aak969.00$date_normal,(aak969.00$lat.mean)  ,lwd=2,col="blue")
lines( aak969.01$date_normal,(aak969.01$lat.mean)  ,lwd=2,col="blue",lty=3)
lines(aak970.00$date_normal,(aak970.00$lat.mean),lwd=2,col="green")
lines( aak970.01$date_normal,(aak970.00$lat.mean)  ,lwd=2,col="green",lty=3)
lines(aak968.00$date_normal,(aak968.00$lat.mean),lwd=2,col="black")
lines( aak968.01$date_normal,(aak968.01$lat.mean)  ,lwd=2,col="black",lty=3)
lines(aak962.00$date_normal,(aak962.00$lat.mean),lwd=2,col="orange")
lines(cal01.00$date_normal,(cal01.00$lat.mean),lwd=2,col="purple")
lines( cal01.01$date_normal,(cal01.01$lat.mean)  ,lwd=2,col="purple",lty=3)
lines(cal02.00$date_normal,(cal02.00$lat.mean),lwd=2,col="pink")
lines( cal02.01$date_normal,(cal02.01$lat.mean)  ,lwd=2,col="pink",lty=3)
 abline(h= 57.289,lty=6, lwd=3, col="gray20")
r <- as.POSIXct(round(range(as.POSIXct( aak969.01$date_normal, origin="1970-01-01")), "days"))
axis.POSIXct(1, at=seq(r[1], r[2], by="month"), format="%b",cex.axis=1.7)

#plotting graphs to display all the data
 plot(aak966.00$date_normal,(-aak966.00$long.mean)  ,pch="",las = 1, ylim=c(13.5,22),col="green",ylab="longitude",xlab="Month",xaxt="n",cex.lab=2,cex.axis=1.7)
lines(aak966.00$date_normal,(-aak966.00$long.mean),lwd=2,col="red")
lines( aak969.00$date_normal,(-aak969.00$long.mean)  ,lwd=2,col="blue")
lines( aak969.01$date_normal,(-aak969.01$long.mean)  ,lwd=2,col="blue",lty=3)
lines(aak970.00$date_normal,(-aak970.00$long.mean),lwd=2,col="green")
lines( aak970.01$date_normal,(-aak970.00$long.mean)  ,lwd=2,col="green",lty=3)
lines(aak968.00$date_normal,(-aak968.00$long.mean),lwd=2,col="black")
lines( aak968.01$date_normal,(-aak968.01$long.mean)  ,lwd=2,col="black",lty=3)
lines(aak962.00$date_normal,(-aak962.00$long.mean),lwd=2,col="orange")
lines(cal01.00$date_normal,(-cal01.00$long.mean),lwd=2,col="purple")
lines( cal01.01$date_normal,(-cal01.01$long.mean)  ,lwd=2,col="purple",lty=3)
lines(cal02.00$date_normal,(-cal02.00$long.mean),lwd=2,col="pink")
lines( cal02.01$date_normal,(-cal02.01$long.mean)  ,lwd=2,col="pink",lty=3)
 abline(h= 17.960,lty=6, lwd=3, col="gray20")
r <- as.POSIXct(round(range(as.POSIXct( aak969.01$date_normal, origin="1970-01-01")), "days"))
axis.POSIXct(1, at=seq(r[1], r[2], by="month"), format="%b",cex.axis=1.7)