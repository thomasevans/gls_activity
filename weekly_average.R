#SET WORKING DIRECTORY, AND SET VECTOR OF COLUMN NAMES 
setwd("F:/Documents/Work/GLS_DATA/Guillemots/")  #select working directory   #win
 headers = c("fix_type","dd_mm_yy","mid_value","mid_value_secs","transition_1","transition_2","stationery_latitude","compensated_latitude","longitude","distance","heading","velocity","confidence")     #names for column headers

#READ IN DATA
 aak969 <- read.table("F:/Documents/Work/GLS_DATA/Guillemots/2011/AAK969/2011-09-22/GLS12018_2011_06_19_AAK969_000_thresh10_both_0370_all.trj", header=F, sep=",", skip=1, col.names= headers)
 aak970 <- read.table("F:/Documents/Work/GLS_DATA/Guillemots/2011/AAK970/2011-09-21/20110615_aak970_gls_download_000_thresh10_both_0370_all.trj", header=F, sep=",", skip=1, col.names= headers)
 aak968 <- read.table("F:/Documents/Work/GLS_DATA/Guillemots/2011/AAK968/2011-09-22/GLS12017_2011_06_27_AAK968_edited_000_thresh10_both_0370_all.trj", header=F, sep=",", skip=1, col.names= headers)
 aak962 <- read.table("F:/Documents/Work/GLS_DATA/Guillemots/2010/gl12013/gl12013_000_thresh10_both_0370_all.trj", header=F, sep=",", skip=1, col.names= headers)
 aak966 <- read.table("F:/Documents/Work/GLS_DATA/Guillemots/2010/gl12016/gl12016_000_thresh10_both_elev_0370_all.trj", header=F, sep=",", skip=1, col.names= headers)
  cal01 <- read.table("F:/Documents/Work/GLS_DATA/Calibration_data/Calibration_01/time_adjusted/calibration_GLS_01_adj_000_both_0370_all.trj", header=F, sep=",", skip=1, col.names= headers)
  cal02 <- read.table("F:/Documents/Work/GLS_DATA/Calibration_data/Calibration_02/clock_adjustment/calibration_GLS_02_adj_000_both_0370_all.trj", header=F, sep=",", skip=1, col.names= headers) 
                       
#MAKES DATES A DATE-TIME OBJECT
dates.aak969 <- strptime(aak969$dd_mm_yy,format="%d/%m/%Y") 
dates.aak970 <- strptime(aak970$dd_mm_yy,format="%d/%m/%Y") 
dates.aak968 <- strptime(aak968$dd_mm_yy,format="%d/%m/%Y") 
dates.aak962 <- strptime(aak962$dd_mm_yy,format="%d/%m/%Y") 
dates.aak966 <- strptime(aak966$dd_mm_yy,format="%d/%m/%Y") 
dates.cal01 <- strptime(cal01$dd_mm_yy,format="%d/%m/%Y")
dates.cal02 <- strptime(cal02$dd_mm_yy,format="%d/%m/%Y")
 
#SEPPERATE TWO YEARS OF DATA
aak966_2009_10 <- aak966[dates.aak966<=max(dates.aak966),]
aak969_2009_10 <- aak969[dates.aak969<=max(dates.aak966),]
aak969_2010_11 <- aak969[dates.aak969>=max(dates.aak966),]
aak970_2009_10 <- aak970[dates.aak970<=max(dates.aak966),]
aak970_2010_11 <- aak970[dates.aak970>=max(dates.aak966),]
aak968_2009_10 <- aak968[dates.aak968<=max(dates.aak966),]
aak968_2010_11 <- aak968[dates.aak968>=max(dates.aak966),]
aak962_2009_10 <- aak962[dates.aak962<=max(dates.aak966),]
cal01_2009_10 <- cal01[dates.cal01<=max(dates.aak966),]
cal01_2010_11 <- cal01[dates.cal01>=max(dates.aak966),]
cal02_2009_10 <- cal02[dates.cal02<=max(dates.aak966),]
cal02_2010_11 <- cal02[dates.cal02>=max(dates.aak966),]


#A NEW VECTOR OF COLUMN NAMES TO INCLUDE DATE NORMALISED TO BASE YEAR (2009/10)
headers2 <- c(headers, "trans_date") 

#FOR EACH YEAR OF DATA ADD NORMALISED DATE AND NEW HEADERS
aak966_2009_10 <- cbind( aak966_2009_10, (strptime(aak966_2009_10$dd_mm_yy,format="%d/%m/%Y")))
names(aak966_2009_10) <- headers2

aak969_2009_10 <- cbind( aak969_2009_10, (strptime(aak969_2009_10$dd_mm_yy,format="%d/%m/%Y")))
names(aak969_2009_10) <- headers2
aak969_2010_11 <- cbind( aak969_2010_11, (strptime(aak969_2010_11$dd_mm_yy,format="%d/%m/%Y")-24*60*60*365))
names(aak969_2010_11) <- headers2 
 
aak970_2009_10 <- cbind( aak970_2009_10, (strptime(aak970_2009_10$dd_mm_yy,format="%d/%m/%Y")))
names(aak970_2009_10) <- headers2
aak970_2010_11 <- cbind( aak970_2010_11, (strptime(aak970_2010_11$dd_mm_yy,format="%d/%m/%Y")-24*60*60*365))
names(aak970_2010_11) <- headers2 
 
aak968_2009_10 <- cbind( aak968_2009_10, (strptime(aak968_2009_10$dd_mm_yy,format="%d/%m/%Y")))
names(aak968_2009_10) <- headers2
aak968_2010_11 <- cbind( aak968_2010_11, (strptime(aak968_2010_11$dd_mm_yy,format="%d/%m/%Y")-24*60*60*365))
names(aak968_2010_11) <- headers2
 
aak962_2009_10 <- cbind( aak962_2009_10, (strptime(aak962_2009_10$dd_mm_yy,format="%d/%m/%Y")))
names(aak962_2009_10) <- headers2 

cal01_2009_10 <- cbind( cal01_2009_10, (strptime(cal01_2009_10$dd_mm_yy,format="%d/%m/%Y")))
names(cal01_2009_10) <- headers2
cal01_2010_11 <- cbind( cal01_2010_11, (strptime(cal01_2010_11$dd_mm_yy,format="%d/%m/%Y")-24*60*60*365))
names(cal01_2010_11) <- headers2
 
cal02_2009_10 <- cbind( cal02_2009_10, (strptime(cal02_2009_10$dd_mm_yy,format="%d/%m/%Y")))
names(cal02_2009_10) <- headers2
cal02_2010_11 <- cbind( cal02_2010_11, (strptime(cal02_2010_11$dd_mm_yy,format="%d/%m/%Y")-24*60*60*365))
names(cal02_2010_11) <- headers2 
#END, MAKE ONE YEAR DATA OBJECTS


start.2009 <- as.POSIXct("2009-06-21", tz="GMT") 
end.2010 <- as.POSIXct("2010-06-20", tz="GMT")
start.2010 <-as.POSIXct("2010-06-21", tz="GMT")
end.2011 <- as.POSIXct("2011-06-20", tz="GMT")




#weekly averages for aak966
long.mean<- long.sd<- long.med<- lat.mean<-lat.sd<- lat.med<- week<- date_real<- date_normal <- c(1:52)
#52 weeks in year
i <- 1
x <- start.2009
for(i in 1:52){
  long.mean[i] <- mean( aak966$longitude[dates.aak966<=(x+(6*24*60*60)) & dates.aak966>=x],na.rm=T)
  long.sd[i] <- sd( aak966$longitude[dates.aak966<=(x+(6*24*60*60)) & dates.aak966>=x],na.rm=T)
  long.med[i] <- median( aak966$longitude[dates.aak966<=(x+(6*24*60*60)) & dates.aak966>=x],na.rm=T)
  lat.mean[i] <- mean( aak966$stationery_latitude[dates.aak966<=(x+(6*24*60*60)) & dates.aak966>=x],na.rm=T)
  lat.sd[i] <- sd( aak966$stationery_latitude[dates.aak966<=(x+(6*24*60*60)) & dates.aak966>=x],na.rm=T)
  lat.med[i] <- median( aak966$stationery_latitude[dates.aak966<=(x+(6*24*60*60)) & dates.aak966>=x],na.rm=T)  
  week[i] <- i
  date_real[i] <- x+(3*24*60*60)
  date_normal[i] <- x+(3*24*60*60)
x <- x+(7*24*60*60)
  }

aak966.00 <- as.data.frame(cbind(long.mean, long.sd, long.med, lat.mean,lat.sd, lat.med, week, date_real, date_normal))


#weekly averages for aak969 - year one
long.mean<- long.sd<- long.med<- lat.mean<-lat.sd<- lat.med<- week<- date_real<- date_normal <- c(1:52)
#52 weeks in year
i <- 1
x <- start.2009
for(i in 1:52){
  long.mean[i] <- mean( aak969$longitude[dates.aak969<=(x+(6*24*60*60)) & dates.aak969>=x],na.rm=T)
  long.sd[i] <- sd( aak969$longitude[dates.aak969<=(x+(6*24*60*60)) & dates.aak969>=x],na.rm=T)
  long.med[i] <- median( aak969$longitude[dates.aak969<=(x+(6*24*60*60)) & dates.aak969>=x],na.rm=T)
  lat.mean[i] <- mean( aak969$stationery_latitude[dates.aak969<=(x+(6*24*60*60)) & dates.aak969>=x],na.rm=T)
  lat.sd[i] <- sd( aak969$stationery_latitude[dates.aak969<=(x+(6*24*60*60)) & dates.aak969>=x],na.rm=T)
  lat.med[i] <- median( aak969$stationery_latitude[dates.aak969<=(x+(6*24*60*60)) & dates.aak969>=x],na.rm=T)  
  week[i] <- i
  date_real[i] <- x+(3*24*60*60)
  date_normal[i] <- x+(3*24*60*60)
x <- x+(7*24*60*60)
  }

aak969.00 <- as.data.frame(cbind(long.mean, long.sd, long.med, lat.mean,lat.sd, lat.med, week, date_real, date_normal))


#weekly averages for aak969 - year two
long.mean<- long.sd<- long.med<- lat.mean<-lat.sd<- lat.med<- week<- date_real<- date_normal <- c(1:52)
#52 weeks in year
i <- 1
x <- start.2010
for(i in 1:52){
  long.mean[i] <- mean( aak969$longitude[dates.aak969<=(x+(6*24*60*60)) & dates.aak969>=x],na.rm=T)
  long.sd[i] <- sd( aak969$longitude[dates.aak969<=(x+(6*24*60*60)) & dates.aak969>=x],na.rm=T)
  long.med[i] <- median( aak969$longitude[dates.aak969<=(x+(6*24*60*60)) & dates.aak969>=x],na.rm=T)
  lat.mean[i] <- mean( aak969$stationery_latitude[dates.aak969<=(x+(6*24*60*60)) & dates.aak969>=x],na.rm=T)
  lat.sd[i] <- sd( aak969$stationery_latitude[dates.aak969<=(x+(6*24*60*60)) & dates.aak969>=x],na.rm=T)
  lat.med[i] <- median( aak969$stationery_latitude[dates.aak969<=(x+(6*24*60*60)) & dates.aak969>=x],na.rm=T)  
  week[i] <- i
  date_real[i] <- x+(3*24*60*60)
  date_normal[i] <- x-(362*24*60*60)
x <- x+(7*24*60*60)
  }

aak969.01 <- as.data.frame(cbind(long.mean, long.sd, long.med, lat.mean,lat.sd, lat.med, week, date_real, date_normal))


#weekly averages for aak970 - year one**************
long.mean<- long.sd<- long.med<- lat.mean<-lat.sd<- lat.med<- week<- date_real<- date_normal <- c(1:52)
#52 weeks in year
i <- 1
x <- start.2009
for(i in 1:52){
  long.mean[i] <- mean( aak970$longitude[dates.aak970<=(x+(6*24*60*60)) & dates.aak970>=x],na.rm=T)
  long.sd[i] <- sd( aak970$longitude[dates.aak970<=(x+(6*24*60*60)) & dates.aak970>=x],na.rm=T)
  long.med[i] <- median( aak970$longitude[dates.aak970<=(x+(6*24*60*60)) & dates.aak970>=x],na.rm=T)
  lat.mean[i] <- mean( aak970$stationery_latitude[dates.aak970<=(x+(6*24*60*60)) & dates.aak970>=x],na.rm=T)
  lat.sd[i] <- sd( aak970$stationery_latitude[dates.aak970<=(x+(6*24*60*60)) & dates.aak970>=x],na.rm=T)
  lat.med[i] <- median( aak970$stationery_latitude[dates.aak970<=(x+(6*24*60*60)) & dates.aak970>=x],na.rm=T)  
  week[i] <- i
  date_real[i] <- x+(3*24*60*60)
  date_normal[i] <- x+(3*24*60*60)
x <- x+(7*24*60*60)
  }

aak970.00 <- as.data.frame(cbind(long.mean, long.sd, long.med, lat.mean,lat.sd, lat.med, week, date_real, date_normal))


#weekly averages for aak970 - year two**********************
long.mean<- long.sd<- long.med<- lat.mean<-lat.sd<- lat.med<- week<- date_real<- date_normal <- c(1:52)
#52 weeks in year
i <- 1
x <- start.2010
for(i in 1:52){
  long.mean[i] <- mean( aak970$longitude[dates.aak970<=(x+(6*24*60*60)) & dates.aak970>=x],na.rm=T)
  long.sd[i] <- sd( aak970$longitude[dates.aak970<=(x+(6*24*60*60)) & dates.aak970>=x],na.rm=T)
  long.med[i] <- median( aak970$longitude[dates.aak970<=(x+(6*24*60*60)) & dates.aak970>=x],na.rm=T)
  lat.mean[i] <- mean( aak970$stationery_latitude[dates.aak970<=(x+(6*24*60*60)) & dates.aak970>=x],na.rm=T)
  lat.sd[i] <- sd( aak970$stationery_latitude[dates.aak970<=(x+(6*24*60*60)) & dates.aak970>=x],na.rm=T)
  lat.med[i] <- median( aak970$stationery_latitude[dates.aak970<=(x+(6*24*60*60)) & dates.aak970>=x],na.rm=T)  
  week[i] <- i
  date_real[i] <- x+(3*24*60*60)
  date_normal[i] <- x-(362*24*60*60)
x <- x+(7*24*60*60)
  }

aak970.01 <- as.data.frame(cbind(long.mean, long.sd, long.med, lat.mean,lat.sd, lat.med, week, date_real, date_normal))



#weekly averages for aak968 - year one
long.mean<- long.sd<- long.med<- lat.mean<-lat.sd<- lat.med<- week<- date_real<- date_normal <- c(1:52)
#52 weeks in year
i <- 1
x <- start.2009
for(i in 1:52){
  long.mean[i] <- mean( aak968$longitude[dates.aak968<=(x+(6*24*60*60)) & dates.aak968>=x],na.rm=T)
  long.sd[i] <- sd( aak968$longitude[dates.aak968<=(x+(6*24*60*60)) & dates.aak968>=x],na.rm=T)
  long.med[i] <- median( aak968$longitude[dates.aak968<=(x+(6*24*60*60)) & dates.aak968>=x],na.rm=T)
  lat.mean[i] <- mean( aak968$stationery_latitude[dates.aak968<=(x+(6*24*60*60)) & dates.aak968>=x],na.rm=T)
  lat.sd[i] <- sd( aak968$stationery_latitude[dates.aak968<=(x+(6*24*60*60)) & dates.aak968>=x],na.rm=T)
  lat.med[i] <- median( aak968$stationery_latitude[dates.aak968<=(x+(6*24*60*60)) & dates.aak968>=x],na.rm=T)  
  week[i] <- i
  date_real[i] <- x+(3*24*60*60)
  date_normal[i] <- x+(3*24*60*60)
x <- x+(7*24*60*60)
  }

aak968.00 <- as.data.frame(cbind(long.mean, long.sd, long.med, lat.mean,lat.sd, lat.med, week, date_real, date_normal))


#weekly averages for aak968 - year two
long.mean<- long.sd<- long.med<- lat.mean<-lat.sd<- lat.med<- week<- date_real<- date_normal <- c(1:52)
#52 weeks in year
i <- 1
x <- start.2010
for(i in 1:52){
  long.mean[i] <- mean( aak968$longitude[dates.aak968<=(x+(6*24*60*60)) & dates.aak968>=x],na.rm=T)
  long.sd[i] <- sd( aak968$longitude[dates.aak968<=(x+(6*24*60*60)) & dates.aak968>=x],na.rm=T)
  long.med[i] <- median( aak968$longitude[dates.aak968<=(x+(6*24*60*60)) & dates.aak968>=x],na.rm=T)
  lat.mean[i] <- mean( aak968$stationery_latitude[dates.aak968<=(x+(6*24*60*60)) & dates.aak968>=x],na.rm=T)
  lat.sd[i] <- sd( aak968$stationery_latitude[dates.aak968<=(x+(6*24*60*60)) & dates.aak968>=x],na.rm=T)
  lat.med[i] <- median( aak968$stationery_latitude[dates.aak968<=(x+(6*24*60*60)) & dates.aak968>=x],na.rm=T)  
  week[i] <- i
  date_real[i] <- x+(3*24*60*60)
  date_normal[i] <- x-(362*24*60*60)
x <- x+(7*24*60*60)
  }

aak968.01 <- as.data.frame(cbind(long.mean, long.sd, long.med, lat.mean,lat.sd, lat.med, week, date_real, date_normal))


#weekly averages for aak962 - year one
long.mean<- long.sd<- long.med<- lat.mean<-lat.sd<- lat.med<- week<- date_real<- date_normal <- c(1:52)
#52 weeks in year
i <- 1
x <- start.2009
for(i in 1:52){
  long.mean[i] <- mean( aak962$longitude[dates.aak962<=(x+(6*24*60*60)) & dates.aak962>=x],na.rm=T)
  long.sd[i] <- sd( aak962$longitude[dates.aak962<=(x+(6*24*60*60)) & dates.aak962>=x],na.rm=T)
  long.med[i] <- median( aak962$longitude[dates.aak962<=(x+(6*24*60*60)) & dates.aak962>=x],na.rm=T)
  lat.mean[i] <- mean( aak962$stationery_latitude[dates.aak962<=(x+(6*24*60*60)) & dates.aak962>=x],na.rm=T)
  lat.sd[i] <- sd( aak962$stationery_latitude[dates.aak962<=(x+(6*24*60*60)) & dates.aak962>=x],na.rm=T)
  lat.med[i] <- median( aak962$stationery_latitude[dates.aak962<=(x+(6*24*60*60)) & dates.aak962>=x],na.rm=T)  
  week[i] <- i
  date_real[i] <- x+(3*24*60*60)
  date_normal[i] <- x+(3*24*60*60)
x <- x+(7*24*60*60)
  }

aak962.00 <- as.data.frame(cbind(long.mean, long.sd, long.med, lat.mean,lat.sd, lat.med, week, date_real, date_normal))


#weekly averages for cal01 - year one
long.mean<- long.sd<- long.med<- lat.mean<-lat.sd<- lat.med<- week<- date_real<- date_normal <- c(1:52)
#52 weeks in year
i <- 1
x <- start.2009
for(i in 1:52){
  long.mean[i] <- mean( cal01$longitude[dates.cal01<=(x+(6*24*60*60)) & dates.cal01>=x],na.rm=T)
  long.sd[i] <- sd( cal01$longitude[dates.cal01<=(x+(6*24*60*60)) & dates.cal01>=x],na.rm=T)
  long.med[i] <- median( cal01$longitude[dates.cal01<=(x+(6*24*60*60)) & dates.cal01>=x],na.rm=T)
  lat.mean[i] <- mean( cal01$stationery_latitude[dates.cal01<=(x+(6*24*60*60)) & dates.cal01>=x],na.rm=T)
  lat.sd[i] <- sd( cal01$stationery_latitude[dates.cal01<=(x+(6*24*60*60)) & dates.cal01>=x],na.rm=T)
  lat.med[i] <- median( cal01$stationery_latitude[dates.cal01<=(x+(6*24*60*60)) & dates.cal01>=x],na.rm=T)  
  week[i] <- i
  date_real[i] <- x+(3*24*60*60)
  date_normal[i] <- x+(3*24*60*60)
x <- x+(7*24*60*60)
  }

cal01.00 <- as.data.frame(cbind(long.mean, long.sd, long.med, lat.mean,lat.sd, lat.med, week, date_real, date_normal))


#weekly averages for cal01 - year two
long.mean<- long.sd<- long.med<- lat.mean<-lat.sd<- lat.med<- week<- date_real<- date_normal <- c(1:52)
#52 weeks in year
i <- 1
x <- start.2010
for(i in 1:52){
  long.mean[i] <- mean( cal01$longitude[dates.cal01<=(x+(6*24*60*60)) & dates.cal01>=x],na.rm=T)
  long.sd[i] <- sd( cal01$longitude[dates.cal01<=(x+(6*24*60*60)) & dates.cal01>=x],na.rm=T)
  long.med[i] <- median( cal01$longitude[dates.cal01<=(x+(6*24*60*60)) & dates.cal01>=x],na.rm=T)
  lat.mean[i] <- mean( cal01$stationery_latitude[dates.cal01<=(x+(6*24*60*60)) & dates.cal01>=x],na.rm=T)
  lat.sd[i] <- sd( cal01$stationery_latitude[dates.cal01<=(x+(6*24*60*60)) & dates.cal01>=x],na.rm=T)
  lat.med[i] <- median( cal01$stationery_latitude[dates.cal01<=(x+(6*24*60*60)) & dates.cal01>=x],na.rm=T)  
  week[i] <- i
  date_real[i] <- x+(3*24*60*60)
  date_normal[i] <- x-(362*24*60*60)
x <- x+(7*24*60*60)
  }

cal01.01 <- as.data.frame(cbind(long.mean, long.sd, long.med, lat.mean,lat.sd, lat.med, week, date_real, date_normal))



#weekly averages for cal02 - year one
long.mean<- long.sd<- long.med<- lat.mean<-lat.sd<- lat.med<- week<- date_real<- date_normal <- c(1:52)
#52 weeks in year
i <- 1
x <- start.2009
for(i in 1:52){
  long.mean[i] <- mean( cal02$longitude[dates.cal02<=(x+(6*24*60*60)) & dates.cal02>=x],na.rm=T)
  long.sd[i] <- sd( cal02$longitude[dates.cal02<=(x+(6*24*60*60)) & dates.cal02>=x],na.rm=T)
  long.med[i] <- median( cal02$longitude[dates.cal02<=(x+(6*24*60*60)) & dates.cal02>=x],na.rm=T)
  lat.mean[i] <- mean( cal02$stationery_latitude[dates.cal02<=(x+(6*24*60*60)) & dates.cal02>=x],na.rm=T)
  lat.sd[i] <- sd( cal02$stationery_latitude[dates.cal02<=(x+(6*24*60*60)) & dates.cal02>=x],na.rm=T)
  lat.med[i] <- median( cal02$stationery_latitude[dates.cal02<=(x+(6*24*60*60)) & dates.cal02>=x],na.rm=T)  
  week[i] <- i
  date_real[i] <- x+(3*24*60*60)
  date_normal[i] <- x+(3*24*60*60)
x <- x+(7*24*60*60)
  }

cal02.00 <- as.data.frame(cbind(long.mean, long.sd, long.med, lat.mean,lat.sd, lat.med, week, date_real, date_normal))


#weekly averages for cal02 - year two
long.mean<- long.sd<- long.med<- lat.mean<-lat.sd<- lat.med<- week<- date_real<- date_normal <- c(1:52)
#52 weeks in year
i <- 1
x <- start.2010
for(i in 1:52){
  long.mean[i] <- mean( cal02$longitude[dates.cal02<=(x+(6*24*60*60)) & dates.cal02>=x],na.rm=T)
  long.sd[i] <- sd( cal02$longitude[dates.cal02<=(x+(6*24*60*60)) & dates.cal02>=x],na.rm=T)
  long.med[i] <- median( cal02$longitude[dates.cal02<=(x+(6*24*60*60)) & dates.cal02>=x],na.rm=T)
  lat.mean[i] <- mean( cal02$stationery_latitude[dates.cal02<=(x+(6*24*60*60)) & dates.cal02>=x],na.rm=T)
  lat.sd[i] <- sd( cal02$stationery_latitude[dates.cal02<=(x+(6*24*60*60)) & dates.cal02>=x],na.rm=T)
  lat.med[i] <- median( cal02$stationery_latitude[dates.cal02<=(x+(6*24*60*60)) & dates.cal02>=x],na.rm=T)  
  week[i] <- i
  date_real[i] <- x+(3*24*60*60)
  date_normal[i] <- x-(362*24*60*60)
x <- x+(7*24*60*60)
  }

cal02.01 <- as.data.frame(cbind(long.mean, long.sd, long.med, lat.mean,lat.sd, lat.med, week, date_real, date_normal))


#plotting graphs to display the data - demonstrating SD error
 par(mfrow=c(2,1))
 plot(aak966_2009_10$trans_date,(aak966_2009_10$stationery_latitude)  ,pch="",las = 1, ylim=c(51,63),col="green",ylab="Latitude",xlab="Month",xaxt="n",cex.lab=2,cex.axis=1.7)
lines(aak966.00$date_normal,(aak966.00$lat.mean),lwd=2,col="red")
lines( aak966.00$date_normal,(aak966.00$lat.mean-aak966.00$lat.sd)  ,lwd=1,lty=3,col="red")
lines( aak966.00$date_normal,(aak966.00$lat.mean+aak966.00$lat.sd)  ,lwd=1,lty=3,col="red")
points( aak966.00$date_normal,(aak966.00$lat.mean))
lines( aak969.00$date_normal,(aak969.00$lat.mean)  ,lwd=2,col="blue")
lines( aak969.00$date_normal,(aak969.00$lat.mean-aak969.00$lat.sd)  ,lwd=1,lty=3,col="blue")
lines( aak969.00$date_normal,(aak969.00$lat.mean+aak969.00$lat.sd)  ,lwd=1,lty=3,col="blue")
points( aak969.00$date_normal,(aak969.00$lat.mean))
 abline(h= 57.289,lty=6, lwd=3, col="gray20")
r <- as.POSIXct(round(range(aak966_2009_10$trans_date), "days"))
axis.POSIXct(1, at=seq(r[1], r[2], by="month"), format="%b",cex.axis=1.7)




#plotting graphs to display all the data
 par(mfrow=c(2,1))
 plot(aak966.00$date_normal,(aak966.00$lat.mean)  ,pch="",las = 1, ylim=c(51,63),col="green",ylab="Latitude",xlab="Month",xaxt="n",cex.lab=2,cex.axis=1.7)
lines(aak966.00$date_normal,(aak966.00$lat.mean),lwd=2,col="red")
lines( aak969.00$date_normal,(aak969.00$lat.mean)  ,lwd=2,col="blue")
lines( aak969.01$date_normal,(aak969.01$lat.mean)  ,lwd=2,col="blue",lty=3)
lines(aak970.00$date_normal,(aak970.00$lat.mean),lwd=2,col="green")
lines( aak970.01$date_normal,(aak971.00$lat.mean)  ,lwd=2,col="green",lty=3)
lines(aak968.00$date_normal,(aak968.00$lat.mean),lwd=2,col="black")
lines( aak968.01$date_normal,(aak968.01$lat.mean)  ,lwd=2,col="black",lty=3)
lines(aak962.00$date_normal,(aak962.00$lat.mean),lwd=2,col="orange")
lines(cal01.00$date_normal,(cal01.00$lat.mean),lwd=2,col="purple")
lines( cal01.01$date_normal,(cal01.01$lat.mean)  ,lwd=2,col="purple",lty=3)
lines(cal02.00$date_normal,(cal02.00$lat.mean),lwd=2,col="pink")
lines( cal02.01$date_normal,(cal02.01$lat.mean)  ,lwd=2,col="pink",lty=3)
 abline(h= 57.289,lty=6, lwd=3, col="gray20")
r <- as.POSIXct(round(range(aak966_2009_10$trans_date), "days"))
axis.POSIXct(1, at=seq(r[1], r[2], by="month"), format="%b",cex.axis=1.7)

#plotting graphs to display all the data
 plot(aak966.00$date_normal,(-aak966.00$long.mean)  ,pch="",las = 1, ylim=c(13.5,22),col="green",ylab="longitude",xlab="Month",xaxt="n",cex.lab=2,cex.axis=1.7)
lines(aak966.00$date_normal,(-aak966.00$long.mean),lwd=2,col="red")
lines( aak969.00$date_normal,(-aak969.00$long.mean)  ,lwd=2,col="blue")
lines( aak969.01$date_normal,(-aak969.01$long.mean)  ,lwd=2,col="blue",lty=3)
lines(aak970.00$date_normal,(-aak970.00$long.mean),lwd=2,col="green")
lines( aak970.01$date_normal,(-aak971.00$long.mean)  ,lwd=2,col="green",lty=3)
lines(aak968.00$date_normal,(-aak968.00$long.mean),lwd=2,col="black")
lines( aak968.01$date_normal,(-aak968.01$long.mean)  ,lwd=2,col="black",lty=3)
lines(aak962.00$date_normal,(-aak962.00$long.mean),lwd=2,col="orange")
lines(cal01.00$date_normal,(-cal01.00$long.mean),lwd=2,col="purple")
lines( cal01.01$date_normal,(-cal01.01$long.mean)  ,lwd=2,col="purple",lty=3)
lines(cal02.00$date_normal,(-cal02.00$long.mean),lwd=2,col="pink")
lines( cal02.01$date_normal,(-cal02.01$long.mean)  ,lwd=2,col="pink",lty=3)
 abline(h= 17.960,lty=6, lwd=3, col="gray20")
r <- as.POSIXct(round(range(aak966_2009_10$trans_date), "days"))
axis.POSIXct(1, at=seq(r[1], r[2], by="month"), format="%b",cex.axis=1.7)






 par(mfrow=c(2,1))
 
 plot(aak966.00$date_normal,(aak966.00$lat.mean)  ,pch="",las = 1,ylim=c(51,63),col="green",ylab="latitude",xlab="Month",xaxt="n",cex.lab=2,cex.axis=1.7)
lines(aak966.00$date_normal,(aak966.00$lat.mean)  ,lwd=2,col="red")
points( aak966.00$date_normal,(aak966.00$lat.mean))
lines(aak969.00$date_normal,(aak969.00$lat.mean) ,lwd=2,col="blue")
points( aak969.00$date_normal,(aak969.00$lat.mean))
 abline(h= 57.289,lty=6, lwd=3, col="gray20")
r <- as.POSIXct(round(range(as.POSIXct( aak966.00$date_normal, origin="1970-01-01")), "days"))
axis.POSIXct(1, at=seq(r[1], r[2], by="month"), format="%b",cex.axis=1.7)
lines( aak969.00$date_normal,((aak969.00$lat.mean-aak969.00$lat.sd)),lwd=1,lty=3,col="blue")
lines( aak969.00$date_normal,((aak969.00$lat.mean+aak969.00$lat.sd)),lwd=1,lty=3,col="blue")
lines( aak966.00$date_normal,((aak966.00$lat.mean-aak966.00$lat.sd)),lwd=1,lty=3,col="red")
lines( aak966.00$date_normal,((aak966.00$lat.mean+aak966.00$lat.sd)),lwd=1,lty=3,col="red")

plot(aak966.00$date_normal,(aak966.00$lat.mean)  ,pch="",las = 1, ylim=c(13.5,22),col="green",ylab="Longitude",xlab="Month",xaxt="n",cex.lab=2,cex.axis=1.7)
lines(lowess( aak966.00$date_normal,(-1*aak966.00$long.mean)  ,f=1/20),lwd=2,col="red")
points( aak966.00$date_normal,(-1*aak966.00$long.mean))
lines(lowess( aak969.00$date_normal,(-1*aak969.00$long.mean)  ,f=1/20),lwd=2,col="blue")
points( aak969.00$date_normal,(-1*aak969.00$long.mean))
abline(h= 17.960,lty=6, lwd=3, col="gray20")
r <- as.POSIXct(round(range(as.POSIXct( aak966.00$date_normal, origin="1970-01-01")), "days"))
axis.POSIXct(1, at=seq(r[1], r[2], by="month"), format="%b",cex.axis=1.7)
lines( aak969.00$date_normal,(-1*(aak969.00$long.mean-aak969.00$long.sd)),lwd=1,lty=3,col="blue")
lines( aak969.00$date_normal,(-1*(aak969.00$long.mean+aak969.00$long.sd)),lwd=1,lty=3,col="blue")
lines( aak966.00$date_normal,(-1*(aak966.00$long.mean-aak966.00$long.sd)),lwd=1,lty=3,col="red")
lines( aak966.00$date_normal,(-1*(aak966.00$long.mean+aak966.00$long.sd)),lwd=1,lty=3,col="red")
