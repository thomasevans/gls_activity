# Source this file for 'activ.plot' function

activ.plot <- function(act_file = "GLS12018_2011_06_19_AAK969_000.act",
                       trn_file = "gls10049_20120625_000_thresh_10.trn",
                       trn_true = TRUE,
                       col.wet = "#56B1F7", col.dry = "#132B43",
                       time_zone = "UTC", long = 18.0, lat = 57.25){
  
  
  
  # act_file <- "GLS12018_2011_06_19_AAK969_000.act"
  
  # In function
  
  # Required packages:
  require("ggplot2")
  require("scales")
  require("maptools")
  
  # Load specified activity file
  d <-read.csv(act_file, header = FALSE, sep = ",")
  names(d) <- c("x","date_time","date_time_excel","activity")
  
    # Fix date_time format and make sepperate date and time objects
  d$date_time <- as.POSIXct(strptime(as.character(d$date_time), format = "%d/%m/%y %H:%M:%S"), tz = "UTC")
  # d$date_time <- as.POSIXct(d$date_time, tz = time_zone)
  d$date <- as.Date(d$date_time, tz = time_zone)
  d$time_only <- strftime(d$date_time, format="%H:%M:%S", tz = time_zone)
  d$time <- as.POSIXct(d$time_only, format="%H:%M:%S", tz = time_zone)
  
    # Fix date range (in case there are partial days at the start and end)
  date.range <- range(d$date, na.rm = TRUE)
  dates.in.file <- unique(d$date)
  # summary((d$date > date.range[1]  & d$date < date.range[2]))
  # Remove first and final day (then hopefully middle days are complete)
  d.new <- d[(d$date > date.range[1]  & d$date < date.range[2]),]
  
  
#   as.Date(d$date_time[1:100], tz = "EST")
#   as.Date(d$date_time[1:100], tz = "UTC")
  
  
  # ?as.POSIXct
  
  if(trn_true){
  
    trn <-read.csv(trn_file, header = FALSE, sep = ",")
    names(trn) <- c("date_time","type","x")
    trn$date_time <- as.POSIXct(strptime(as.character(trn$date_time), format = "%d/%m/%y %H:%M:%S"), tz = "UTC")
    sunrises <- trn$date_time[trn$type == "Sunrise"]
    sunsets <- trn$date_time[trn$type == "Sunset"]
    
    sunrise.dates <- as.Date(sunrises, tz = time_zone)
    sunrise.times <- strftime(sunrises, format="%H:%M:%S", tz = time_zone)
    sunrise.new <- cbind.data.frame(sunrise.dates, sunrise.times)
      
    sunset.dates <- as.Date(sunsets, tz = time_zone)
    sunset.times <- strftime(sunsets, format="%H:%M:%S", tz = time_zone)
    sunset.new <- cbind.data.frame(sunset.dates, sunset.times)
    
    names(sunrise.new) <- c("date", "sunrise_time")
    names(sunset.new) <- c("date", "sunset_time")
    
    suntimes <- merge(sunrise.new, sunset.new, by = "date", all= FALSE)
    
    # suntimes$date %in% d.new$date
    
    # ?merge
    d.new.test <- merge(d.new, suntimes, by = "date", all.x = TRUE, all.y = FALSE)
    
    d.new.test$sunrise_time <- as.POSIXct(as.character(d.new.test$sunrise_time), format="%H:%M:%S", tz = time_zone)
    d.new.test$sunset_time <- as.POSIXct(as.character(d.new.test$sunset_time), format="%H:%M:%S", tz = time_zone)
    
    d.new <- d.new.test
    # str(d.new)
# str(d.new.test)
    
    
  }else {
    # Add code to get sunrise/sunset times for specified location (e.g. colony)
    # x_seq <- seq(from = date.range[1], to = date.range[2], by = "days")
    # x_seq <- as.POSIXct(x_seq, format = "%d/%m/%y", tz = "UTC")
    coord <- matrix(c(long, lat), nrow = 1)
    sunrise <- sunriset(coord, d.new$date_time, direction = "sunrise",
                        POSIXct.out = TRUE)[,2]
    sunset <- sunriset(coord, d.new$date_time, direction = "sunset",
                       POSIXct.out = TRUE)[,2]
    d.new <- cbind.data.frame(d.new, sunrise, sunset)
    # names(sunriseset) <- c("date", "rise", "set")
    d.new$sunrise <- format(d.new$sunrise, format = "%H:%M:%S")
    d.new$sunrise <- as.POSIXct(d.new$sunrise, format = "%H:%M:%S")
    d.new$sunset <- format(d.new$sunset, format = "%H:%M:%S")
    d.new$sunset <- as.POSIXct(d.new$sunset, format = "%H:%M:%S")
    
  }
  
  
  
  # Fix dates of sunrise and sunsets
  
# 
#   d.new$sunrise[c(1,15050,30000)]
#   d.new$date[c(1,15050,30000)]
#   d.new$time[c(1,15050,30000)]
#   

  
  
#   # get H:M components
#   d.new$hm <- strftime(d.new$date_time, format="%H:%M")    
#   
#   # create y-axis breaks and labels
#   # lab <- with(d.new, paste(strftime(d.new$date_time, format="%H"), "00", sep = ":"))
#   
#   
#   d.new$m <- strftime(d.new$date_time, format="%M") 
#   d.new$h <- strftime(d.new$date_time, format="%H") 
#   d.new$mh <- as.numeric(d.new$m) + as.numeric(d.new$h)*60
#   
  d.new$ten <- as.POSIXlt(floor(as.double(d.new$date_time)/(10*60))*(10*60),
                          origin = (as.POSIXlt('1970-01-01')), tz = time_zone)
  
  
  time_only_ten <- strftime(d.new$ten, format="%H:%M:%S")
  d.new$ten_t <- as.POSIXct(time_only_ten, format="%H:%M:%S", tz = "UTC")
#   x1 <-  as.POSIXct(time_only_ten, format="%H:%M:%S", tz = time_zone)
#   x2 <-  as.POSIXct(time_only_ten, format="%H:%M:%S")
#   head(x1)
#   head(x2)
  # str(d.new)
  # d.new$ten_t<- format(d.new$ten_t, "%H:%M")
  # ylim <- as.POSIXct(c("00:00:00","24:00:00"), format="%H:%M:%S", tz = time_zone)
  # ylim <- as.POSIXct(, format="%H:%M:%S", tz = time_zone)
#   (d.new$ten_t[1:10])
  
  
  
  if(trn_true){
    
#     x <- paste(Sys.Date(), c("00:04:00", "28:00:00"), sep = " ")
#     x <- as.POSIXct(limit.times, format="%Y-%m-%d %H:%M:%S", tz = time_zone)
#     
    
    limit.times <- paste(Sys.Date(), c("00:00:00", "24:00:00"), sep = " ")
    limit.times <- as.POSIXct(limit.times, format="%Y-%m-%d %H:%M:%S", tz = time_zone)
    
    reset.datetime <- function(x){
      if(is.na(x)) return(x) else {
        if(x > limit.times[2]){return((x - 24*60*60))} else if(x < limit.times[1]){
          return((x + 24*60*60))
        } else return(x)
      }
     }
    
    
    d.new$sunrise_time3 <- paste(Sys.Date(), strftime(d.new$sunrise_time, format="%H:%M:%S"), sep = " ")
    # str(d.new$sunrise_time3)
    d.new$sunrise_time4 <- as.POSIXct(d.new$sunrise_time3, format="%Y-%m-%d %H:%M:%S", tz = time_zone)
    d.new$sunrise_time4 <-  as.POSIXct(sapply(d.new$sunrise_time4, reset.datetime), origin = (as.POSIXlt('1970-01-01')), tz = "UTC")
    str(d.new$sunrise_time5)
    
    
    d.new$sunset_time3 <- paste(Sys.Date(), strftime(d.new$sunset_time, format="%H:%M:%S"), sep = " ")
    # str(d.new$sunrise_time3)
    d.new$sunset_time4 <- as.POSIXct(d.new$sunset_time3, format="%Y-%m-%d %H:%M:%S", tz = time_zone)
    d.new$sunset_time4 <-  as.POSIXct(sapply(d.new$sunset_time4, reset.datetime), origin = (as.POSIXlt('1970-01-01')), tz = "UTC")
    
  }
  
  
#   as.POSIXlt(d.new$ten_t[1:10], format = "%H:%M")
  P <- ggplot(data = d.new, aes(x = date, y = as.POSIXlt(ten_t, format = "%H:%M"), fill = activity)) +
    geom_tile()
  # dev.off()
  P <- P + scale_fill_gradient(high = col.wet, low = col.dry)
  P <- P + xlab("Date") + ylab("Time")
  P <- P + scale_y_datetime(breaks = date_breaks("2 hour"),
                        labels = date_format("%H:%M"))
#   P <- P + scale_y_date(breaks = "1 month", 
#                         labels=date_format("%b-%Y"),
#                         )
  
#   
#   str(d.new$sunrise_time)
#   (d.new$sunrise_time[20000:20010])
#   d.new$date[20000:20010]
#   old.t <-  d.new$sunrise_time
#   
  # (d.new$sunset_time4[20000:20050])
#   d.new$date[20000:20050]
#   d.new$time[20000:20050]
#   
#   (d.new$sunrise_time[60000:60050])
#   d.new$date[60000:60050]
#   d.new$time[60000:60050]
#   str(d.new$sunrise_time)
#   
#   d.new$sunrise_time <- old.t
#   as.Date(paste("2011-", format(x, "%m-%d"), sep = ""))
#   d.new$sunrise_time <- as.POSIXct(paste(format(d.new$sunrise_time, "%H:%M")), format = "%Y-%m-%d %H:%M", tz = time_zone)
#   
#   as.POSIXlt(d.new$sunrise_time2[20000:20010], format = "%H:%M")
# d.new$sunrise_time2 <-  as.POSIXct(format(d.new$sunrise_time, tz = time_zone), tz = time_zone)
# str(d.new$sunrise_time2)
# 


  # format(d.new$sunrise_time[20000:20010], tz = "UTC")
  
  
    if(trn_true){
    
      

     P <- P + geom_point(
        aes(x = date, y = as.POSIXlt(sunrise_time4)), size = 2, colour = "green", alpha = 0.1) #Add line for sun rise
      
     
     P <-   P + geom_point(
        aes(x = date, y = as.POSIXlt(sunset_time4)), size = 2, colour = "red", alpha = 0.1) #Add line for sun rise
      
#       
#       
#     P <- P + geom_point(
#       aes(x = date, y = as.POSIXlt(sunrise_time, format = "%H:%M")), size = 2, colour = "green", alpha = 0.5) #Add line for sun rise
#     

#     P + geom_point(data = suntimes,
#       aes(x = date, y = as.POSIXlt(sunrise_time, format = "%H:%M") ), size = 2, colour = "green", alpha = 0.5) #Add line for sun rise
    

    # P <- P + geom_point(
      # aes(x = date, y = as.POSIXlt(sunset_time, format = "%H:%M", tz = time_zone)), size = 2, colour = "red", alpha = 0.5) #Add line for sun rise
  } else {
      # Add sunrise and sunset lines
      P <- P + geom_point(
        aes(x = date, y = sunrise), size = 2, colour = "green", alpha = 0.5) #Add line for sun rise
      P <- P + geom_point(
        aes(x = date, y = sunset), size = 2, colour = "red", alpha = 0.5) #Add line for sun rise
      
  }

  
  
  
  print(P)

  
}  
