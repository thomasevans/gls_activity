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
  

  
  
  
  
  # get H:M components
  d.new$hm <- strftime(d.new$date_time, format="%H:%M")    
  
  # create y-axis breaks and labels
  # lab <- with(d.new, paste(strftime(d.new$date_time, format="%H"), "00", sep = ":"))
  
  
  d.new$m <- strftime(d.new$date_time, format="%M") 
  d.new$h <- strftime(d.new$date_time, format="%H") 
  d.new$mh <- as.numeric(d.new$m) + as.numeric(d.new$h)*60
  
  d.new$ten <- as.POSIXlt(floor(as.double(d.new$date_time)/(10*60))*(10*60),
                          origin = (as.POSIXlt('1970-01-01')))
  
  
  time_only_ten <- strftime(d.new$ten, format="%H:%M:%S")
  d.new$ten_t <- as.POSIXct(time_only_ten, format="%H:%M:%S")
  
  # str(d.new)
  # d.new$ten_t<- format(d.new$ten_t, "%H:%M")
  
  P <- ggplot(data = d.new, aes(x = date, y = as.POSIXlt(ten_t, format = "%H:%M"), fill = activity)) +
    geom_tile()
  # dev.off()
  P <- P + scale_fill_gradient(high = col.wet, low = col.dry)
  P <- P + xlab("Date") + ylab("Time")
  P <- P + scale_y_datetime(breaks = date_breaks("2 hour"),
                        labels = date_format("%H:%M"))
  
  
  if(trn_true){
    
    P <- P + geom_line(
      aes(x = date, y = as.POSIXlt(sunrise_time, format = "%H:%M") ), size = 2, colour = "green", alpha = 0.5) #Add line for sun rise
    
    P <- P + geom_line(
      aes(x = date, y = as.POSIXlt(sunset_time, format = "%H:%M")), size = 2, colour = "red", alpha = 0.5) #Add line for sun rise
  } else {
      # Add sunrise and sunset lines
      P <- P + geom_line(
        aes(x = date, y = sunrise), size = 2, colour = "green", alpha = 0.5) #Add line for sun rise
      P <- P + geom_line(
        aes(x = date, y = sunset), size = 2, colour = "red", alpha = 0.5) #Add line for sun rise
      
  }

  
  
  
  print(P)

  
}  
