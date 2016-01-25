library(ggplot2)

# setwd("C:/Users/GNK/Dropbox/Aktiva kurser/Doktorandtjänst/Sillgrissla/")

d <-read.csv("aak968_20120620_000_activity_data.csv", header = TRUE, sep = ",")
# d <-read.csv("gls10049_20120625_000_activity_data.csv", header = TRUE, sep = ",")
# d <-read.csv("GLS12018_2011_06_19_AAK969_000.act", header = FALSE, sep = ",")
# names(d) <- c("x","date_time","date_time_excel","activity")
# d <- d[-1]
str(d)
d$date_time <- as.POSIXct(strptime(d$date_time, format = "%d/%m/%y %H:%M:%S"), tz = "GMT")
d$date <- as.Date(d$date_time)
time_only <- strftime(d$date_time, format="%H:%M:%S")
d$time <- as.POSIXct(time_only, format="%H:%M:%S")
d <- d[which(d$date > "2011-05-10"),] #Polish the start time to to get the repeat function to work
d <- d[which(d$date < "2012-06-15"),] 

d <- d[which(d$date > "2009-05-10"),] #Polish the start time to to get the repeat function to work
d <- d[which(d$date < "2010-06-15"),] 
# d$sample <- rep(NA,nrow(d))
d$sample <- rep(1:144,nrow(d)/144)
# d$sample <- c(1,rep(1:144,nrow(d)/144)) #To be used for the Y-axis in the plot. Days lacking any observations will mess up this
# test <- rep(1:144,nrow(d)/144)


sun <-read.csv("sunStK.csv", header = TRUE, sep = ",")
str(sun)
#Following lines translate hours and minutes into number of 10min sections for rise and set respectively
sun$rise.time <- as.integer(substr(strptime(sun$Sunrise.Time..LST., format = "%H:%M:%S"), start = 12, stop = 13)) * 6 +
            as.integer(substr(strptime(sun$Sunrise.Time..LST., format = "%H:%M:%S"), start = 15, stop = 16)) / 10
sun$set.time <- as.integer(substr(strptime(sun$Sunset.Time..LST., format = "%H:%M:%S"), start = 12, stop = 13)) * 6 +
  as.integer(substr(strptime(sun$Sunset.Time..LST., format = "%H:%M:%S"), start = 15, stop = 16)) / 10

P <- ggplot(d)
P <- P + geom_tile(aes(x = date, y = as.integer(sample), fill = activity)) #Daily pattern of wet-dry data across dates. "sample" is the series of 10min sections per day 1-144
P <- P + geom_tile(aes(x = date, y = time, fill = activity)) #Daily pattern of wet-dry data across dates. "sample" is the series of 10min sections per day 1-144







# create date variable for the x-axis
d$date <- as.Date(d$date_time, format = "%Y-%m-%d")

# get H:M components
d$hm <- strftime(d$date_time, format="%H:%M")    

# create y-axis breaks and labels
lab <- with(d, paste(strftime(d$date_time, format="%H"), "00", sep = ":"))

gg <- ggplot(data = d, aes(x = date, y = hm, fill = activity)) +
  geom_tile() +
  scale_y_discrete(breaks = lab)

gg

d$m <- strftime(d$date_time, format="%M") 
d$h <- strftime(d$date_time, format="%H") 
d$mh <- as.numeric(d$m) + as.numeric(d$h)*60

d$ten <- as.POSIXlt(floor(as.double(d$date_time)/(10*60))*(10*60),origin=(as.POSIXlt('1970-01-01')))


time_only_ten <- strftime(d$ten, format="%H:%M:%S")
d$ten_t <- as.POSIXct(time_only_ten, format="%H:%M:%S")

# d$ten_t <- 
  
  
  
  
ggplot(data=d, aes(x=date ,y=ten_t, fill = activity)) +
  geom_tile()
  # stat_density(aes(fill = activity), contour = FALSE)
# hist(d$mh)



P <- P + scale_fill_gradient(low = "#56B1F7", high = "#132B43")
# P <- P + scale_fill_gradient(low = "black", high = "green")

# ?scale_fill_gradient
# P <- P + geom_point(aes(x = as.character(date), y = as.integer(sample),size = temp), colour = "red") #Adds a distribution and level of temperature data
# P <- P + geom_point(aes(x = as.character(date), y = as.integer(sample), colour = temp), na.rm = TRUE) #Adds a distribution and level of temperature data
# P
# P <- P + scale_colour_gradient(limits=c(0, 25), low = "blue", high = "red", na.rm = TRUE)
# P <- P + scale_color_discrete(na.value= NA)
# P <- P + scale_size_continuous(range = c(1,3)) #Adjust size range of temperature points
P <- P + geom_line(data = sun, aes(x = as.character(Date), y = as.integer(rise.time), group = group), size = 2, colour = "white", alpha = 0.5) #Add line for sun rise
P <- P + geom_line(data = sun, aes(x = as.character(Date), y = as.integer(set.time), group = group), size = 2, colour = "white", alpha = 0.5) #Add line for sun set

P

png("AAK970_wetdry.png", width = 1800, height = 1500) #1300*600 för A58417
P
dev.off()
