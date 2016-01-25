library(ggplot2)

setwd("C:/Users/GNK/Dropbox/Aktiva kurser/Doktorandtjänst/Sillgrissla/")

d <-read.csv("sillgrissla.csv", header = TRUE, sep = ",")
str(d)
d$date_time <- as.POSIXct(strptime(d$date_time, format = "%d/%m/%y %H:%M:%S"), tz = "GMT")
d$date <- as.Date(d$date_time)
d <- d[which(d$date > "2011-04-12"),] #Polish the start time to to get the repeat function to work
d <- d[which(d$date < "2012-06-19"),] 
d$sample <- rep(1:144,nrow(d)/144) #To be used for the Y-axis in the plot. Days lacking any observations will mess up this



sun <-read.csv("sunStK.csv", header = TRUE, sep = ",")
str(sun)
#Following lines translate hours and minutes into number of 10min sections for rise and set respectively
sun$rise.time <- as.integer(substr(strptime(sun$Sunrise.Time..LST., format = "%H:%M:%S"), start = 12, stop = 13)) * 6 +
            as.integer(substr(strptime(sun$Sunrise.Time..LST., format = "%H:%M:%S"), start = 15, stop = 16)) / 10
sun$set.time <- as.integer(substr(strptime(sun$Sunset.Time..LST., format = "%H:%M:%S"), start = 12, stop = 13)) * 6 +
  as.integer(substr(strptime(sun$Sunset.Time..LST., format = "%H:%M:%S"), start = 15, stop = 16)) / 10

P <- ggplot(d)
P <- P + geom_tile(aes(x = as.character(date), y = as.integer(sample), fill = activity)) #Daily pattern of wet-dry data across dates. "sample" is the series of 10min sections per day 1-144
P <- P + geom_point(aes(x = as.character(date), y = as.integer(sample),size = temp), colour = "red") #Adds a distribution and level of temperature data
P <- P + scale_size_continuous(range = c(1,3)) #Adjust size range of temperature points
P <- P + geom_line(data = sun, aes(x = as.character(Date), y = as.integer(rise.time), group = group), size = 2, colour = "white", alpha = 0.5) #Add line for sun rise
P <- P + geom_line(data = sun, aes(x = as.character(Date), y = as.integer(set.time), group = group), size = 2, colour = "white", alpha = 0.5) #Add line for sun set

P

png("Sillgrissla.png", width = 1800, height = 1500) #1300*600 för A58417
P
dev.off()
