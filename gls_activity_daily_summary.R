# Set working directory where your files are located (new daily
# summary will be output here too)
setwd("F:/Documents/Work/GLS_DATA/Guillemots/2010/gl12013")

# getwd()

# Activity file to read in
file.in <- "data_example.act"

# Name of file to output (should be csv)
file.out <- "data_example_daily_summary.csv"


#*********************************************************************
# After changing file names above run rest of code




#start with raw files
#names for column headers
headers = c("fix_type","dd_mm_yy","mid_value","activity")     


data1 <- read.table(file.in, header = F, sep = ",", col.names = headers)


# Get number of days represented in file
days <- max(floor(data1$mid_value))- min(floor(data1$mid_value))

# Initiate (make empty vectors) variables
n.dry <- n.wet <- median.value <- mean.value  <- median.value.between <- mean.value.night <- mean.value.day <-  day <- time <- day.n <- day.prop.wet <- n.wet <- n.mix <- NULL



# Get number of data points (rows of data)
m <- length(data1$activity)
# Make vectors with day and time (i.e. date-time - date)
# Function to number each point by day
day.n.fun <- function(date_value, date_value_min){
  ((floor(date_value)) - (floor(date_value_min)) )
}

day <- sapply(X = data1$mid_value, FUN = day.n.fun, date_value_min = min(data1$mid_value))

time <- data1$mid_value - floor(data1$mid_value)



# for each day, count number of dry and wet blocks
for (i in 1:days){
  x <- (day == i )
  y <- (day == i) & (data1$activity > 150)
  z <- (day == i) & (data1$activity < 50)
   nighttime <- x & ((time < 1/24 )| (time >23/24))
  #summary(nighttime)
  daytime <- x & time > 11/24 & time <  13/24
#   summary(daytime)
  median.value[i] <- median(data1$activity[x]) 
    mean.value[i] <- mean(data1$activity[x]) 
  n.wet[i] <-    length(data1$fix_type[y])
  median.value.between[i]  <- median(data1$activity[x & (data1$activity != 0) & (data1$activity != 200)]  )
  n.dry[i] <-  length(data1$fix_type[z])
  n.wet[i] <-  length(data1$fix_type[y])
  n.mix[i] <-  length(data1$fix_type[!z & !y])
  day.n[i] <-    floor(data1$mid_value[x][1])
  day.prop.wet[i] <- (sum(data1$activity[x])) /28800
  mean.value.day[i] <-  mean(data1$activity[daytime])
  mean.value.night[i] <-  mean(data1$activity[nighttime])  
}



#counting consecutive dry/ wet blocks

#1st pass, label each block, as wet (>150) 2, mix (50 - 150) 1, dry (<50) 0
m <- length(data1$activity)

activity.type <- rep(1, m)
activity.type[data1$activity < 50] <- 0
activity.type[data1$activity > 150] <- 2

  
#2nd pass, number consecutive contiguous activity type blocks the same
event <- NULL
v <- 0
event[1] <- 0
i <- 2
for(i in 2:m){
  if(activity.type[i] == activity.type[i-1]){
  event[i] <- v}
  else{
  event[i] <- v +1
  v <- v + 1
  }
}  

  
#3rd pass, make a dataframe of events
event.type <- event.date_time <- event.blocks <- event.date <- NULL
m2 <- max(event)
for(i in 1:m2){
  x <- event == i
  event.type[i] = median(activity.type[x])
  event.date_time[i] = min(data1$mid_value[x])
  event.blocks[i] = length(activity.type[x] == T)
  event.date[i] = floor(event.date_time[i])
}  



#4th pass, make daily summary table
day.dry.number <- day.wet.number  <- day.mix.number  <- day.max.wet    <- day.max.dry    <- day.min.wet   <- day.min.dry   <- day.date   <- day.number     <- day.med.dry    <- day.med.wet    <- day.mean.dry   <- day.mean.wet   <- day.time       <- NULL

days <- max(event.date) - min(event.date)

start.date <- min(event.date)
day.date <- NULL
z <- start.date

for(i in 1:days){
  day.number[i] <-     i
  x <- event.date == z
  wet <- (event.date == z) & (event.type == 2)
  dry <- (event.date == z) & (event.type == 0)
  (event.date == z) & (event.type == 1)
  event.type[event.date == z]

#   summary(as.factor(event.type))
  
  # http://stackoverflow.com/questions/2190756/in-r-how-to-count-true-values-in-a-logical-vector
#   x <- c(0,0,0,1,1,1,1,2,2)
#   length(x == 1)
#   sum((x == 1), na.rm=TRUE)
  
  day.dry.number[i] <- sum((event.type[x] == 0), na.rm=TRUE)
  day.wet.number[i] <- sum((event.type[x] == 2), na.rm=TRUE)
  day.mix.number[i] <- sum((event.type[x] == 1), na.rm=TRUE)  
  day.max.wet[i] <- max(event.blocks[wet])    
  day.min.wet[i] <- min(event.blocks[wet])      

  day.max.dry[i] <- max(event.blocks[dry]) 
  # If there are no 
#   if.na(day.max.dry[i])day.max.dry[i]  <- 0
#   max(numeric(0))
  day.min.dry[i] <- min(event.blocks[dry])
  day.mean.dry[i] <- mean(event.blocks[dry])    
  day.med.dry[i] <- median(event.blocks[dry])
  day.mean.wet[i] <- mean(event.blocks[wet])    
  day.med.wet[i] <- median(event.blocks[wet])

  if(sum((event.blocks[wet]), na.rm=TRUE) == 0){
    day.max.wet[i] <- day.min.wet[i] <- day.mean.wet[i] <- day.med.wet[i] <- 0
  }

  if(sum((event.blocks[dry]), na.rm=TRUE) == 0){
    day.max.dry[i] <- day.min.dry[i] <- day.mean.dry[i] <- day.med.dry[i] <- 0
  }

  day.date[i] <- z
  z <- z+1
}




real <- c(1:length(day.number))
daily.summary <- as.data.frame(cbind(day.number[real], day.dry.number[real],  day.wet.number[real], day.mix.number[real],  day.max.wet[real], day.min.wet[real], day.max.dry[real], day.min.dry[real], day.mean.dry[real], day.med.dry[real], day.mean.wet[real], day.med.wet[real], day.date[real], median.value[real],mean.value[real],median.value.between[real],n.dry[real], n.wet[real], n.mix[real], day.prop.wet[real], mean.value.day[real], mean.value.night[real], as.character( as.POSIXct(day.date[real]*24*60*60,origin="1900-01-01", tz="UTC"))))


header.names <- c("day.number", "day.dry.number",  "day.wet.number", "day.mix.number",  "day.max.wet", "day.min.wet", "day.max.dry", "day.min.dry", "day.mean.dry", "day.med.dry", "day.mean.wet", "day.med.wet", "day.date", "median.value","mean.value","median.value.between","n.dry", "n.wet", "n.wet", "day.prop.wet", "mean.value.day", "mean.value.night", "date")

names(daily.summary) <- header.names

write.csv(daily.summary, file = file.out,
          row.names = FALSE)


