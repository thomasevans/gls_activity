
# Enter name of file to produce figure from
file.in <- "GLS12018_2011_06_19_AAK969_000_daily_summary.csv"

# ***************************************
# Run below code to produce figure of daily activity budget accross year


# Load data ----
day.dat <- read.csv(file.in)


# Calculate day percentages -----
day.block.tot <- day.dat$n.wet + day.dat$n.dry + day.dat$n.mix
day.dry.p <- 100 * day.dat$n.dry / day.block.tot
day.wet.p <- 100 * day.dat$n.wet / day.block.tot
day.mix.p <- 100 * day.dat$n.mix / day.block.tot

# Make date object ------
day.date <- as.Date(day.dat$date, tz = "UTC")


# Plot activity budget over time ------
library(ggplot2)
library(scales)
library(RColorBrewer)
library(reshape2)

# ?melt

x <- cbind(day.dry.p,day.wet.p,day.mix.p)
dates <- rep(day.date, 3)
x.long <- melt(x)
x.long <- x.long[,2:3]
names(x.long) <- c("Activity", "Percent")


x.long <- data.frame(dates, x.long)

# levels(x.long$Activity)
levels(x.long$Activity) <- c("Dry", "Wet", "Mix")
ar_lev <- levels(x.long$Activity)
ar_lev <- ar_lev[c(1,3,2)]
x.long$Activity <- factor(x.long$Activity, levels = ar_lev)

# ?pdf
# pdf("GLS12018_2011_06_19_AAK969_000_daily_summary.pdf")

ggplot(x.long, aes(dates, Percent, group = Activity)) +
  geom_bar(stat = "identity", aes(fill = Activity, order = Activity), position = "fill")  +
  scale_fill_manual(values = rev(brewer.pal(3, "Pastel1")),
                    name = "Activity") +
  scale_y_continuous("Percent", labels = percent) +
  ylab('Proportion of day') +
  xlab('Date') +
  scale_x_date(labels = date_format("%Y-%m"),
               minor_breaks = NULL,
               breaks = date_breaks(width = "1 month")) +
#   ?scale_x_date
  theme(axis.text.x = element_text(angle = 90, size = 10, hjust = 0.5, vjust = 0.5),
        axis.text = element_text(colour = 'black', size = 10),
        axis.title = element_text(face = "bold",
                                  colour = 'black', size = 14),
        strip.text.x = element_text(size = 10, face = "italic"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold")) 

# dev.off()

# Plot some more figures ------

# Proportion of day wet
plot(day.dat$day.prop.wet~day.date)
# Plot a smoothed line through data. To make it less smooth, set the span to a smaller number (bigger number for more smooth)
lines(loess.smooth(as.numeric(day.date), day.dat$day.prop.wet , span=.1),
      lwd = 2, col = "red")

# Plot duration of longest wet events
wet.max.time.hour <- day.dat$day.max.wet*600/60/60
plot(wet.max.time.hour~day.date)
lines(loess.smooth(as.numeric(day.date), wet.max.time.hour , span=.1),
      lwd = 2, col = "red")

# Plot duration of longest dry events
dry.max.time.hour <- day.dat$day.max.dry*600/60/60
plot(dry.max.time.hour~day.date)
# Add date labels
axis.Date(side = 1, at = seq(min(day.date), max(day.date), by = "month"), format = "%Y-%m-%d", las = 2)
# ?axis.Date
lines(loess.smooth(as.numeric(day.date), dry.max.time.hour , span=.1),
      lwd = 2, col = "red")

