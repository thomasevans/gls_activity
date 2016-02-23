# Make an activity plot from a GLS activity (*.act) file

# Install required packages (if you don't have):
install.packages(c("ggplot2", "maptools", "scales", "lubridate"))

# Set the working directory (where your files are located)
setwd("")

# Source the function 'activ.plot'
source("activ_plot2_broken.R")


# NB to see accepted arguments for 'time_zone' run the following:
OlsonNames()

# Run plot function
activ.plot(act_file = "gls10049_20120625_000.act",
           trn_file = "gls10049_20120625_000_thresh_10.trn",
           trn_true = FALSE,
           col.wet = "black", col.dry = "white",
           time_zone = "EST", long = 18.0, lat = 57.25)

# timezone thing doesn't seem to be working properly - time wrong for sunrise/set, and
# not wrapping properly for either.

# Notes
# act_file - specify the file name
# trn_file - transition file (sunrise/ sunset times)
# trn_true - TRUE if you want the sunrise/ sunset lines to use your 'transition' time data
# if FALSE uses the given location instead and give sunrise sunset times for there.
# col.wet/ col.dry, specify the colours you want for wet and dry, default is light-blue for wet and dark-blue for dry
# time_zone - default is UTC/ GMT. Can use e.g. 'EST' (Eastern Standard Time), or many others
# long/lat - location for which you want sunrise an sunset times calculated

