#
# 1dg618
#

setwd("/Users/1dg618/coursera_data_science/ExData_Plotting1")

if(! require("data.table")) {
  install.packages("data.table")
}
library(data.table)

#
# date_time takes the date and time as the arguements
# and returns the date/time in the format day/month/year
# hour:minutes/seconds.
#
date_time <- function(date, time) {

	#
	# paste() - Concatenate vectors after converting to
	# character.
	#
	# strptime() - convert between character representations
	# and objects of classes "POSIXlt" and "POSIXct" representing
	# calendar dates and times.
	#
	return (strptime(paste(date, time), "%d/%m/%Y %H:%M:%S"))
}

#
# * The dataset has 2,075,259 rows and 9 columns.
# * We will only be using data from dates 2007-02-01 and
#   2007-02-02, which is a pretty important point.
# * We want to convert the Date and Time variables to
#   Date/Time using strptime() and as.Date().
# * Missing values are coded as '?'.
#
get_our_clean_data <- function() {

  filename <- "household_power_consumption.txt"
  zip_filename <- "household_power_consumption.zip"
  
  if(! file.exists(filename)) {
    
    if(! file.exists("household_power_consumption.zip")) {
      
      #
      # Download the zip file and unzip it.
      #
      url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
      file <- basename(url)
      download.file(url, file, method = "curl")
      
    }
    
    #
    # Unzip the file
    #
    unzip(file, exdir = getwd())
    
  }
  
	#
	# Read the data from the file into the data.table.
	#
	my_data_table <- read.table(filename,
		header=TRUE,
		sep=";",
		colClasses=c("character", "character", rep("numeric",7)),
		na="?")

	#
	# Convert date and time variables to Date/Time class
	#
	my_data_table$Time <- strptime(paste(my_data_table$Date, my_data_table$Time), "%d/%m/%Y %H:%M:%S")
	my_data_table$Date <- as.Date(my_data_table$Date, "%d/%m/%Y")

	#
	# only use data from the dates 2007-02-01 and 2007-02-02
	#
	dates <- as.Date(c("2007-02-01", "2007-02-02"), "%Y-%m-%d")

	#
	# Get the data from the data.table based on the dates.
	#
	my_data_table <- subset(my_data_table, Date %in% dates)

	return(my_data_table)

}

# *****************************************************************
#
#	PLOT 3
#
# *****************************************************************

#
#
#
plot3 <- function() {

	#
	# The function get_our_clean_data() will get
	# us the clean data we need from the file to
	# create the histogram. At this point, you
	# don't need to do anything to the data because
	# it is CLEAN.
	#
	clean_data <- get_our_clean_data()

	#
	# png() provides an easy and simple way to read,
	# write, and display bitmap images stored in the
	# PNG format. That's it.
	#
	png("plot3.png", width=480, height=480)

	#
	# Explanatory (x-axis) - date/time
	# Response (y-axis) - Sub_metering_1
	#
	# Date/time vs. Sub_metering_1
	#
	plot(clean_data$Time, clean_data$Sub_metering_1,
		type="l",
		col="black",
		xlab="",
		ylab="Energy sub metering")

	#
	# A generic function taking coordinates given in various ways
	# and joining the corresponding points with line segments. Now,
	# we want to see date/time vs. Sub_metering_2 and date/time
	# vs. Sub_metering_3. Somehow it magically knows we want it
	# on our plot(), which we did above.
	#
	lines(clean_data$Time, clean_data$Sub_metering_2, col="red")
	lines(clean_data$Time, clean_data$Sub_metering_3, col="blue")

	#
	# Explain the meaning of the colors.
	#
	# * black - Sub_metering_1
	# * red - Sub_metering_2
	# * blue - Sub_metering_3
	#
	legend("topright",
		col=c("black", "red", "blue"),
		c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
		lty=1)

	#
	# Shuts down the specified device. I guess our
	# device is png()?
	#
	dev.off()

}

