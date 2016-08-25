# George Williams
# 25-Aug-2016
# Script for Coursera course "Exploratory Data Analysis", Week 1
# Script file:  plot4.R
png_file <- "plot4.png"
#================================80 cols wide==================================#
### Preliminaries:  get and unzip the data:
# get the data file
data_src <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
data_name <- "household_power_consumption"
data_targ <- paste0(data_name,".zip")
if(!file.exists(data_targ)){
    download.file(data_src, data_targ)
}
# unzip the data
data_file <- paste0(data_name,".txt")
if(!file.exists(data_file)){
    unzip(data_targ)
}

### data file examination from the command line:
# > head -4 household_power_consumption.txt
# Date;Time;Global_active_power;Global_reactive_power;Voltage;Global_intensity;Sub_metering_1;Sub_metering_2;Sub_metering_3
# 16/12/2006;17:24:00;4.216;0.418;234.840;18.400;0.000;1.000;17.000
# 16/12/2006;17:25:00;5.360;0.436;233.630;23.000;0.000;1.000;16.000
# 16/12/2006;17:26:00;5.374;0.498;233.290;23.000;0.000;2.000;17.000
# >
# > wc -l household_power_consumption.txt
# 2075260 household_power_consumption.txt

library(dplyr)
library(lubridate)

### inconsistent time zones have been giving me a problem with date conversion
# > OlsonNames()[599]
# [1] "UTC"
# > OlsonNames()[589]
# [1] "US/Central"

TZ <- OlsonNames()[589]
Sys.setenv(TZ=TZ)
# > Sys.timezone()
# [1] "US/Central"

# We will only be using data from the dates 2007-02-01 and 2007-02-02
begin <- as.POSIXct(ymd_hms("2007-02-01 00:00:00", tz=TZ))
end <- as.POSIXct(ymd_hms("2007-02-03 00:00:00", tz=TZ))

dbg <- 0
# a short file for quick debugging turnaround
if(dbg){
    system(paste0("head ",data_file," > dum"))
    data_file <-"dum"
    begin <- as.POSIXct(ymd_hms("2006-12-16 17:27:00", tz=TZ))
    end <- as.POSIXct(ymd_hms("2006-12-16 17:30:00", tz=TZ))
}

hpc <- read.table(data_file,header=TRUE,sep=";",na.strings="?",
                  stringsAsFactors=FALSE) %>%
       mutate(Date = dmy(Date,tz=TZ)) %>%
       mutate(Time = hms(Time)) %>%
       mutate(Time = as.POSIXct(Date)+(Time)) %>%
       rename(DateTime = Time) %>%
       filter(DateTime >= begin & DateTime < end)

#================================80 cols wide==================================#
op <- par(mfrow=c(2,2))
print_png <- 0
if(print_png){
    png(file = png_file,
        width = 480,
        height = 480
        )
}

with(hpc,{
    # plot 1
    plot(DateTime, Global_active_power,
         type = "l",
         xlab = "",
         ylab = "Global Active Power"
    )
    # plot 2
    plot(DateTime, Voltage,
         type = "l",
         xlab = "datetime"
    )
    # plot 3
    plot(DateTime, Sub_metering_1,
             type = "l",
             xlab = "",
             ylab = "Energy sub metering"
             )
         lines(DateTime, Sub_metering_2,
              type = "l",
              col = "red"
              )
         lines(DateTime, Sub_metering_3,
               type = "l",
               col = "blue"
         )
         legend("topright",
                lty = 1,
                col = c("black", "red", "blue"),
                legend = c("Sub_metering_1",
                           "Sub_metering_2",
                           "Sub_metering_3"),
                bty = "n",
                inset = c(0.15, 0.0)
                )
         # plot 4
         plot(DateTime, Global_reactive_power,
              type = "l",
              xlab = "datetime"
         )
         
         }
     )

if(print_png){
    dev.off()
}

print_png <- 1
if(print_png){
    dev.print(png,
              file = png_file,
              width = 480,
              height = 480
    )
    dev.off()
}


par <- op
