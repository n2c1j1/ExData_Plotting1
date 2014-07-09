plot4 <- function () {
  myzip = "exdata_data_household_power_consumption.zip"
  #if data file has not yet been downloaded, fetch it
  if (!file.exists(myzip)) {
    download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",
                  destfile=myzip,method="curl")
    unzip(myzip)

  }
  
  #Read the unzipped file into a data frame
  rawtab <- read.table("household_power_consumption.txt",header=TRUE,sep=";",row.names=NULL, 
                      na.strings="?",comment.char="",nrows=2075259)
  
  #make sure the time column reads as a time class
  #prepend the time with the date column, otherwise strptime assumes the current date
  rawtab$Time <- paste(as.character(rawtab$Date), as.character(rawtab$Time))
  rawtab$Time <- strptime(as.character(rawtab$Time),format="%d/%m/%Y %H:%M:%S")
  
  #make sure the date column reads as a date class
  rawtab$Date <- as.Date(as.character(rawtab$Date),format="%d/%m/%Y")
  
  #pick out the dates of interest
  subdata <- subset(rawtab,rawtab$Date=="2007-02-01" | rawtab$Date == "2007-02-02")
  
  # didn't filter the data for this plot and that seems ok
  filt <- subdata
  
  #initiate write to png file
  png(filename="plot4.png",
      bg="white")
  par(mfrow=c(2,2))
  with(filt,
      {
        #Global active power plot
        plot(filt$Time,filt$Global_active_power,main="",xlab="",
             ylab="Global Active Power (kilowatts)",type="n")
        lines(filt$Time,filt$Global_active_power,type="l")
        
        #Voltage plot
        plot(filt$Time,filt$Voltage,main="",
             ylab="Voltage",xlab="datetime",type="n",yaxt="n")
        axis(2,at=c(234,236,238,240,242,244,246),labels=c(234,"",238,"",242,"",246))
        lines(filt$Time,filt$Voltage,type="l")
        
        #Sub-metering plot
        plot(filt$Time,filt$Sub_metering_1,main="",xlab="",
             ylab="Energy sub metering",type="n")
        lines(filt$Time,filt$Sub_metering_1,type="l",col="black")
        lines(filt$Time,filt$Sub_metering_2,type="l",col="red")
        lines(filt$Time,filt$Sub_metering_3,type="l",col="blue")
        legend("topright",legend=c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"),
               col=c("black","red","blue"),lty=1,bty="n")
        
        #Global reactive power plot
        plot(filt$Time,filt$Global_reactive_power,main="",
             ylab="Global_reactive_power",xlab="datetime",type="n",yaxt="n")
        axis(2,at=c(0.0,0.1,0.2,0.3,0.4,0.5),labels=c(0.0,0.1,0.2,0.3,0.4,0.5))
        lines(filt$Time,filt$Global_reactive_power,type="l")
      }
  )
 
  #close the png pipe
  dev.off()
}