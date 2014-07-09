plot3 <- function () {
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
  png(filename="plot3.png",
      bg="white")
  
  #plot the line graph for each sub_metering column
  plot(filt$Time,filt$Sub_metering_1,main="",xlab="",
       ylab="Energy sub metering",type="n")
  lines(filt$Time,filt$Sub_metering_1,type="l",col="black")
  lines(filt$Time,filt$Sub_metering_2,type="l",col="red")
  lines(filt$Time,filt$Sub_metering_3,type="l",col="blue")
  legend("topright",legend=c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"),
         col=c("black","red","blue"),lty=1)
  
  #close the png pipe
  dev.off()
}