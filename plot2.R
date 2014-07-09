plot2 <- function () {
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
  
  #get list of good data points (not NA)
  cc <- complete.cases(subdata$Global_active_power)
  filt <- subdata[cc,]
  
  #initiate write to png file
  png(filename="plot2.png",
      bg="white")
  
  #plot the line graph
  plot(filt$Time,filt$Global_active_power,main="",xlab="",
       ylab="Global Active Power (kilowatts)",pch=20,type="n")
  lines(filt$Time,filt$Global_active_power,type="l")
  
  #cloes the png pipe
  dev.off()
}