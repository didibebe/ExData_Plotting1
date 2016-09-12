## Reads the rows to plot from the file
## Requires library sqldf
readinfilecolumns <- function()
{
  dataOfInterest <- "./module_4_quiz_week_1/household_power_consumption.txt"
  fileGenerated <- file(dataOfInterest)
  # filter for only those records with the appropriate date
  filteredData <- sqldf("select * from fileGenerated where date == '1/2/2007' or date == '2/2/2007'",
              file.format = list(header = TRUE, sep = ";"))
  close(fileGenerated)
  # get rid of "?" in the records and replace with "NA"
  filteredData[filteredData=="?"]=NA
  # transfer the Date and Time columns into a new Column DateTime with POSIXct format
  filteredData$DateTime <- as.POSIXct(paste(filteredData$Date, filteredData$Time), format="%d/%m/%Y %H:%M:%S")
  return(filteredData)
}

## draw the multiple plot (Scatterplot) to screen device and save into png-file
drawScatterplotMultiple <- function()
{
  # read the appropriate data into a dataframe
  dataForScatterplot <- readinfilecolumns()
  # draw the scatterplot to screen device with the first x,y-values
  # connect the points with the points hidden
  plot(dataForScatterplot$DateTime, dataForScatterplot$Sub_metering_1, 
       xlab = " ", ylab = "Energy sub metering", type="o",
       pch = 16, cex = 0)
  # add the points for the second plot within the first plot
  points(dataForScatterplot$DateTime, dataForScatterplot$Sub_metering_2, 
         xlab = " ", ylab = "Energy sub metering", type="o", col = "red",
         pch = 16, cex = 0)
  # add the points for the third plot within the first plot
  points(dataForScatterplot$DateTime, dataForScatterplot$Sub_metering_3, 
         xlab = " ", ylab = "Energy sub metering", type="o", col = "blue",
         pch = 16, cex = 0)
  # add the legend
  legend("topright", c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
         lty = c(1,1,1), col = c("black", "red", "blue"))
  # copy the content of the screen device into png-file
  dev.copy(png,'plot3.png')
  dev.off()
}
