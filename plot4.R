## Reads the rows to plot from the file
## Requires library sqldf
readinfilecolumns <- function()
{
  dataOfInterest <- "./Electric Power Consumption/household_power_consumption.txt"
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

## Plots the energy submetering plot
energysubmetering_plot <- function(dataForPlots)
{
  # draw the scatterplot to screen device with the first x,y-values
  # connect the points with the points hidden
  plot(dataForPlots$DateTime, dataForPlots$Sub_metering_1, 
       xlab = " ", ylab = "Energy sub metering", type="o",
       pch = 16, cex = 0)
  # add the points for the second plot within the first plot
  points(dataForPlots$DateTime, dataForPlots$Sub_metering_2, 
         xlab = " ", ylab = "Energy sub metering", type="o", col = "red",
         pch = 16, cex = 0)
  # add the points for the third plot within the first plot
  points(dataForPlots$DateTime, dataForPlots$Sub_metering_3, 
         xlab = " ", ylab = "Energy sub metering", type="o", col = "blue",
         pch = 16, cex = 0)
  # add the legend
  legend("topright", c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
         lty = c(1,1,1), col = c("black", "red", "blue"), bty = "n")
}

## Plots the global active power plot
globalactivepower_plot <- function(dataForPlots)
{
  plot(dataForPlots$DateTime, dataForPlots$Global_active_power, 
       xlab = " ", ylab = "Global Active Power", type="o",
       pch = 16, cex = 0)
}

## Plots the voltage plot
voltage_plot <- function(dataForPlots)
{
  plot(dataForPlots$DateTime, dataForPlots$Voltage, 
       xlab = "datetime", ylab = "Voltage", type="o",
       pch = 16, cex = 0)
}

## Plots the global reactive power plot
globalreactivepower_plot <- function(dataForPlots)
{
  plot(dataForPlots$DateTime, dataForPlots$Global_reactive_power, 
       xlab = "datetime", ylab = "Global_reactive_Power", type="o",
       pch = 16, cex = 0)
}

## draw multiple plots (Scatterplots) to screen device and save into png-file
drawMultiplePlots <- function()
{
  dataForPloting <- readinfilecolumns()
  # 2x2 matrix of plots
  par(mfrow=c(2,2)) 
  # read the appropriate data into a dataframe and make the global active power plot
  globalactivepower_plot(dataForPloting)
  # read the appropriate data into a dataframe and make the voltage plot
  voltage_plot(dataForPloting)
  # read the appropriate data into a dataframe and make the energy submetering plot
  energysubmetering_plot(dataForPloting)
  # read the appropriate data into a dataframe and make the global reactive power plot
  globalreactivepower_plot(dataForPloting)
  # copy the content of the screen device into png-file
  dev.copy(png,'plot4.png')
  dev.off()
}
