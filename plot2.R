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

## draw the first plot (Scatterplot) to screen device and save into png-file
drawScatterplot <- function()
{
  # read the appropriate data into a dataframe
  dataForScatterplot <- readinfilecolumns()
  # draw the scatterplot to screen device
  # connect the points with the points hidden
  plot(dataForScatterplot$DateTime, dataForScatterplot$Global_active_power, 
       xlab = " ", ylab = "Global Active Power (kilowatts)", type="o",
       pch = 16, cex = 0)
  # copy the content of the screen device into png-file
  dev.copy(png,'plot2.png')
  dev.off()
}
