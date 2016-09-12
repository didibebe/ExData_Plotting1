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

## draw the first plot (Histogram) to screen device and save into png-file
drawHistogram <- function()
{
  # read the appropriate data into a dataframe
  dataForHistogramm <- readinfilecolumns()
  # draw the hisstogram to screen device
  hist(dataForHistogramm$Global_active_power, col = "red", xlab = "Global Active Power (kilowatts)", main = "Global Active Power")
  # copy the content of the screen device into png-file
  dev.copy(png,'plot1.png')
  dev.off()
}
