##############################  RETRIEVE  ##################################
## Create colClasses for data table
setClass('myDate')
setAs('character','myDate', function(from) as.Date(from, format="%d/%m/%Y") )
colClasses <- c(
        'myDate', 'character', 'numeric', 
        'numeric', 'numeric', 'numeric',
        'numeric', 'numeric', 'numeric')

## Import Raw Data
file <- c('~/R/EDA/Proj1/household_power_consumption.txt')
raw <- read.table(file, header = TRUE, sep = ";", na.strings = "?", colClasses = colClasses)

## Find Rows for extraction from Raw
dates <- as.Date(c("01/02/2007", "03/02/2007"), '%d/%m/%Y')
startDate <- match(dates[1], raw$Date)
endDate <- (match(dates[2], raw$Date)) - 1

# Create plot dataset
plot.data <- raw[startDate:endDate, 'Global_active_power']

# Sanity Check
totalMins <- length(plot.data[,1])
dailyMins <- totalMins / 2
hourlyMins <- dailyMins / 24

if(hourlyMins == 60)  # Should Return 60
        # Remove raw for space savings
        rm(raw)

#################################  [Plot 2]  ####################################
##  Title:      NULL
##  x-axis:     ""  scale: Thursday, Friday, Saturday 
##  y-axis:     "Global Active Power (kilowatts)" scale 0-6 / 2
##  type:       Line 
##  color:      Black
png("plot2.png", 480, 480)
plot(plot.data,
     xaxt = 'n',
     xlab = "",
     ylab = 'Global Active Power (kilowatts)',
     type = 'l')

axis(side = 1, at = c(0, 1440, 2880), labels = c("Thursday", "Friday", "Saturday"))
dev.off()
