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

# Create plot dataset; sub-meter Rows 7:9
plot.data <- raw[startDate:endDate, 7:9]

# Sanity Check
totalMins <- length(plot.data[,1])
dailyMins <- totalMins / 2
hourlyMins <- dailyMins / 24

if(hourlyMins == 60)  # Should Return 60
        # Remove raw for space savings
        rm(raw)

#################################  [Plot 3]  ####################################
##  Title:      NULL
##  x-axis:     ""  scale: Thursday, Friday, Saturday 
##  y-axis:     "Energy sub metering" scale 0-30 / 10
##  type:       Line 
##  color:      Black, Red, Blue
##  key:        Title: NULL     Labels: "Sub_metering_#" colnames
colors = c('black', 'red', 'blue')
range <- range(plot.data)
png("plot3.png", 480, 480)
plot(plot.data[[1]],
     xaxt = "n",
     xlab = "",
     ylab = "Energy sub metering",
     type = 'l', col = colors[1])
lines(plot.data[[2]], type="l", col = colors[2])
lines(plot.data[[3]], type="l", col = colors[3])

# add x-axis 
axis(side = 1, at = c(0, 1440, 2880), labels = c("Thursday", "Friday", "Saturday"))

# add a legend 
legend("topright", legend = colnames(plot.data), col=colors, lty = 1, cex = 0.80)
dev.off()