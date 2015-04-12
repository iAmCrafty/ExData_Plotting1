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
cols <- c(3, 4, 5, 7, 8, 9)
plot.data <- raw[startDate:endDate, cols]

# Sanity Check
totalMins <- length(plot.data[,1])
dailyMins <- totalMins / 2
hourlyMins <- dailyMins / 24

if(hourlyMins == 60)  # Should Return 60
        # Remove raw for space savings
        rm(raw)

#################################  [Plot 4]  ####################################
##  Multi-Plot  NO TITLES
png("plot4.png", 480, 480)
par(mfrow=c(2,2))
##  [Plot 4A]  Q4 Top - Left == [Plot 2*]
##      x-axis: ""          scale Thursday, Friday, Saturday 
##      y-axis: *"Global Active Power" scale 0-6 / 2
##      type:   Line Graph - Black
plot4A_data <- plot.data$Global_active_power
plot(plot4A_data,
     xaxt = 'n',
     xlab = "",
     ylab = 'Global Active Power',
     type = 'l')

axis(side = 1, at = c(0, 1440, 2880), labels = c("Thursday", "Friday", "Saturday"))

##    [Plot 4B]  Q1 Top - Right 
##        x-axis: "datetime"    scale Thursday, Friday, Saturday 
##        y-axis: "Voltage"     scale 234 - 246  / 4
##        type: Line Graph - Black
plot4B_data <- plot.data$Voltage
plot(plot4B_data,
     xaxt = 'n',
     xlab = "datetime",
     ylab = 'Voltage',
     type = 'l')
axis(side = 1, at = c(0, 1440, 2880), labels = c("Thursday", "Friday", "Saturday"))

##    [Plot 4C]  Q4 Bottom  - Left == [Plot 3]
plot4C_data <- plot.data[,4:6]
colors = c('black', 'red', 'blue')
range <- range(plot4C_data)

plot(plot4C_data[[1]],
     xaxt = "n",
     xlab = "",
     ylab = "Energy sub metering",
     type = 'l', col = colors[1])
lines(plot4C_data[[2]], type="l", col = colors[2])
lines(plot4C_data[[3]], type="l", col = colors[3])

# add x-axis 
axis(side = 1, at = c(0, 1440, 2880), labels = c("Thursday", "Friday", "Saturday"))

# add a legend 
legend("topright", legend = colnames(plot4C_data), col=colors, lty = 1, 
       cex = 0.75)

##    [Plot 4D]  Q1 Botttom - Right == [Plot 2*]
##        x-axis: "datetime"    scale Thursday, Friday, Saturday 
##        y-axis: *"Global_reactive_power" scale 0.0 - 0.5 / 0.1
##        type: Line Graph - Black
plot4D_data <- plot.data$Global_reactive_power
plot(plot4D_data,
     xaxt = 'n',
     xlab = "datetime",
     ylab = 'Global_reactive_power',
     type = 'l')
axis(side = 1, at = c(0, 1440, 2880), labels = c("Thursday", "Friday", "Saturday"))
dev.off()