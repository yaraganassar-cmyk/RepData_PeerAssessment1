# Read data (sep = ";", missing values coded as "?") and keep 2007-02-01 and 2007-02-02
data <- read.table("household_power_consumption.txt", header = TRUE, sep = ";",
                   na.strings = "?", stringsAsFactors = FALSE)
data <- data[data$Date %in% c("1/2/2007", "2/2/2007"), ]
data$datetime <- strptime(paste(data$Date, data$Time), "%d/%m/%Y %H:%M:%S")

# Plot 3: Energy sub metering over time
png("plot3.png", width = 480, height = 480)
plot(data$datetime, data$Sub_metering_1, type = "l", xlab = "",
     ylab = "Energy sub metering")
lines(data$datetime, data$Sub_metering_2, col = "red")
lines(data$datetime, data$Sub_metering_3, col = "blue")
legend("topright", lty = 1, col = c("black", "red", "blue"),
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
dev.off()
