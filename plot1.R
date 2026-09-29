# Read data (sep = ";", missing values coded as "?") and keep 2007-02-01 and 2007-02-02
data <- read.table("household_power_consumption.txt", header = TRUE, sep = ";",
                   na.strings = "?", stringsAsFactors = FALSE)
data <- data[data$Date %in% c("1/2/2007", "2/2/2007"), ]
data$datetime <- strptime(paste(data$Date, data$Time), "%d/%m/%Y %H:%M:%S")

# Plot 1: histogram of Global Active Power
png("plot1.png", width = 480, height = 480)
hist(data$Global_active_power, col = "red", main = "Global Active Power",
     xlab = "Global Active Power (kilowatts)")
dev.off()
