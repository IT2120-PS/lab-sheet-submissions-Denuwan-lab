
# Setting the working directory
setwd("C:/Users/DENUWAN/Desktop/IT24103031")

# read data from txt
Delivery_Times <- read.table("Exercise - Lab 05.txt", header = TRUE)

# breaks for the histogram
breaks <- seq(20, 70, by = 5)
# Create the histogram
hist(Delivery_Times$Delivery_Time_, breaks = breaks,
     right = TRUE, 
     main = "Histogram of Delivery Times", 
     xlab = "Delivery Time")



# calculate the frequency table
freq_table <- hist(Delivery_Times$Delivery_Time_, breaks = breaks, right = TRUE, plot = FALSE)

# get the cumulative frequency
cum_freq <- cumsum(freq_table$counts)

# create the cumulative frequency polygon
plot(freq_table$mids, cum_freq, type = "o",
     main = "Cumulative Frequency Polygon", 
     xlab = "Delivery Time", 
     ylab = "Cumulative Frequency")
