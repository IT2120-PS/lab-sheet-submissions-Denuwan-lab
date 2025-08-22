#1
setwd("c:\\Users\\IT24103031\\Desktop\\IT24103031")

branch_data <- read.table("Exercise.txt", header = TRUE, sep=",")

head(branch_data)

# 2.
#structure of the data
str(branch_data)

# Get a summary of the data (numeric summaries and data type)
summary(branch_data)

# 3.
# Boxplot for Sales
boxplot(branch_data$Sales_X1, 
        outline = TRUE,
        outpch=8,
        horizontal=TRUE,
        main = "sales distribution")

# 4.
# Five number summary, min, max, q1, q2 ,q3
summary(branch_data$Advertising)

# Calculate IQR
IQR_advertising <- IQR(branch_data$Advertising)
IQR_advertising # disaply iqr

# 5.
# Function to find outliers
find_outliers <- function(years) {
  Q1 <- quantile(years)[2]
  Q3 <- quantile(years)[4]
  iqr <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * iqr
  upper_bound <- Q3 + 1.5 * iqr
  
  outliers <- years[years < lower_bound | years > upper_bound]
  # sort the outliers
  outliers = sort(outliers)
  
  print(paste("Upper Bound : ", upper_bound))
  print(paste("Lower Bound : ", lower_bound))
  print(paste("IQR : ", iqr))
  print(paste("outliers", paste(outliers, collapse= ",")))
}

# get outliers for the 'Years' variable
find_outliers(branch_data$Years)

