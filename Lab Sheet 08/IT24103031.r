getwd()
setwd("C:/Users/Denuwan/Desktop/IT24103031")
# 1. Import the dataset using read.table
weights <- read.table("Exercise - LaptopsWeights.txt", header = TRUE)$Weight

# Calculate population mean and population standard deviation
pop_mean <- mean(weights)
n <- length(weights)
pop_sd <- sqrt(sum((weights - pop_mean)^2) / n)

cat("1. Population mean:", pop_mean, "\n")
cat("   Population standard deviation:", pop_sd, "\n\n")

# 2. Draw 25 random samples of size 6 (with replacement) and calculate sample mean and sd
set.seed(123)  # For reproducibility
sample_means <- numeric(25)
sample_sds <- numeric(25)

for(i in 1:25) {
  samp <- sample(weights, 6, replace = TRUE)
  sample_means[i] <- mean(samp)
  sample_sds[i] <- sd(samp)
}

cat("2. Sample means and standard deviations:\n")
for(i in 1:25) {
  cat("   Sample", i, ": mean =", sample_means[i], ", sd =", sample_sds[i], "\n")
}
cat("\n")

# 3. Calculate mean and sd of the 25 sample means
mean_sample_means <- mean(sample_means)
sd_sample_means <- sd(sample_means)

cat("3. Mean of sample means:", mean_sample_means, "\n")
cat("   Standard deviation of sample means:", sd_sample_means, "\n\n")

# State the relationship
cat("   Relationship:\n")
cat("   - The mean of the sample means (", mean_sample_means, ") is approximately equal to the population mean (", pop_mean, ").\n")
expected_se <- pop_sd / sqrt(6)
cat("   - The standard deviation of the sample means (", sd_sample_means, ") is approximately equal to the population standard deviation divided by sqrt(sample size) (", pop_sd, " / sqrt(6) ≈ ", expected_se, ").\n")
