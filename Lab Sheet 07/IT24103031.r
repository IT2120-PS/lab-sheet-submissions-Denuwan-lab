# Set working directory
setwd("C:\\Users\\DENUWAN\\desktop\\IT24103031")

# Exercise Question 1: Uniform Distribution

prob_q1 <- punif(25, min=0, max=40) - punif(10, min=0, max=40)
cat("Q 1: Probability that the train arrives between 8:10 a.m. and 8:25 a.m.","\n", prob_q1, "\n")

# Exercise Question 2: Exponential Distribution

prob_q2 <- pexp(2, rate=1/3)
cat("Q 2: Probability that an update takes at most 2 hours ","\n", prob_q2, "\n")

# Exercise Question 3i: Normal Distribution

prob_q3i <- 1 - pnorm(130, mean=100, sd=15)
cat("Q 3i: Probability of IQ above 130 ","\n", prob_q3i, "\n")

# Exercise Question 3ii: 95th Percentile

iq_95th <- qnorm(0.95, mean=100, sd=15)
cat("Q 3ii: IQ score for 95th percentile ","\n", iq_95th, "\n")

