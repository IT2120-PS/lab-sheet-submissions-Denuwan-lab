#setting Directry
setwd("C:\\Users\\DENUWAN\\Desktop\\IT24103031")
getwd()
#Exercise 2
#Question 1
#Part 1
#Binomial Distribution
#Here, random variable X has binomial distribution with n = 50 and p=0.85

#Part 2
#What is the probability that at least 47 students passed the test?
dbinom(47,50,0.85)

#Question 2
#Part 1
#What is the random variable (X) for the problem?
#---(X) is the number of customer calls received in one hour.

#Part2
#What is the distribution of X?
#Here, random variable X has poisson distribution with lamda = 12

#Part 3
#What is the probability that exactly 15 calls are received in an hour?
dpois(15,12)

