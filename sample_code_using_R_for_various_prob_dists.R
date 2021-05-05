# Course - Using probability distributions for real world problems in R
# Instructor - Dr. Nikunj Maheshwari

library(ggplot2)

#-------------------------------------------------------------------------------
# Binomial distribution

# Problem
# Suppose there are ten multiple choice questions in an English class quiz. 
# Each question has four possible answers, and only one of them is correct.
# 
# Q1. Find the probability of having exactly 4 correct answers by random 
# Solution

dbinom(x=4, size = 10, prob=0.25)#Exactly 4

# Q2. What is the probability of having four or less correct answers if a 
#     student attempts to answer every question at random?
# Solution
pbinom(q=4,size=10,prob = 0.25)#r or less

k <- seq(0,10,1)
probTable <- data.frame(answer=k, prob=dbinom(k,10,0.25))

ggplot(data=probTable, aes(x=answer,y=prob)) +
  geom_bar(stat="identity", fill="blue") +
  scale_x_continuous(breaks = k) +
  theme_bw()
#-------------------------------------------------------------------------------
# Poisson distribution

# Problem
# If there are ten cars crossing a bridge per minute on average, find the 
# probability of having seventeen or more cars crossing the bridge in a 
# particular minute.
# Solution


ppois(16, lambda = 10) # this is Probability of 16 or less cars crossing
(1- ppois(16, lambda = 10))# this is probability of 17 or more
#ALTERNATIVELY
ppois(16, lambda = 10, lower=FALSE)


cars <- seq(10,20,1)
prob <- dpois(cars, lambda = 10)

ggplot() + geom_step(aes(x=cars,y=prob)) +
  scale_x_continuous(breaks = cars) +
  theme_bw()
 
#-------------------------------------------------------------------------------
# Normal distribution

# Problem
# The Iris dataset in R contains iris flower specifications. The column 
# Sepal Width follows a normal distribution.
#
# Poisson needs a Lambda, Normal needs a Mean and Std. Dev
# Q1. What is the percentage of flowers with Sepal Width 3cm or less?
# Solution

meanIris <- mean(iris$Sepal.Width)
sdIris <- sd(iris$Sepal.Width)
pnorm(3, mean=meanIris, sd= sdIris)
# Q2. What fraction of the flowers where Sepal Width lies between 3cm and 4cm?
# Solution
pnorm(4, mean=meanIris, sd= sdIris) - pnorm(3, mean=meanIris, sd= sdIris)

sepal_width <- seq(1,5,0.1)
prob <- dnorm(sepal_width, mean=meanIris, sd= sdIris)


ggplot() + geom_point(aes(x=sepal_width,y=prob)) 
  theme_bw()

#-------------------------------------------------------------------------------
# Exponential distribution
#
# Use to measure Time Lapsed between events
# Require Rate
#
# Q1. Suppose the mean arrival time of a customer at a supermarket cashier is 
# four minutes. Find the probability of a customer arriving at the cashier 
# in less than three minutes.
# Solution

#Note: Rate = 1/4
pexp(3, rate=1/4)  

# Q2. The number of days ahead travelers buy their flight tickets follows an 
# exponential distribution with the average number of days = 15. 
# Find the probability that a traveler will purchase a ticket more than 
# 10 days in advance.
# Solution
1-pexp(10, rate=1/15)

days <- seq(1,200,1)
prob <- pexp(days,  rate=1/15)

ggplot() + geom_point(aes(x=days,y=prob)) 
theme_bw()
#-------------------------------------------------------------------------------
# Chi-Square distribution
#
# Need degrees fo freedom and observed and expected std. dev
# lower value means more correlation
#
# Problem Statement
# Say, on average, a battery lasts 50 minutes on a single charge. The standard 
# deviation is 3 minutes. Suppose the battery manufacturer runs a quality 
# control test. They randomly select 9 batteries. The standard deviation of 
# the selected batteries is 5 minutes. 
# Q1. What would be the chi-square statistic represented by this test?
# Solution
dof <- 8
chi <- (dof *5^2)/3^2
chi



# Q2. Suppose they repeated the test with a new random sample of 9 batteries. 
# What is the probability that the standard deviation in the new test would be 
# greater than 5 minutes?
# Solution
1- pchisq(chi, df = dof)
chi_sq <- seq(1,40,1)
prob <- pchisq(chi_sq, df = dof)
ggplot() + geom_point(aes(x=chi_sq,y=prob)) 
theme_bw()

#-------------------------------------------------------------------------------
### End of course.
### Thank you for pursuing it.