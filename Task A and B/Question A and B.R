# *** Task A ***

# Import Dataset
economic <- read.csv("C:/Users/Batch 27 BA/CIS6008-JULY- 2024-Dataset/Question-(a)/economic_status.csv")
economic

#GGPlot Library
library (ggplot2)

# Perform Shapiro tests and histograms available for Consumption and other factors

# Shapiro Test for Consumption
shapiro.test(economic$Consumption)
hist(economic$Consumption, prob = TRUE, main = "Consumption Histogram")
curve(dnorm(x, mean = mean(economic$Consumption), sd = sd(economic$Consumption)), add = TRUE)

# Shapiro Test for Inflation Rate
shapiro.test(economic$Inflation_Rate)
hist(economic$Inflation_Rate, prob = TRUE, main = "Inflation Rate Histogram")
curve(dnorm(x, mean = mean(economic$Inflation_Rate), sd = sd(economic$Inflation_Rate)), add = TRUE)

# Shapiro Test for Interest Rate
shapiro.test(economic$Interest_Rate)
hist(economic$Interest_Rate, prob = TRUE, main = "Interest Rate Histogram")
curve(dnorm(x, mean = mean(economic$Interest_Rate), sd = sd(economic$Interest_Rate)), add = TRUE)

# Shapiro Test for Unemployment Rate
shapiro.test(economic$Unemployment_Rate)
hist(economic$Unemployment_Rate, prob = TRUE, main = "Unemployment Rate Histogram")
curve(dnorm(x, mean = mean(economic$Unemployment_Rate), sd = sd(economic$Unemployment_Rate)), add = TRUE)

# Shapiro Test for GDP
shapiro.test(economic$GDP)
hist(economic$GDP, prob = TRUE, main = "GDP Histogram")
curve(dnorm(x, mean = mean(economic$GDP), sd = sd(economic$GDP)), add = TRUE)



# *** Hypothesis Based Correlation Testing ***

# Correlation Test Between Consumption and Inflation Rate
cor.test(economic$Inflation_Rate,economic$Consumption)

# Plot for Consumption and Inflation Rate
plot(economic$Inflation_Rate,economic$Consumption,
     
     xlab = "Inflation Rate", 
     ylab = "Consumption",
     main = "Consumption and Inflation Rate", 
     pch = 16)


# Correlation Test Between Consumption and Interest Rate
cor.test(economic$Interest_Rate,economic$Consumption)

# Plot for Consumption and Interest Rate
plot(economic$Interest_Rate,economic$Consumption,
     
     xlab = "Interest Rate", 
     ylab = "Consumption",
     main = "Consumption and Interest Rate", 
     pch = 16)


# Correlation Test Between Consumption and Unemployment Rate
cor.test(economic$Unemployment_Rate,economic$Consumption)

# Plot for Consumption and Unemployment Rate
plot(economic$Unemployment_Rate,economic$Consumption,
     
     xlab = "Unemployment Rate", 
     ylab = "Consumption",
     main = "Consumption and Unemployment Rate", 
     pch = 16)


# Correlation Test Between Consumption and GDP
cor.test(economic$GDP,economic$Consumption)

# Plot for Consumption and GDP
plot(economic$GDP,economic$Consumption,
     
     xlab = "GDP", 
     ylab = "Consumption",
     main = "Consumption and GDP", 
     pch = 16)


#GGally Library
library(GGally)

# Correlation Matrix
data = data.frame(economic$GDP,economic$Unemployment_Rate,economic$Interest_Rate,economic$Inflation_Rate)

ggpairs(data, columnLabels = c("GDP", "Unemployment Rate", "Interest Rate", "Inflation Rate"))



# *** Task B ***

# Linear Regression for Consumption and Inflation Rate
LinReg1 <- lm(economic$Consumption ~ economic$Inflation_Rate)
LinReg1

plot(economic$Inflation_Rate,economic$Consumption,
     
     xlab = "Inflation Rate",
     ylab = "Consumption",
     main = "Check for Linearity",
     pch = 16)
abline(LinReg1)
summary(LinReg1)


# Linear Regression for Consumption and Interest Rate
LinReg2 <- lm(economic$Consumption ~ economic$Interest_Rate)
LinReg2

plot(economic$Interest_Rate,economic$Consumption,
     
     xlab = "Interest Rate",
     ylab = "Consumption",
     main = "Check for Linearity",
     pch = 16)
abline(LinReg2)
summary(LinReg2)


# Linear Regression for Consumption and Unemployment Rate
LinReg3 <- lm(economic$Consumption ~ economic$Unemployment_Rate)
LinReg3

plot(economic$Unemployment_Rate,economic$Consumption,
     
     xlab = "Unemployment Rate",
     ylab = "Consumption",
     main = "Check for Linearity",
     pch = 16)
abline(LinReg3)
summary(LinReg3)


# Linear Regression for Consumption and GDP
LinReg4 <- lm(economic$Consumption ~ economic$GDP)
LinReg4

plot(economic$GDP,economic$Consumption,
     
     xlab = "GDP",
     ylab = "Consumption",
     main = "Check for Linearity",
     pch = 16)
abline(LinReg4)
summary(LinReg4)
