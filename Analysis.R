## This is the analysis for Coursera DataScience Class7 Regression Model Final Project
## Charles 08/15/2017

# Set working directory
setwd("C:/Study/Coursera/1 Data-Science/2 RStudio/7 Class 7/Coursera_DataScience_Class7Regression_FinalProject")
library(UsingR)
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
library(manipulate)
library(olsrr)

# Load data
data(mtcars)
str(mtcars)
head(mtcars)

# Explotary plot
g <- ggplot(mtcars, aes(x=factor(am), y=mpg, fill = factor(am))) +
        geom_boxplot() +
        xlab('Transmission (0=automatic, 1=manual)') +
        ylab('Miles Per Gallon (MPG)')
g

# Fit linear model for am
fitAm <- lm(mpg ~ am, data = mtcars)
summary(fitAm)$coef
summary(fitAm)$adj.r.squared

# Fit linear model for all
fitAll <- lm(mpg ~ ., data = mtcars)
summary(fitAll)$coef
corCars <- cor(mtcars)
data.frame(Cor.mpg = corCars[,which(names(mtcars)=='mpg')], Cor.wt = corCars[,which(names(mtcars)=='wt')])

# Fit try different models
fitTry1 <- lm(mpg ~ wt, data = mtcars)
summary(fitTry1)$coef
summary(fitTry1)$adj.r.squared

fitTry2 <- lm(mpg ~ qsec, data = mtcars)
summary(fitTry2)$coef
summary(fitTry2)$adj.r.squared

fitTry3 <- lm(mpg ~ disp, data = mtcars)
summary(fitTry3)$coef
summary(fitTry3)$adj.r.squared

fit1 <- lm(mpg ~ wt, data = mtcars)
fit2 <- update(fit1, mpg ~ wt + qsec)
fit3 <- update(fit1, mpg ~ wt + qsec + am)
anova(fit1,fit2,fit3)

# Fit new linear model
fitNew <- lm(mpg ~ am + qsec + wt, data = mtcars)
summary(fitNew)$coef
summary(fitNew)$adj.r.squared
par(mfrow = c(2,2))
plot(fitNew)





































