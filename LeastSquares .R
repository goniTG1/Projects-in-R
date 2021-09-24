

# Load data
load("least-squares-3D.RData")


# Fit model: the "+" is for including predictors in the model, not for adding them!
# Observe that x1 and x2 are variables in leastSquares3D
mod <- lm(yLin ~ x1 + x2, data = leastSquares3D)
mod


Y <- leastSquares3D$yLin
X <- cbind(1, leastSquares3D$x1, leastSquares3D$x2)
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y
drop(beta_hat)
mod$coefficients



h_mat = X%*%solve(t(X)%*%X)%*%t(X)
y_hat = h_mat%*%Y
all.equal(mod$fitted.values, as.vector(y_hat))
mod$fitted.values
as.vector(y_hat)


mod_x1 = lm(yLin ~ x1, data = leastSquares3D)
mod_x2 = lm(yLin ~ x2, data = leastSquares3D)

car::compareCoefs(mod, mod_x1, mod_x2)


summary(mod)
# x1 is highly significant for the regression: H0 is rejected for all
# sensible significance levels
# x2 is less significant: H0 will be rejected for alpha > 0.028831 but, 
# for example, not for alpha = 0.01. Therefore there is less evidence,
# when compared with x2, that supports that the linear effect of x2 on
# yLin is significant


confint(mod, level = 0.90)
confint(mod, level = 1-0.02831)

predict(mod, newdata = data.frame(x1 =-1, x2 = 1),
        interval = 'confidence', level = 0.9)


# This is *NOT* the ANOVA table we have seen
anova(mod)

# This function computes the simplified anova from a linear model
simpleAnova <- function(object, ...) {
  
  # Compute anova table
  tab <- anova(object, ...)
  
  # Obtain number of predictors
  p <- nrow(tab) - 1
  
  # Add predictors row
  predictorsRow <- colSums(tab[1:p, 1:2])
  predictorsRow <- c(predictorsRow, predictorsRow[2] / predictorsRow[1])
  
  # F-quantities
  Fval <- predictorsRow[3] / tab[p + 1, 3]
  pval <- pf(Fval, df1 = p, df2 = tab$Df[p + 1], lower.tail = FALSE)
  predictorsRow <- c(predictorsRow, Fval, pval)
  
  # Simplified table
  tab <- rbind(predictorsRow, tab[p + 1, ])
  row.names(tab)[1] <- "Predictors"
  return(tab)
  
}

# Simplified ANOVA
anova_tab <- simpleAnova(mod)
anova_tab$`Mean Sq`[2]

# i

sigma2_hat = anova_tab$`Mean Sq`[2]

# j

m = summary(mod)
m$r.squared
m$adj.r.squared
# R^2
cov(mod$coefficients)

SST = anova_tab$`Mean Sq`[1]

r_squared = 1-sigma2_hat/sum(anova_tab$`Sum Sq`) * anova_tab$Df[2]

all.equal(r_squared, m$r.squared)

# R Adj
m$adj.r.squared
r_sq_2 = 1- sigma2_hat / sum(anova_tab$`Mean Sq`)*anova_tab$Df[1]
all.equal(r_sq_2, m$adj.r.squared)




# Read data
wine <- read.csv("wine.csv", header = TRUE)


# Regression on all the predictors
modWine1 <- lm(Price ~ Age + AGST + FrancePop + HarvestRain + WinterRain,
               data = wine)

# A useful shortcut
modWine1 <- lm(Price ~ ., data = wine)
modWine1

# Summary
sumModWine1 <- summary(modWine1)
sumModWine1

# Contains the estimation of sigma ("Residual standard error")
sumModWine1$sigma

# The fitted regression is 
# Price = ??? 2.343 + 0.013 ? Age + 0.614 ? AGST
#         - 0.000 ? FrancePop ??? 0.003 ? HarvestRain
#         + 0.001 ? WinterRain
# Recall that the 'Multiple R-squared' has almost doubled with respect to
# the best simple linear regression! This tells us that combining several
# predictors may lead to important performance gains in the prediction of
# the response. However, note that the R2 of the multiple linear model
# is not the sum of the R2's of the simple linear models

# Exclude Year
modWine2 <- lm(Price ~ . - Year, data = wine)
summary(modWine2)

# exclude Age, FrancePop large p-value 
modWine3 <- lm(Price ~  WinterRain + HarvestRain + AGST, data = wine)
summary(modWine3)
# may be done in a different way
modWine3_b <- lm(Price ~ . -(Year + FrancePop + WinterRain),
                 data = wine)

summary(modWine3_b)

confint(modWine3_b)



modWine4 <- lm(Price ~ WinterRain + HarvestRain + AGST,
               data = wine)

p = predict(modWine4, 
        newdata = data.frame(WinterRain = 500,
                             HarvestRain = 123, 
                             AGST = 17))
confint(modWine4)
# Find a way to compute the Confidence Interval





rm(list = ls())

# Generate data
p <- 198
n <- 200
set.seed(3456732)
beta <- c(0.5, -0.5, rep(0, p - 2))
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
Y <- drop(X %*% beta + rnorm(n, sd = 3))
data <- data.frame(y = Y, x = X)
# Regression on the two meaningful predictors
summary(lm(y ~ x.1 + x.2, data = data))
# Adding 20 garbage variables
# R^2 increases and adjusted R^2 decreases
summary(lm(y ~ X[, 1:22], data = data))




# Model with intercept
mod1 <- lm(Sepal.Length ~ Petal.Width, data = iris)
mod1
# Model without intercept
mod0 <- lm(Sepal.Length ~ 0 + Petal.Width, data = iris)
mod0
# Recall the different way of obtaining the estimators
X1 <- cbind(1, iris$Petal.Width)
X0 <- cbind(iris$Petal.Width) # No column of ones!
Y <- iris$Sepal.Length
(betaHat1 <- solve(crossprod(X1)) %*% t(X1) %*% Y)
(betaHat0 <- solve(crossprod(X0)) %*% t(X0) %*% Y)



# Summaries: higher R^2 for the model with no intercept!?
summary(mod1)
summary(mod0)
# Wait a moment... let's see the actual fit
plot(Sepal.Length ~ Petal.Width, data = iris)
abline(mod1, col = 2) # Obviously, much better
abline(mod0, col = 3)
# Manually checking the R^2 indeed reveals that summary is doing something
# different for computing the R^2 when no intercept
c= cor(mod0$model$Sepal.Length, mod0$fitted.values)^2
m =summary(mod1)
all.equal(c, m$r.squared) # hehe noice

# Compute the R^2 manually for mod1
SSE1 <- sum((mod1$residuals - mean(mod1$residuals))^2)
SST1 <- sum((mod1$model$Sepal.Length - mean(mod1$model$Sepal.Length))^2)
1 - SSE1 / SST1

# Compute the R^2 manually for mod0
SSE0 <- sum((mod0$residuals - mean(mod0$residuals))^2)
SST0 <- sum((mod0$model$Sepal.Length - mean(mod0$model$Sepal.Length))^2)
1 - SSE0 / SST0
# It is negative!
# Recall that the mean of the residuals is not zero
mean(mod0$residuals)
# What summary really returns if there is no intercept
n <- nrow(iris)
p <- 1
R0 <- 1 - sum(mod0$residuals^2) / sum(mod0$model$Sepal.Length^2)
R0Adj <- 1 - sum(mod0$residuals^2) / sum(mod0$model$Sepal.Length^2) * (n - 1) / (n - p - 1)
R0
R0Adj

# What if we centred the data previously?

irisCen <- data.frame(scale(iris[, -5], center = TRUE, scale = FALSE))
modCen1 <- lm(Sepal.Length ~ Petal.Width, data = irisCen)
modCen0 <- lm(Sepal.Length ~ 0 + Petal.Width, data = irisCen)

par('mar')
par(mar = c(1,1,1,1))
par(mfrow = c(1,1))
plot(iris[, 1:2])
plot(irisCen[, 1:2], col = 2)

# No problem, "correct" R^2
summary(modCen1)
summary(modCen0)
# But only if we center predictor and response...
summary(lm(iris$Sepal.Length ~ 0 + irisCen$Petal.Width))




R0_shift = function(shift){
  
  # iris$Petal.Width_shift = iris$Petal.Width + shift
  # and add that variable
  mod0 <- lm(Sepal.Length ~ 0 + 
               I(Petal.Width + shift), 
             data = iris)
  return(summary(mod0)$r.squared)
  
}

s <- seq(-10, 10, l = 200)
mod0 <- lm(Sepal.Length ~ + I(Petal.Width - 1.3), data = iris)
summary(mod0)$r.squared




plot(s, sapply(s, R0_shift), type = 'l', col = 3)
abline(v = 0.05, col = 2)
title('Shift in R0')


