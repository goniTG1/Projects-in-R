
rm(list = ls())


# Load data -- baseball players statistics
install.packages('ISLR')
data(Hitters, package = "ISLR")

# Discard NA's
Hitters <- na.omit(Hitters)

# The glmnet function works with the design matrix of predictors (without
# the ones). This can be obtained easily through model.matrix()
x <- model.matrix(Salary ~ 0 + ., data = Hitters)
# build matrix x from the Dataset.
# 0 + to exclude a column of 1's for the intercept, since the intercept will be
# added by default in glmnet::glmnet and if we do not exclude it here we will
# end with two intercepts, one of them resulting in NA

# Interestingly, note that in Hitters there are two-level factors and these
# are automatically transformed into dummy variables in x -- the main advantage
# of model.matrix.lm
head(Hitters[, 14:20])
head(x[, 14:19])

# We also need the vector of responses
y <- Hitters$Salary

# Call to the main function -- use alpha = 0 for ridge regression
#install.packages('glmnet')
library(glmnet)
ridgeMod <- glmnet(x = x, y = y, alpha = 0)
# By default, it computes the ridge solution over a set of lambdas
# automatically chosen. It also standardizes the variables by default to make
# the model fitting since the penalization is scale-sensitive. Importantly,
# the coefficients are returned on the original scale of the predictors

# Plot of the solution path -- gives the value of the coefficients for different
# measures in xvar (penalization imposed to the model or fitness)
par(mfcol = c(1, 1))
par(mar=c(1,1,1,1))
#rm(list = ls())
if(!is.null(dev.list())) dev.off()
cat("\014") 

plot(ridgeMod, xvar = "norm", label = TRUE)
# xvar = "norm" is the default: L1 norm of the coefficients sum_j abs(beta_j)

# Versus lambda
plot(ridgeMod, label = TRUE, xvar = "lambda")

# Versus the percentage of deviance explained -- this is a generalization of the
# R^2 for generalized linear models. Since we have a linear model, this is the
# same as the R^2
plot(ridgeMod, label = TRUE, xvar = "dev")
# The maximum R^2 is slightly above 0.5

# Indeed, we can see that R^2 = 0.5461
summary(lm(Salary ~., data = Hitters))$r.squared

# Some persistently important predictors are 16, 14, and 15
colnames(x)[c(16, 14, 15)]

# What is inside glmnet's output?
names(ridgeMod)

# lambda versus R^2 -- fitness decreases when sparsity is introduced, in
# in exchange of better variable interpretation and avoidance of overfitting
plot(log(ridgeMod$lambda), ridgeMod$dev.ratio, type = "l",
     xlab = "log(lambda)", ylab = "R2")
ridgeMod$dev.ratio[length(ridgeMod$dev.ratio)]
# Slightly different to lm's because of compromises in accuracy for speed

# The coefficients for different values of lambda are given in $a0 (intercepts)
# and $beta (slopes) or, alternatively, both in coef(ridgeMod)
length(ridgeMod$a0)
dim(ridgeMod$beta)
length(ridgeMod$lambda) # 100 lambda's were automatically chosen

# Inspecting the coefficients associated to the 50th lambda
coef(ridgeMod)[, 50]
ridgeMod$lambda[50]

# Zoom in path solution
plot(ridgeMod, label = TRUE, xvar = "lambda",
     xlim = log(ridgeMod$lambda[50]) + c(-2, 2), ylim = c(-30, 10))
abline(v = log(ridgeMod$lambda[50]))
points(rep(log(ridgeMod$lambda[50]), nrow(ridgeMod$beta)), ridgeMod$beta[, 50],
       pch = 16, col = 1:6)

# The squared l2-norm of the coefficients decreases as lambda increases
plot(log(ridgeMod$lambda), sqrt(colSums(ridgeMod$beta^2)), type = "l",
     xlab = "log(lambda)", ylab = "l2 norm")

# If we want, we can choose manually the grid of penalty parameters to explore
# The grid should be descending
ridgeMod2 <- glmnet(x = x, y = y, alpha = 0, lambda = 100:1)
plot(ridgeMod2, label = TRUE, xvar = "lambda") # Not a good choice!

# Lambda is a tuning parameter that can be chosen by cross-validation, using as
# error the MSE (other possible error can be considered for generalized models
# using the argument type.measure)

# 10-fold cross-validation. Change the seed for a different result
set.seed(12345)
kcvRidge <- cv.glmnet(x = x, y = y, alpha = 0, nfolds = 10)
# The lambda grid in which CV is done

# The lambda that minimises the CV error is
kcvRidge$lambda.min

# Equivalent to
indMin <- which.min(kcvRidge$cvm)
kcvRidge$lambda[indMin]

# The minimum CV error
kcvRidge$cvm[indMin]
min(kcvRidge$cvm)

# Potential problem! Minimum occurs at one extreme of the lambda grid in which
# CV is done. The grid was automatically selected, but can be manually inputted
range(kcvRidge$lambda)
lambdaGrid <- 10^seq(log10(kcvRidge$lambda[1]), log10(0.1),
                     length.out = 150) # log-spaced grid
kcvRidge2 <- cv.glmnet(x = x, y = y, nfolds = 10, alpha = 0,
                       lambda = lambdaGrid)

# Much better
plot(kcvRidge2)
kcvRidge2$lambda.min

# But the CV curve is random, since it depends on the sample. Its variability
# can be estimated by considering the CV curves of each fold. An alternative
# approach to select lambda is to choose the largest within one standard
# deviation of the minimum error, in order to favour simplicity of the model
# around the optimal lambda value. This is know as the "one standard error rule"
kcvRidge2$lambda.1se

# Location of both optimal lambdas in the CV loss function in dashed vertical
# lines, and lowest CV error and lowest CV error + one standard error
plot(kcvRidge2)
indMin2 <- which.min(kcvRidge2$cvm)
abline(h = kcvRidge2$cvm[indMin2] + c(0, kcvRidge2$cvsd[indMin2]))
# The consideration of the one standard error rule for selecting lambda makes
# special sense when the CV function is quite flat around the minimum (hence an
# overpenalization that gives more sparsity does not affect so much the CV loss)

# Leave-one-out cross-validation. More computationally intense but completely
# objective in the choice of the fold-assignment
ncvRidge <- cv.glmnet(x = x, y = y, alpha = 0, nfolds = nrow(Hitters),
                      lambda = lambdaGrid)

# Location of both optimal lambdas in the CV loss function
plot(ncvRidge)

# The glmnet fit is inside the output of cv.glmnet
modRidgeCV <- kcvRidge2$glmnet.fit

# Inspect the best models
plot(modRidgeCV, label = TRUE, xvar = "lambda")
abline(v = log(c(kcvRidge2$lambda.min, kcvRidge2$lambda.1se)))

# The model associated to lambda.1se (or any other lambda not included in the
# original path solution -- obtained by an interpolation) can be retrieved with
predict(modRidgeCV, type = "coefficients", s = kcvRidge2$lambda.1se)

# Predictions for the first two observations
predict(modRidgeCV, type = "response", s = kcvRidge2$lambda.1se,
        newx = x[1:2, ])

# Predictions for the first observation, for all the lambdas. We can see how
# the prediction for one observation changes according to lambda
plot(log(modRidgeCV$lambda),
     predict(modRidgeCV, type = "response", newx = x[1, , drop = FALSE]),
     type = "l", xlab = "log(lambda)", ylab = " Prediction")

# Get the Hitters data back
x <- model.matrix(Salary ~ 0 + ., data = Hitters)
y <- Hitters$Salary

# Call to the main function -- use alpha = 1 for lasso regression (the default)
lassoMod <- glmnet(x = x, y = y, alpha = 1)
# Same defaults as before, same object structure

# Plot of the solution path -- now the paths are not smooth when decreasing to
# zero (they are zero exactly). This is a consequence of the l1 norm
plot(lassoMod, xvar = "lambda", label = TRUE)
# Some persistently important predictors are 15, 14, and 19

# Versus the R^2 -- same maximum R^2 as before
plot(lassoMod, label = TRUE, xvar = "dev")

# Now the l1-norm of the coefficients decreases as lambda increases
plot(log(lassoMod$lambda), colSums(abs(lassoMod$beta)), type = "l",
     xlab = "log(lambda)", ylab = "l1 norm")

# 10-fold cross-validation. Change the seed for a different result
set.seed(12345)
kcvLasso <- cv.glmnet(x = x, y = y, alpha = 1, nfolds = 10)

# The lambda that minimises the CV error is
kcvLasso$lambda.min

# The "one standard error rule" for lambda
kcvLasso$lambda.1se

# Location of both optimal lambdas in the CV loss function
indMin <- which.min(kcvLasso$cvm)
plot(kcvLasso)
abline(h = kcvLasso$cvm[indMin] + c(0, kcvLasso$cvsd[indMin]))
# No problems now: minimum does not occur at one extreme
# Interesting: note that the numbers on top of the figure gives the number of
# coefficients *exactly* different from zero -- the number of predictors
# effectively considered in the model!
# In this case, the one standard error rule makes also sense

# Leave-one-out cross-validation
lambdaGrid <- 10^seq(log10(kcvRidge$lambda[1]), log10(0.1),
                     length.out = 150) # log-spaced grid
ncvLasso <- cv.glmnet(x = x, y = y, alpha = 1, nfolds = nrow(Hitters),
                      lambda = lambdaGrid)

# Location of both optimal lambdas in the CV loss function
plot(ncvLasso)

# Inspect the best models
modLassoCV <- kcvLasso$glmnet.fit
plot(modLassoCV, label = TRUE, xvar = "lambda")
abline(v = log(c(kcvLasso$lambda.min, kcvLasso$lambda.1se)))

# The model associated to lambda.min (or any other lambda not included in the
# original path solution -- obtained by an interpolation) can be retrieved with
predict(modLassoCV, type = "coefficients",
        s = c(kcvLasso$lambda.min, kcvLasso$lambda.1se))

# Predictions for the first two observations
predict(modLassoCV, type = "response",
        s = c(kcvLasso$lambda.min, kcvLasso$lambda.1se),
        newx = x[1:2, ])

# We can use lasso for model selection!
selPreds <- predict(modLassoCV, type = "coefficients",
                    s = c(kcvLasso$lambda.min, kcvLasso$lambda.1se))[-1, ] != 0
x1 <- x[, selPreds[, 1]]
x2 <- x[, selPreds[, 2]]

# Least squares fit with variables selected by lasso
modLassoSel1 <- lm(y ~ x1)
modLassoSel2 <- lm(y ~ x2)
summary(modLassoSel1)
summary(modLassoSel2)

# Comparison with stepwise selection
modBIC <- MASS::stepAIC(lm(Salary ~ ., data = Hitters), k = log(nrow(Hitters)),
                        trace = 0)
summary(modBIC)
# The lasso variable selection is similar, although the model is slightly worse
# in  terms of adjusted R^2 and significance of the predictors. However, keep in
# mind that lasso is solving a constrained least squares problem, so it is
# expected to achieve better R^2 and adjusted R^2 via a selection procedure
# that employs solutions of unconstrained least squares. What is remarkable
# is the speed of lasso on selecting variables, and the fact that gives quite
# good starting points for performing further model selection

# Another interesting possibility is to run a stepwise selection starting from
# the set of predictors selected by lasso. In this search, it is important to
# use direction = "both" (default) and define the scope argument adequately
f <- formula(paste("Salary ~", paste(names(which(selPreds[, 2])),
                                     collapse = " + ")))
start <- lm(f, data = Hitters) # Model with predictors selected by lasso
scope <- list(lower = lm(Salary ~ 1, data = Hitters), # No predictors
              upper = lm(Salary ~ ., data = Hitters)) # All the predictors
modBICFromLasso <- MASS::stepAIC(object = start, k = log(nrow(Hitters)),
                                 scope = scope, trace = 0)
summary(modBICFromLasso)

# Comparison in terms of BIC, slight improvement with modBICFromLasso
BIC(modLassoSel1, modLassoSel2, modBICFromLasso, modBIC)


# a

pisaUS2009 <- read.table("datasets/pisaUS2009.csv", header = TRUE, sep = ",")
pisaUS2009_no_NA <- na.omit(pisaUS2009)
mean(pisaUS2009_no_NA$readingScore)

# b

x <- model.matrix(readingScore ~ 0 + . - raceeth, data = pisaUS2009_no_NA)
y <- pisaUS2009_no_NA$readingScore
mod_ridge <- glmnet::glmnet(x = x, y = y, alpha = 0)
plot(mod_ridge, xvar = "lambda")

# c

coef <- predict(mod_ridge, s = exp(1), type = "coefficients")[2:4, 1]
names(coef) <- NULL
coef

# d

mod_lasso <- glmnet::glmnet(x = x, y = y, alpha = 1)
plot(log(mod_lasso$lambda), mod_lasso$dev.ratio, type = "l",
     xlab = "log(lambda)", ylab = "R2")

# e

n <- nrow(x)
mod_lasso_cv <- glmnet::cv.glmnet(x = x, y = y, alpha = 1, nfolds = n)
plot(mod_lasso_cv)
min(mod_lasso_cv$cvm)

# f

pred <- drop(predict(mod_lasso, s = mod_lasso_cv$lambda.1se,
                     newx = x[(n - 1):n, ]))
names(pred) <- NULL
pred

# g

sel_preds <- predict(mod_lasso_cv, type = "coefficients",
                     s = mod_lasso_cv$lambda.1se)[-1, ] != 0
x_sel <- x[, sel_preds]
summary(lm(y ~ x_sel))$adj.r.squared

# h

f <- formula(paste("readingScore ~", paste(names(which(sel_preds)),
                                           collapse = " + ")))
start <- lm(f, data = pisaUS2009_no_NA)
scope <- list(lower = lm(readingScore ~ 1, data = pisaUS2009_no_NA),
              upper = lm(readingScore ~ . - raceeth, data = pisaUS2009_no_NA))
mod_BIC_lasso <- MASS::stepAIC(object = start, k = log(nrow(pisaUS2009_no_NA)),
                               scope = scope, direction = "both", trace = 0)
summary(mod_BIC_lasso)
BIC(mod_BIC_lasso)



install.packages('readxl')
library(readxl)
laliga <- readxl::read_excel(path = 'la-liga-2015-2016.xlsx')


laliga <- as.data.frame(laliga)


# x <-
# y <-
# cvlaliga <- cv.glmnet(x=x, y=y,alpha1,nfolds=10)
predict(cvlaliga$glmnet.fit, type = 'coefficients',
        s = cvlaliga$lambda.min)



# Computes the AIC, BIC and CV errors for all the possible models with *at least*
# one predictor built from a design matrix X and a response Y
metrics_lm <- function(X, Y) {
  
  # Number of predictors (the first column of X contains 1's)
  p <- ncol(X) - 1
  
  # Possible combinations of predictors (excluding the case with no predictors)
  comb_preds <- lapply(1:p, function(k) t(combn(x = p, m = k)))
  
  # Where to store AIC, BIC and CV for all the models (2^p - 1 because we exclude
  # the model with no predictors for simplicity)
  metrics <- matrix(0, nrow = 2^p - 1, ncol = 3)
  
  # Loop on the model size (number of predictors included)
  k <- 0
  for (num_preds in 1:p) {
    
    # Loop on the possible models for a given model size
    for (j in 1:nrow(comb_preds[[num_preds]])) {
      
      # Predictors included in the model -- shift by one since the index is
      # to be used in the columns of X, the first one being 1's
      preds_included <- comb_preds[[num_preds]][j, ] + 1
      
      # Hat matrix of the model, very usedul for CV. We have to include the
      # column of 1's, that's why the c(1, preds_included)
      H <- X[, c(1, preds_included), drop = FALSE]
      H <- H %*% solve(crossprod(H)) %*% t(H)
      
      # Fit model with a combination of predictors -- we do not include the
      # column of 1's because the intercept is automatically added by lm
      mod <- lm(Y ~ X[, preds_included, drop = FALSE])
      
      # CV error -- this computation of Y_hat_i is highly inefficient
      # Y_hat <- sapply(1:n, function(i) {
      #   mod_i <- lm(Y[-i] ~ X[-i, preds_included, drop = FALSE])
      #   coefficients(mod_i)[1] + X[i, preds_included] %*% coefficients(mod_i)[-1]
      # })
      # CV_error <- mean((Y - Y_hat)^2)
      
      # A much more efficient computation of the CV error -- linear magic!
      CV_error <- mean(((Y - mod$fitted.values) / (1 - diag(H)))^2)
      
      # Save model metrics
      k <- k + 1
      metrics[k, ] <- c(AIC(mod), BIC(mod), CV_error)
      
    }
    
  }
  
  return(metrics)
  
}

# Scenario
p <- 4
sigma <- 1
beta <- c(0.5, rep(1, p %/% 2), rep(0, p - p %/% 2))
ind_beta_nonzero <- beta[-1] != 0

# Some ugly code to get the index of the true model and compare to
# the result of metrics_lm
comb_preds <- lapply(1:p, function(k) t(combn(x = p, m = k)))
ind_true <- which(unlist(lapply(comb_preds, function(x) {
  if (ncol(x) != sum(ind_beta_nonzero)) {
    return(rep(FALSE, nrow(x)))
  } else {
    return(apply(x, 1, function(y) all(y == which(ind_beta_nonzero))))
  }
})))

# Simulation setttings
M <- 100
nn <- c(10, 25, 50)
prob_true <- matrix(nrow = length(nn), ncol = 5)

# Loop on sample sizes
j <- 0
for (n in nn) {
  
  # Progress
  cat("\nn =", n, "\n")
  pb <- txtProgressBar(style = 3)
  
  # Simulation
  selected <- matrix(0, nrow = M, ncol = 5)
  for (i in 1:M) {
    
    # Simulate X (design matrix) and Y (response)
    set.seed(i)
    X <- cbind(1, matrix(rnorm(n * p), nrow = n, ncol = p, byrow = TRUE))
    set.seed(i)
    eps <- rnorm(n, sd = sigma)
    Y <- X %*% beta + eps
    
    # Matrics for all the possible linear models
    metrics <- metrics_lm(X = X, Y = Y)
    
    # Best models according to the minimum of such metrics
    best <- apply(metrics, 2, which.min)
    
    # Was the true model selected?
    selected[i, 1:3] <- best == ind_true
    
    # LASSO path fit
    # Remove the column with ones as the function adds an intercept
    lasso <- goffda::cv_glmnet(x = X[, -1], y = Y)
    
    # Was the true model selected by LASSO based on CV_1se?
    # lasso_sel_1se <- # TODO
    # selected[i, 4] <- all(lasso_sel_1se == ind_beta_nonzero) # TODO
    
    # LASSO selection based on CV_min
    # lasso_sel_min <- # TODO
    # selected[i, 5] <- all(lasso_sel_min == ind_beta_nonzero) # TODO
    
    # Update progress
    setTxtProgressBar(pb, i / M)
    
  }
  
  # Probability estimate of selecting the true model for a given sample size
  j <- j + 1
  prob_true[j, ] <- colMeans(selected)
  
  # Show plot on the iterations
  matplot(nn, prob_true, type = "o", pch = 16, lty = 1, xlab = "Sample size",
          ylab = "Probability of selecting the correct model", ylim = c(0, 1), lwd = 2,
          log = "x")
  legend("bottom", legend = c("AIC", "BIC", "LOO_CV", "LASSO_CV_1SE", "LASSO_CV_min"),
         col = 1:5, lwd = 3)
  abline(h = 1, lty = 2)
  
}

# Voluntary homework
