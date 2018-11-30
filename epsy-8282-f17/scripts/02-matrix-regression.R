##################################################
### Load libraries
##################################################





###################################################
### Create data
###################################################

myData = data.frame(
  wage = c(12, 8, 16.26, 13.65, 8.5, 8.5),
  age = c(32, 33, 32, 33, 26, 28),
  sex = c("M", "F", "M", "M", "M", "M"),
  collar = c("Pink", "Blue", "Pink", "White", "White", "Blue")
)

myData



###################################################
### Regression in practice
###################################################

lm.1 = lm(wage ~ 1 + age, data = myData)

summary(lm.1)
anova(lm.1)
vcov(lm.1)

fitted(lm.1)
residuals(lm.1)



###################################################
### Create outcome vector and design matrix
###################################################

# Outcome vector
Y = myData$wage
Y

# Design matrix
X = matrix(c(rep(1, 6), myData$age), ncol = 2)
X



###################################################
### Check determinant of X'X
###################################################

det( t(X) %*% X)



###################################################
### Obtain the coefficients
###################################################

b = solve(t(X) %*% X) %*% t(X) %*% Y
b



###################################################
### Compute fitted values
###################################################

fitted_values = X %*% b
fitted_values



###################################################
### Compute residuals
###################################################

residuals = Y - fitted_values
residuals



###################################################
### Compute MSE
###################################################

# Compute SS for the residuals
SSE = t(residuals) %*% residuals
SSE


# Compute df for the residuals
n = 6
c = 2

# Compute MSE
MSE = SSE / (n - c)
MSE


###################################################
### Compute Variance--covariance matrix of coefficients
###################################################

V_b = solve(t(X) %*% X)

# Compute variance--covariance matrix
var_cov = as.numeric(MSE) * V_b
var_cov



###################################################
### Compute SEs for coefficients
###################################################

sqrt(diag(var_cov))



###################################################
### Compute correlation matrix
###################################################

cov2cor(var_cov)



###################################################
### Obtain design matrix from the lm object
###################################################

model.matrix(lm.1)



###################################################
### Include age, sex, and age:sex in the model
###################################################

# Fit model
lm.2 = lm(wage ~ 1 + age + sex + age:sex, data = myData)

Y = myData$wage
X = model.matrix(lm.2)
X

det(t(X) %*% X)

b = solve(t(X) %*% X) %*% t(X) %*% Y

summary(lm.2)



###################################################
### Design matrix with intercept and all three dummy-coded predictors included
###################################################

X = matrix(
  data = c(1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0),
  byrow = TRUE,
  ncol = 3
  )
X

det(t(X) %*% X)



###################################################
### Design matrix with intercept and two of the three dummy-coded predictors included
###################################################

X = matrix(
  data = c(1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0),
  byrow = TRUE,
  ncol = 4
)
X

det(t(X) %*% X)


