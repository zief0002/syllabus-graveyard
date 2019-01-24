#####################################
# Load Librariesn
#####################################

library(ggplot2)
library(plyr)



#####################################
# Log-likelihood function
#####################################

LL = function(b0) {
	-5/2 * log(2 * pi * 15) - 1/(2*15) * ( (12 - b0)^2 + (8 - b0)^2 + (16.26 - b0)^2 + 
	(13.65 - b0)^2 + (8.5 - b0)^2 ) 
	}

## Try function
LL(11)
LL(11.01)
LL(11.02)
LL(11.03)


#####################################
# Grid Search (b0)
#####################################

## Generate candidate values for b0
candidates <- data.frame(
	b0 = seq(from = 11, to = 12, by = 0.01)
	)

## Generate the log-likelihood values and store the results

gridSearch = mdply(candidates, LL)


## Change the name of the second column
names(gridSearch)[2] = "logLikelihood"

## Plot log-likelihood vs. b0
ggplot(data = gridSearch, aes(x = b0, y = logLikelihood)) + 
    geom_line() +
    theme_bw() +
    xlab(expression(beta[0])) +
    ylab("Log-Likelihood")

## Arrange from smallest to largest log-likelihood
tail(arrange(gridSearch, logLikelihood))




#####################################
# Grid Search (b0, b1, sigma2)
#####################################

## Create the function
dev = function(b0, b1, sigma2) {
	5 * log(2 * pi * sigma2) + 1/sigma2 * ( (12 - b0 - b1 * 32)^2 + 
	(8 - b0 - b1 * 33)^2 + (16.26 - b0 - b1 * 32)^2 + 
	(13.65 - b0 - b1 * 33)^2 + (8.5 - b0 - b1 * 26)^2 ) 
	}
	
## Try function
dev(b0 = 0, b1 = 0, sigma2 = 15)

## Generate candidate values for the unknown parameters
candidates = expand.grid(
    b0 = seq(from = -4, to = -3, by = 0.01),
    b1 = seq(from = 0, to = 1, by = 0.01),
    sigma2 = seq(from = 0, to = 20, by = 0.01)
    )

## Generate the deviance values and store the results
gridSearch <- mdply(candidates, dev)


## Change the name of the third column
names(gridSearch)[2] <- "deviance"

## Order the data frame by deviance
arrange(gridSearch, dev)



#####################################
# Grid Search (b0, b1) - Logistic
#####################################

## Create the function to compute log-likelihood
ll = function(b0, b1) {
     2 * b0 + 112 * b1 -
	log(1 + exp(b0 + 81 * b1)) -
	log(1 + exp(b0 + 46 * b1))-
	log(1 + exp(b0 + 53 * b1))-
	log(1 + exp(b0 + 66 * b1))-
	log(1 + exp(b0 + 63 * b1)) 
    }

## Try function
ll(b0 = 0, b1 = 0)

## Generate candidate values for the unknown parameters
candidates = expand.grid(
    b0 = seq(from = 4.3, to = 4.4, by = 0.001),
    b1 = seq(from = -.08, to = -.07, by = 0.001)
    )

## Generate the deviance values and store the results
gridSearch <- mdply(candidates, ll)


## Change the name of the third column
names(gridSearch)[3] <- "logLikelihood"

## Order the data frame by deviance
tail(arrange(gridSearch, logLikelihood))




#####################################
# Compute derivative
#####################################

g = expression(log(mu) - log(1 - mu))
D(g, "mu")



#####################################
# IRLS Example
#####################################

beta = c(0, 0)
x = c(81, 46, 63, 53, 66)
y = c(0, 1, 0, 0, 1)


iter = 10

for (i in 1:iter) {
	eta = beta[1] + beta[2] * x
	mu = exp(eta) / (1 + exp(eta))
	z = eta + (y - mu) * (1/(mu * (1 - mu)))
    w = 1/(1/(mu * (1 - mu))^2 * mu * (1 - mu))
    wlm = lm(z ~ x, weights = w)
    beta = unname(coef(wlm))
    print(beta)
	}


# Compare to the glm() output
x = c(81, 46, 63, 53, 66)
y = c(0, 1, 0, 0, 1)
glm(y ~ x, family = binomial(link="logit"))



#####################################
# IRLS Example - Better printing
#####################################

beta = c(0, 0)
x = c(81, 46, 63, 53, 66)
y = c(0, 1, 0, 0, 1)

iter = 10

results = matrix(ncol = 3, nrow = iter)
colnames(results) = c("Iterations", "b_0", "b_1")

for (i in 1:iter) {
	eta = beta[1] + beta[2] * x
	mu = exp(eta) / (1 + exp(eta))
	z = eta + (y - mu) * (1/(mu * (1 - mu)))
    w = mu * (1 - mu)
    wlm = lm(z ~ x, weights = w)
    beta = unname(coef(wlm))
    results[i, ] = c(round(i, 0), unname(beta))
	}

print(results)



#####################################
# IRLS Example - GLOW data
#####################################
glow = read.table(file = "http://www.tc.umn.edu/~zief0002/Data/GLOW.txt", header = TRUE)

beta = c(0, 0)
x = glow$age
y = glow$fracture

iter = 10

results = matrix(ncol = 3, nrow = iter)
colnames(results) = c("Iterations", "b_0", "b_1")

for (i in 1:iter) {
	eta = beta[1] + beta[2] * x
	mu = exp(eta) / (1 + exp(eta))
	z = eta + (y - mu) * (1/(mu * (1 - mu)))
    w = mu * (1 - mu)
    wlm = lm(z ~ x, weights = w)
    beta = unname(coef(wlm))
    results[i, ] = c(round(i, 0), unname(beta))
	}

print(results)

# Compare with glm() results
glm.a <- glm(fracture ~ age, data = glow, family = binomial(link = "logit"))
summary(glm.a, print = TRUE)

























## Try different starting values for beta 
x = c(81, 46, 63, 53, 66)
y = c(0, 1, 0, 0, 1)

# Try different starting values
glm.a = glm(y ~ x, family = binomial(link = "logit"), start = c(0, 0))
glm.b = glm(y ~ x, family = binomial(link = "logit"), start = c(1, 3))

# Set a prameter as fixed
glm.c <- glm(y ~ 1 + offset(I(0 * x)), family = binomial(link = "logit")) 
summary(glm.c)

glm.d = glm(y ~ 1 + offset(I(1 * x)), family = binomial(link = "logit"))
summary(glm.d)



#####################################
# See the deviance at each iteration
#####################################

glm.a <- glm(fracture ~ age, data = glow, family = binomial(link = "logit"), trace = TRUE)
