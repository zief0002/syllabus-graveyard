#####################################
# Read in and prepare data
#####################################

glow = read.table(file = "http://www.tc.umn.edu/~zief0002/Data/GLOW.txt", header = TRUE)
head(glow)

#####################################
# Load libraries
#####################################
library(arm)
library(binomTools)
library(ggplot2)
library(gmodels)
library(msm)
library(sm)



#####################################
# Fit additive model
#####################################

glm.c = glm(fracture ~ age + momfrac + bmi, data = glow, family = binomial(link = "logit"))
summary(glm.c)

# LRT for coefficients
drop1(glm.c, test = "LRT")



#####################################
# Plot results: Logits
#####################################


new = data.frame(
	age = 55:90
	)

new$logit0 = -4.85 + 0.05 * new$age
new$logit1 = -4.21 + 0.05 * new$age


ggplot(data = new, aes(x = age, y = logit0)) +
	geom_line() +
	geom_line(aes(y = logit1), lty = "dashed") +
	theme_bw() +
	xlab("Age") +
	ylab("Log-odds of a fracture")


#####################################
# Plot results: Odds
#####################################


new = data.frame(
	age = 55:90
	)

new$odds0 = exp(-4.85 + 0.05 * new$age)
new$odds1 = exp(-4.21 + 0.05 * new$age)


ggplot(data = new, aes(x = age, y = odds0)) +
	geom_line() +
	geom_line(aes(y = odds1), lty = "dashed") +
	theme_bw() +
	xlab("Age") +
	ylab("Odds of a fracture")	
       


#####################################
# Plot results: Probabilities
#####################################

new = expand.grid(
	age = 55:90,
	momfrac = c(0, 1)
	)

new$probs = predict(glm.c, newdata = new, type = "response")

ggplot(data = new, aes(x = age, y = probs, group = factor(momfrac))) +
	geom_line(aes(lty = factor(momfrac))) +
	theme_bw() +
	xlab("Age") +
	ylab("Probability of a fracture") +
	guides(lty = FALSE)	



#####################################
# Examine interaction
#####################################

#glow$age2 = cut(glow$age, breaks = c(55, 60, 70, 80, 90), include.lowest = TRUE)
glow$age2 = cut(glow$age, breaks = 6, include.lowest = TRUE)

tab = table(glow$age2, glow$fracture, glow$momfrac)
tab

prop = prop.table(tab, 1)
prop

new = data.frame(prop)
new

names(new) = c("age", "fracture", "momfrac", "prop")


ggplot(data = new[new$fracture == 1, ], aes(x = age, y = prop)) +
	geom_line(aes(group = factor(momfrac), lty = factor(momfrac))) +
	theme_bw() +
	xlab("Age") +
	ylab("Proportion of a fracture")


# Fit interaction model
glm.d <- glm(fracture ~ age + momfrac + age:momfrac, data = glow, family = binomial(link = "logit"))
summary(glm.d)
BIC(glm.d)




#####################################
# Examine other relationships
#####################################
cor(glow[c("fracture", "age", "momfrac", "prior", "selfrisk", "premeno", "armassist", "smoke", "bmi")])

# Fit simple model
glm.e <- glm(fracture ~ prior, data = glow, family = binomial(link = "logit"))
summary(glm.e)
BIC(glm.e)

# Fit additive model (prior and age)
glm.f <- glm(fracture ~ prior + age, data = glow, family = binomial(link = "logit"))
summary(glm.f)
BIC(glm.f)

# Fit additive model (prior and momfrac)
glm.g <- glm(fracture ~ prior + momfrac, data = glow, family = binomial(link = "logit"))
summary(glm.g)
BIC(glm.g)

# Examine interaction b/w age and prior
tab = table(glow$age2, glow$fracture, glow$prior)
prop = prop.table(tab, 1)
new = data.frame(prop)
names(new) = c("age", "fracture", "prior", "prop")

ggplot(data = new[new$fracture == 1, ], aes(x = age, y = prop)) +
	geom_line(aes(group = factor(prior), lty = factor(prior))) +
	theme_bw() +
	xlab("Age") +
	ylab("Proportion of a fracture")

# Examine interaction b/w momfrac and prior
tab = table(glow$momfrac, glow$fracture, glow$prior)
prop = prop.table(tab, 1)
new = data.frame(prop)
names(new) = c("momfrac", "fracture", "prior", "prop")

ggplot(data = new[new$fracture == 1, ], aes(x = momfrac, y = prop)) +
	geom_line(aes(group = factor(prior), lty = factor(prior))) +
	theme_bw() +
	xlab("momfrac") +
	ylab("Proportion of a fracture")


# Fit interaction model (prior and age)
glm.h <- glm(fracture ~ age + prior + age:prior, data = glow, family = binomial(link = "logit"))
summary(glm.h)
drop1(glm.h, test = "LRT")
BIC(glm.h)

# Fit interaction model (prior and momfrac)
glm.i <- glm(fracture ~ momfrac + prior + momfrac:prior, data = glow, family = binomial(link = "logit"))
summary(glm.i)
drop1(glm.i, test = "LRT")
BIC(glm.i)

# Fit additive model (all 3 predictors)
glm.j <- glm(fracture ~ momfrac + prior + age, data = glow, family = binomial(link = "logit"))
summary(glm.j)
drop1(glm.j, test = "LRT")
BIC(glm.j)

# Fit interaction model
glm.k <- glm(fracture ~ momfrac + prior + age + age:prior, data = glow, family = binomial(link = "logit"))
summary(glm.k)
drop1(glm.k, test = "LRT")
BIC(glm.k)






binnedplot(x = fitted(glm.k), y = resid(glm.k))
binnedplot(x = fitted(glm.f), y = resid(glm.f))



#####################################
# Present results: Odds ratios
#####################################

exp(-5.89630809 + 0.70771427*(0) + 0.06400330 * (60) + 5.34762635*1 - 0.06274619*60*1)
exp(-5.89630809 + 0.70771427*(0) + 0.06400330 * (60) + 5.34762635*0 - 0.06274619*60*0)
0.6229711 / 0.1279507

new =  expand.grid(
	momfrac = 0,
    prior = c(0, 1), 
    age = 60
    )

new$logits = predict(glm.k, newdata = new)
new$odds = exp(new$logits)

new[9:12, 5] / new[1:4, 5]
new[13:16, 5] / new[5:8, 5]



#####################################
# Function to compute OR and CI limits
#####################################

predict.or = function(a, m){
	covariates = expand.grid(age = a, momfrac = m, prior = c(0, 1), fracture = 1)
	m = model.matrix(glm.k, data = covariates) 
	x.prime = c(-1, 1) %*% m 
	v = vcov(glm.k) 
	se = sqrt(x.prime %*% v %*% t(x.prime))
	print(exp(x.prime %*% coef(glm.k) + qnorm(c(0.50, 0.025, 0.975)) * se))
	}

predict.or(a = 60, m = 0)
predict.or(a = 70, m = 0)
predict.or(a = 80, m = 0)
predict.or(a = 90, m = 0)

