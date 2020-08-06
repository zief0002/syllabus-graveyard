#####################################
# Read in and prepare data
#####################################

glow = read.table(file = "http://www.tc.umn.edu/~zief0002/Data/GLOW.txt", header = TRUE)
glow$age2 = cut(glow$age, breaks = c(55, 60, 70, 80, 90), include.lowest = TRUE)
head(glow)



#####################################
# Load libraries
#####################################
library(binomTools)
library(ggplot2)
library(gmodels)
library(msm)



#####################################
# Examine predictor
#####################################
summary(glow$momfrac)

# Examine relationship with outcome
CrossTable(glow$fracture, glow$momfrac, format = "SPSS")

# Jittered scatterplot 
ggplot(data = glow, aes(x = momfrac, y = fracture)) +
	geom_hline(yintercept = 0.5, alpha = 0.3) +
	geom_vline(xintercept = 0.5, alpha = 0.3) +
	geom_point(position = position_jitter(width = 0.2, height = 0.2), alpha = 0.4) +
	scale_x_continuous(
		name = "Mother had a  hip fracture?", 
		breaks = c(0, 1),
		labels = c("No", "Yes")
		) +
	scale_y_continuous(
		name = "Fracture", 
		breaks = c(0, 1),
		labels = c("No", "Yes")
		) +
	theme_bw()

# Correlation (phi)
cor(glow$fracture, glow$momfrac) 



#####################################
# Fit logistic model
#####################################

glm.a <- glm(fracture ~ momfrac, data = glow, family = binomial(link = "logit"))
summary(glm.a)



#####################################
# Analysis of deviance - Likelihood ratio test
#####################################

# Fit full model
glm.f = glm(fracture ~ momfrac, data = glow, family = binomial(link = "logit"))

# Fit reduced model
glm.r = glm(fracture ~ 1, data = glow, family = binomial(link = "logit")) 

# Compute G
LR = exp(logLik(glm.r)[1]) / exp(logLik(glm.f)[1])
G = -2 * log(LR)   
G    

# Compute p-value
pchisq(G, df = 1, lower.tail = FALSE)

# Quick version - Analysis of deviance
anova(glm.r, glm.f, test ="LRT")

#or anova(glm.r, glm.f, test ="Chisq")



#####################################
# Score test
#####################################

anova(glm.a, test ="Rao")



###################################################
### Examine profile plot for CIs
###################################################

logLik(glm.a)

qchisq(0.95, df = 1) / 2

logLik(glm.a)[1] - qchisq(0.95, df = 1) / 2

profile.a <- profile(glm.a)
plot(profile.a, , which.par = 2, log = TRUE, rel = FALSE)

plot(profile.a)

# Produce profile CIs
confint(glm.a)



###################################################
### CIs for odds
###################################################

exp(confint(glm.a))

# Estimate SE for odds
grad = exp(coef(glm.a))[2]
vc = vcov(glm.a)[2, 2]
var.b1 = t(grad) %*% vc %*% grad
se.b1 = sqrt(var.b1)

# CI
grad - 2 * se.b1
grad + 2 * se.b1

# Use delta-method to compute SE
b1 = coef(glm.a)[2]
vc = vcov(glm.a)[2, 2]

deltamethod(~ exp(x1), b1, vc)



###################################################
### CIs at particular X values
###################################################

ci.data = data.frame(
	momfrac = c(0, 1)
	)

# Use logit scale (default)
predict(glm.a, newdata = ci.data, type = "link", se.fit = TRUE)

-0.5355182 - 2* 0.2570154
-0.5355182 + 2* 0.2570154

# Use probability scale
predict(glm.a, newdata = ci.data, type = "response", se.fit = TRUE)

0.3692308 - 2* 0.05985873
0.3692308 + 2* 0.05985873



###################################################
### Add confidence envelope to graphical model
###################################################

glm.b <- glm(fracture ~ age, data = glow, family = binomial(link = "logit"))

data1 <- data.frame(
     age = seq(from = 55, to = 90, by = 1)
     )

fitted <- predict(glm.b, newdata = data1, type = "response", se.fit = TRUE)

data1$fit = fitted$fit
data1$se = fitted$se.fit

data1$lowerLimit <- data1$fit - 2 * data1$se
data1$upperLimit <- data1$fit + 2 * data1$se

ggplot(data = data1, aes(x = age, y = fit)) +
     geom_ribbon(aes(ymin = lowerLimit, ymax = upperLimit), color = "grey", alpha = 0.2) +
	geom_line(color = "blue") +
	xlab("Age") +
	ylab("Probability of Fracture") +
	ylim(c(0, 1)) +
	theme_bw()

# shortcut
ggplot(data = glow, aes(x = age, y = fracture)) +
    geom_smooth(method = "glm", family = binomial(link = "logit")) +
	xlab("Age") +
	ylab("Probability of Fracture") +
	ylim(c(0, 1)) +
	theme_bw()	
