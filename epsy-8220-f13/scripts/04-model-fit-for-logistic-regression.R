#####################################
# Read in and prepare data
#####################################

glow = read.table(file = "http://www.tc.umn.edu/~zief0002/Data/GLOW.txt", header = TRUE)
head(glow)



#####################################
# Load libraries
#####################################

library(arm)
library(car)
library(ggplot2)
library(gmodels)
library(MKmisc)




#####################################
# Chi-squared test of independence
#####################################

cor(glow$fracture, glow$momfrac)

CrossTable(glow$fracture, glow$momfrac, format = "SPSS", chisq = TRUE)


#####################################
# Likelihood ratio test for goodness of fit
#####################################

O = c(334, 41, 101, 24)
E = c(326.25, 48.75, 108.75, 16.25)

G = 2 * sum(O * log(O/E))
pchisq(G, df = 1, lower = FALSE)


#####################################
# Analysis of deviance - Likelihood ratio test
#####################################

# Fit full model
glm.f = glm(fracture ~ momfrac, data = glow, family = binomial(link = "logit"))

# Fit reduced model
glm.r = glm(fracture ~ 1, data = glow, family = binomial(link = "logit")) 

# Analysis of deviance
anova(glm.r, glm.f, test ="Chisq")



#####################################
# Deviance
#####################################

deviance(glm.f)
summary(glm.f)

deviance(glm.r)



#####################################
# Fit age model
#####################################

glm.b = glm(fracture ~ age, data = glow, family = binomial(link = "logit"))
summary(glm.b)




#####################################
# Hosmer–Lemeshow test
#####################################

# age model
HLgof.test(fit = fitted(glm.b), obs = glow$fracture)

# momfrac model
#HLgof.test(fit = fitted(glm.f), obs = glow$fracture)



#####################################
# Information criteria
#####################################

AIC(glm.b)
AIC(glm.f)

BIC(glm.b)
BIC(glm.f)


#####################################
# Examine residuals
#####################################

head(resid(glm.b)) # Deviance residuals
head(resid(glm.b, type = "pearson")) # Pearson residuals

# Obtain observation-level indices
out.b = fortify(glm.b)
head(out.b)

# Binned residual plot
binnedplot(x = out.b$.fitted, y = out.b$.resid) 
#binnedplot(x = fitted(glm.b), y = resid(glm.b)) 



#####################################
# Diagnostics
#####################################

# Index plot of the leverage values
influenceIndexPlot(glm.b, vars = "hat", id.n = 10)

# Index plot of the Cook's values
influenceIndexPlot(glm.b, vars = "Cook", id.n = 10)

# Plot to examine residual values, leverage values and Cook's values
influencePlot(glm.b, vars = "hat", id.n = 10)

# Fit updated model removing observations 450 and 169
glm.c =  update(glm.b, subset = - c(169, 450))
summary(glm.c)

