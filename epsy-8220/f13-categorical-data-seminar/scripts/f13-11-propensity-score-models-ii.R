#####################################
# Read in and prepare data
#####################################

glow = read.table(file = "http://www.tc.umn.edu/~zief0002/Data/GLOW2.txt", header = TRUE)
glow$BONETREAT = ifelse( (glow$BONEMED + glow$BONEMED_FU) == 2, 1, 0)

glm.psm = glm(BONETREAT ~ PRIORFRAC + AGE + I(AGE ^ 2) + HEIGHT + BMI, data = glow, family = binomial(link = "logit"))
glow$PS = fitted(glm.psm)


#####################################
# Load libraries
#####################################

library(ggplot2)
library(msm)
library(survival)
library(verification)



##################################################
### Eliminate observations and refit PSM
###################################################

glow2 = glow[glow$PS >= 0.04573851 & glow$PS <= 0.794613688, ]

glm.psm2 = glm(BONETREAT ~ PRIORFRAC + AGE + I(AGE ^ 2) + HEIGHT + BMI, data = glow2, family = binomial(link = "logit"))
glow2$PS = fitted(glm.psm2)


###################################################
### Propensity Score Analysis #2
###################################################

# library(ggplot2)
glow2$Quintile = cut_number(glow2$PS, n = 5, labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))

# library(PSAgraphics)
box.psa(glow2$AGE, treatment = glow2$BONETREAT, strata = glow2$Quintile, balance = TRUE)
box.psa(glow2$BMI, treatment = glow2$BONETREAT, strata = glow2$Quintile, balance = TRUE)
box.psa(glow2$HEIGHT, treatment = glow2$BONETREAT, strata = glow2$Quintile, balance = TRUE)
cat.psa(glow2$PRIORFRAC, treatment = glow2$BONETREAT, strata = glow2$Quintile, balance = TRUE)


# Increase the number of strata
glow2$SIX = cut_number(glow2$PS, n = 6, labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6"))

# Check balance
box.psa(glow2$AGE, treatment = glow2$BONETREAT, strata = glow2$SIX, balance = TRUE)
box.psa(glow2$BMI, treatment = glow2$BONETREAT, strata = glow2$SIX, balance = TRUE)
box.psa(glow2$HEIGHT, treatment = glow2$BONETREAT, strata = glow2$SIX, balance = TRUE)
cat.psa(glow2$PRIORFRAC, treatment = glow2$BONETREAT, strata = glow2$SIX, balance = TRUE)


CoefGLM = function(x){
	round(summary(glm(FRACTURE ~ BONETREAT, data = x, family = binomial(link = "logit")))$coefficients, 3)
	}

by(glow2, glow2$SIX, CoefGLM)



# Odds ratos
OR = function(x){
	round(exp(coef(glm(FRACTURE ~ BONETREAT, data = x, family = binomial(link = "logit")))[2]), 3)
	}

by(glow2, glow2$SIX, OR)


# SE by delta method
# library(msm)

SE = function(x){
	psa2 = glm(FRACTURE ~ BONETREAT, data = x, family = binomial(link = "logit"))
	b1 = coef(psa2)[2]
	vc = vcov(psa2)[2, 2]
	deltamethod(~ exp(x1), b1, vc)
	}

by(glow2, glow2$SIX, SE)


CI = function(x){
	psa2 = glm(FRACTURE ~ BONETREAT, data = x, family = binomial(link = "logit"))
	b1 = coef(psa2)[2]
	vc = vcov(psa2)[2, 2]
	SE = deltamethod(~ exp(x1), b1, vc)
	paste0(round(exp(b1) - 2 * SE, 3), ", ", round(exp(b1) + 2 * SE, 3))	
	}

by(glow2, glow2$SIX, CI)



###################################################
### Propensity Score Analysis #2 - Pooled analysis
###################################################

glow2$Q2 = ifelse(glow2$SIX == "Q2", 1, 0)
glow2$Q3 = ifelse(glow2$SIX == "Q3", 1, 0)
glow2$Q4 = ifelse(glow2$SIX == "Q4", 1, 0)
glow2$Q5 = ifelse(glow2$SIX == "Q5", 1, 0)
glow2$Q6 = ifelse(glow2$SIX == "Q6", 1, 0)

psa2 = glm(FRACTURE ~ BONETREAT + Q2 + Q3 + Q4 + Q5 + Q6, 
	data = glow2, family = binomial(link = "logit"))
summary(psa2)

#OR
exp(coef(psa2)[2])

#Compute SE
b1 = coef(psa2)[2]
vc = vcov(psa2)[2, 2]
deltamethod(~ exp(x1), b1, vc)

# SE = 0.3504809

# CI
1.4233625 - 2*0.3504809
1.4233625 + 2*0.3504809



###################################################
### Propensity Score Analysis #3 - Matching
###################################################

psm = glm(BONETREAT ~ PRIORFRAC + AGE + I(AGE ^ 2) + HEIGHT + BMI, 
    data = glow2, family = binomial(link = "logit"))

# library(optmatch)

# dist = match_on(psm)
# psmCaliper = caliper(dist, width = 0.2)
# fullmatch(psmCaliper, data = glow2)


library(Matching)

# Estimand = "ATT"
glowMatch = Match(Y = glow2$FRACTURE, Tr = glow2$BONETREAT, X = glow2$PS, caliper = 0.2)

tr = glowMatch$index.treated
cn = glowMatch$index.control

treat = glow2[tr, c("SUB_ID", "FRACTURE", "BONETREAT", "PS")]
control = glow2[cn, c("SUB_ID", "FRACTURE", "BONETREAT", "PS")]

glow3 = rbind(treat, control)

glow3$ID = rep(1:160, 2)

#library(survival)
psa3 = clogit(FRACTURE ~ BONETREAT + strata(ID), data = glow3)
summary(psa3)



# Average treatment effect
glowMatch = Match(Y = glow2$FRACTURE, Tr = glow2$BONETREAT, X = glow2$PS, caliper = 0.2, estimand = "ATE")
treat = glow2[glowMatch$index.treated, c("SUB_ID", "FRACTURE", "BONETREAT", "PS")]
control = glow2[glowMatch$index.control, c("SUB_ID", "FRACTURE", "BONETREAT", "PS")]
glow3 = rbind(treat, control)
glow3$ID = rep(1:516, 2)
psa3 = clogit(FRACTURE ~ BONETREAT + strata(ID), data = glow3)
summary(psa3)

# Average treatment effect for the Controls
glowMatch = Match(Y = glow2$FRACTURE, Tr = glow2$BONETREAT, X = glow2$PS, caliper = 0.2, estimand = "ATC")
treat = glow2[glowMatch$index.treated, c("SUB_ID", "FRACTURE", "BONETREAT", "PS")]
control = glow2[glowMatch$index.control, c("SUB_ID", "FRACTURE", "BONETREAT", "PS")]
glow3 = rbind(treat, control)
glow3$ID = rep(1:356, 2)
psa3 = clogit(FRACTURE ~ BONETREAT + strata(ID), data = glow3)
summary(psa3)




###################################################
### Fractional polynomials
###################################################

library(mfp)
f = mfp(BONETREAT ~ fp(PRIORFRAC) + fp(AGE) + fp(HEIGHT) + fp(BMI), 
	family = binomial(link = "logit"), data = glow)

