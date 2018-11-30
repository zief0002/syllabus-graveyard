#####################################
# Read in and prepare data
#####################################

glow = read.table(file = "http://www.tc.umn.edu/~zief0002/Data/GLOW2.txt", header = TRUE)
head(glow)

glow$BONETREAT = ifelse( (glow$BONEMED + glow$BONEMED_FU) == 2, 1, 0)



#####################################
# Load libraries
#####################################

library(ggplot2)
library(gmodels)
library(psych)
library(verification)



#####################################
# Examine relationship between fracture and BONETREAT
#####################################

CrossTable(glow$FRACTURE, glow$BONETREAT, format = "SPSS")

# Correlation (phi)
cor(glow$FRACTURE, glow$BONETREAT) 



#####################################
# Fit logistic model
#####################################

glm.a = glm(FRACTURE ~ BONETREAT, data = glow, family = binomial(link = "logit"))
summary(glm.a)
exp(coef(glm.a))


#####################################
# Fit all two predictor models with BONETREAT
#####################################

# Get names of predictors
preds = names(glow)[c(4:12)]

# Create vector from 1 to number of predictors
n = 1:length(preds)

# Quickly write out the formulas in a vector
formulas = sapply(n, function(i) {paste("FRACTURE ~ BONETREAT + ", preds[i], sep = "")} )

# Quickly fit all of the glm models and output the coefficients
lapply(formulas, function(i) {coef(glm(as.formula(i), data = glow, family = binomial(link = "logit")))} )

# Compute Delta(beta_1)
new = data.frame(
	beta = unlist(lapply(formulas, function(i) {coef(glm(as.formula(i), data = glow, family = binomial(link = "logit")))[[2]]}))
	)

new$Delta = 100 * (0.5833 - new$beta) / new$beta



###################################################
### Fit logistic model with BONETREAT as outcome
###################################################

ggplot(data = glow, aes(x = AGE, y = BONETREAT)) +
	stat_summary(fun.y = mean, geom = "point") +
	geom_smooth(se = FALSE) +
	theme_bw()

ggplot(data = glow, aes(x = HEIGHT, y = BONETREAT)) +
	stat_summary(fun.y = mean, geom = "point") +
	geom_smooth(se = FALSE) +
	theme_bw()

ggplot(data = glow, aes(x = BMI, y = BONETREAT)) +
	stat_summary(fun.y = mean, geom = "point") +
	geom_smooth(se = FALSE) +
	theme_bw()		


# Fit Main effect model
glm.psm = glm(BONETREAT ~ PRIORFRAC + AGE + HEIGHT + BMI, 
	data = glow, family = binomial(link = "logit"))

out.psm = fortify(glm.psm)

# Check functional form of predictors
ggplot(data = out.psm, aes(x = AGE, y = .resid)) +
	stat_summary(fun.y = mean, geom = "point") +
	geom_smooth(se = FALSE) +
	theme_bw()

ggplot(data = out.psm, aes(x = BMI, y = .resid)) +
	stat_summary(fun.y = mean, geom = "point") +
	geom_smooth(se = FALSE) +
	theme_bw()

ggplot(data = out.psm, aes(x = HEIGHT, y = .resid)) +
	stat_summary(fun.y = mean, geom = "point") +
	geom_smooth(se = FALSE) +
	theme_bw()			


# Fit quadratic age model
glm.psm = glm(BONETREAT ~ PRIORFRAC + AGE + I(AGE ^ 2) + HEIGHT + BMI, 
	data = glow, family = binomial(link = "logit"))

out.psm = fortify(glm.psm)

# Check functional form of predictors
ggplot(data = out.psm, aes(x = AGE, y = .resid)) +
	stat_summary(fun.y = mean, geom = "point") +
	geom_smooth(se = FALSE) +
	theme_bw()

ggplot(data = out.psm, aes(x = BMI, y = .resid)) +
	stat_summary(fun.y = mean, geom = "point") +
	geom_smooth(se = FALSE) +
	theme_bw()

ggplot(data = out.psm, aes(x = HEIGHT, y = .resid)) +
	stat_summary(fun.y = mean, geom = "point") +
	geom_smooth(se = FALSE) +
	theme_bw()	

# Get coeffficients
summary(glm.psm)



###################################################
### Region of Common Support
###################################################

glow$PS = fitted(glm.psm)

ggplot(data = glow, aes(x = PS)) +
	geom_dotplot(dotsize = 0.4) +
	theme_bw() +
	facet_wrap(~BONETREAT, ncol = 1)

describeBy(glow$PS, glow$BONETREAT)

sort(glow$PS[glow$BONETREAT == 1])
sort(glow$PS[glow$BONETREAT == 0])


###################################################
### Eliminate observations and refit PSM
###################################################

glow2 = glow[glow$PS >= 0.04573851 & glow$PS <= 0.794613688, ]

table(glow2$BONETREAT)

glm.psm2 = glm(BONETREAT ~ PRIORFRAC + AGE + I(AGE ^ 2) + HEIGHT + BMI, 
    data = glow2, family = binomial(link = "logit"))

summary(glm.psm2)
confint(glm.psm2)

roc.area(glow2$BONETREAT, fitted(glm.psm2))
roc.plot(x = glow2$BONETREAT, pred = fitted(glm.psm2))



pdf(file = "~/Desktop/aaaaa.pdf", height = 8, width = 6)
dev.off()



###################################################
### Propensity Score Analysis #1
###################################################

# Unadjusted model
summary(glm(FRACTURE ~ BONETREAT, data = glow2, family = binomial(link = "logit")))
exp(coef(glm(FRACTURE ~ BONETREAT, data = glow2, family = binomial(link = "logit"))))

# PS adjusted model
glow2$PS = fitted(glm.psm2)

psa1 = glm(FRACTURE ~ BONETREAT + PS, data = glow2, family = binomial(link = "logit"))
summary(psa1)
exp(coef(psa1))

# Examine potential covariate imbalance
t.test(AGE ~ BONETREAT, data = glow2)
t.test(PRIORFRAC ~ BONETREAT, data = glow2)
t.test(BMI ~ BONETREAT, data = glow2)
t.test(HEIGHT ~ BONETREAT, data = glow2)

prop.test(table(glow2$PRIORFRAC, glow2$BONETREAT), correct = FALSE)

###################################################
### Propensity Score Analysis #2
###################################################

glow2$Quintile = cut_number(glow2$PS, n = 5, labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))

# Examine covariate balance
by(glow2, glow2$Quintile, function(x) t.test(AGE ~ BONETREAT, data = x)$p.value)
by(glow2, glow2$Quintile, function(x) t.test(PRIORFRAC ~ BONETREAT, data = x)$p.value)
by(glow2, glow2$Quintile, function(x) t.test(BMI ~ BONETREAT, data = x)$p.value)
by(glow2, glow2$Quintile, function(x) t.test(HEIGHT ~ BONETREAT, data = x)$p.value)


CoefGLM = function(x){
	round(summary(glm(FRACTURE ~ BONETREAT, data = x, family = binomial(link = "logit")))$coefficients, 3)
	}

by(glow2, glow2$Quintile, CoefGLM)

# Odds ratos
OR = function(x){
	round(exp(coef(glm(FRACTURE ~ BONETREAT, data = x, family = binomial(link = "logit")))), 3)
	}

by(glow2, glow2$Quintile, OR)



###################################################
### Propensity Score Analysis #2 - Pooled analysis
###################################################

glow2$Q2 = ifelse(glow2$Quintile == "Q2", 1, 0)
glow2$Q3 = ifelse(glow2$Quintile == "Q3", 1, 0)
glow2$Q4 = ifelse(glow2$Quintile == "Q4", 1, 0)
glow2$Q5 = ifelse(glow2$Quintile == "Q5", 1, 0)

psa2 = glm(FRACTURE ~ BONETREAT + Q2 + Q3 + Q4 + Q5, data = glow2, family = binomial(link = "logit"))
summary(psa2)



###################################################
### Propensity Score Analysis #3 - Matching
###################################################

psm = glm(BONETREAT ~ PRIORFRAC + AGE + I(AGE ^ 2) + HEIGHT + BMI, 
    data = glow2, family = binomial(link = "logit"))

library(optmatch)

dist = match_on(psm)
psmCaliper = caliper(dist, width = 0.2)
fullmatch(psmCaliper, data = glow2)


library(Matching)

# Estimand = "ATT"
glowMatch = Match(Y = glow2$FRACTURE, Tr = glow2$BONETREAT, X = glow2$PS, caliper = 0.2)
summary(glowMatch)

glowMatch = Match(Y = glow2$FRACTURE, Tr = glow2$BONETREAT, X = glow2$PS, caliper = 0.2, estimand = "ATE")
summary(glowMatch)

glowMatch = Match(Y = glow2$FRACTURE, Tr = glow2$BONETREAT, X = glow2$PS, caliper = 0.2, estimand = "ATC")
summary(glowMatch)


glowMatch$index.treated
glowMatch$index.control




###################################################
### Fractional polynomials
###################################################

library(mfp)
f = mfp(BONETREAT ~ fp(PRIORFRAC) + fp(AGE) + fp(HEIGHT) + fp(BMI), 
	family = binomial(link = "logit"), data = glow)

