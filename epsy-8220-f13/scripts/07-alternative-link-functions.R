#####################################
# Read in and prepare data
#####################################

#glow = read.table(file = "http://www.tc.umn.edu/~zief0002/Data/GLOW.txt", header = TRUE)
#head(glow)

gay = read.csv(file = "http://www.tc.umn.edu/~zief0002/Data/Gay-Marriage.csv")

gay$support = ifelse(gay$marriage == 1, 1, 0) 
gay$attend = 5 - gay$attend 




#####################################
# Load libraries
#####################################

library(BaylorEdPsych)
library(ggplot2)
#library(pscl)
#library(sm)
#library(verification)





#####################################
# Consider variables
#####################################

names(gay)

# [1] "id"          "marriage"    "attend"      "denom"       "gender"      "ideology"   
# [7] "orientation" "friends"     "educ"        "region"      "race"        "age"        
#[13] "support"     

# Outcome: support
# Religious predictors: attend, denom
# Demographic predictors: gender, ideology, orientation, friends, educ, region, race, age



#####################################
# Recode variables
#####################################

# Recode as factors
gay$denom = factor(gay$denom, labels = c("Protestant", "Catholic", "Jewish", "Other"))
gay$orientation = factor(gay$orientation, labels = c("Heterosexual", "Homosexual", "Bisexual"))
gay$region = factor(gay$region, labels = c("Northeast", "Midwest", "South", "West"))

gay$race = ifelse(gay$race %in% 4:7, 3, gay$race)
gay$race = factor(gay$race, labels = c("White", "Black", "Other"))


# Dummy code
gay$female = ifelse(gay$gender == 2, 1, 0) 
gay$friendsLGBT = ifelse(gay$friends == 5, 0 ,1)


# Unsure: Can we treat ideology as continuous?
ggplot(data = gay, aes(x = ideology, y = support)) +
	stat_summary(fun.y = mean, geom = "point", size = 3)
# hmmmm...yes, but may be quadratic...


# No need to recode
# age, educ

# Outcome: support
# Religious predictors: attend, denom
# Demographic predictors: female, ideology, orientation, friendsLGBT, educ, region, race, age



#####################################
# Plot of conditional proportions for religious predictors
#####################################

ggplot(data = gay, aes(x = attend, y = support)) +
	stat_summary(fun.y = mean, geom = "point")

ggplot(data = gay, aes(x = denom, y = support)) +
	stat_summary(fun.y = mean, geom = "point")



#####################################
# Plot of conditional proportions for demographic predictors
#####################################

ggplot(data = gay, aes(x = female, y = support)) +
	stat_summary(fun.y = mean, geom = "point")

ggplot(data = gay, aes(x = ideology, y = support)) +
	stat_summary(fun.y = mean, geom = "point") 

ggplot(data = gay, aes(x = orientation, y = support)) +
	stat_summary(fun.y = mean, geom = "point", size = 3)	

ggplot(data = gay, aes(x = friendsLGBT, y = support)) +
	stat_summary(fun.y = mean, geom = "point", size = 3)

ggplot(data = gay, aes(x = educ, y = support)) +
	stat_summary(fun.y = mean, geom = "point", size = 3) +
	geom_smooth(se = FALSE)	

ggplot(data = gay, aes(x = region, y = support)) +
	stat_summary(fun.y = mean, geom = "point", size = 3)

ggplot(data = gay, aes(x = race, y = support)) +
	stat_summary(fun.y = mean, geom = "point", size =3)

ggplot(data = gay, aes(x = age, y = support)) +
	stat_summary(fun.y = mean, geom = "point", size = 3) +
	geom_smooth(se = FALSE)	



#####################################
# Fit additive model
#####################################

glm.a <- glm(support ~ age + I(age^2), data = gay, family = binomial(link = "logit"))
summary(glm.a)
PseudoR2(glm.a)

glm.b <- glm(support ~ age + I(age^2), data = gay, family = binomial(link = "probit"))
summary(glm.b)
PseudoR2(glm.b)


new = data.frame(
	age = 18:95
	)
new$probitProb = predict(glm.a, newdata = new, type = "response")
new$logitProb = predict(glm.b, newdata = new, type = "response")

ggplot(data = new, aes(x = age, y = logitProb)) +
	geom_line(aes(y = probitProb), color = "darkred") +
	geom_line() +
	theme_bw() +
	xlab("Age") +
	ylab("Probability")



#####################################
# Fit complementary log-log model
#####################################

glm.c <- glm(support ~ age + I(age^2), data = gay, family = binomial(link = "cloglog"))
summary(glm.c)
PseudoR2(glm.c)

new = data.frame(
	age = 18:95
	)
new$probitProb = predict(glm.a, newdata = new, type = "response")
new$logitProb = predict(glm.b, newdata = new, type = "response")
new$clogProb = predict(glm.c, newdata = new, type = "response")

ggplot(data = new, aes(x = age, y = logitProb)) +
	geom_line(aes(y = clogProb), color = "darkred") +
	geom_line() +
	theme_bw() +
	xlab("Age") +
	ylab("Probability")



#####################################
# Reverse code outcome
#####################################

gay$noSupport = ifelse(gay$support == 0, 1, 0)

glm.a2 <- glm(noSupport ~ age + I(age^2), data = gay, family = binomial(link = "logit"))
summary(glm.a2)

glm.b2 <- glm(noSupport ~ age + I(age^2), data = gay, family = binomial(link = "probit"))
summary(glm.b2)

glm.c2 <- glm(noSupport ~ age + I(age^2), data = gay, family = binomial(link = "cloglog"))
summary(glm.c2)





glm.z <- glm(support ~ attend + denom, data = gay, family = binomial(link = "logit"))
summary(glm.z)


glm.y <- glm(support ~ attend + denom + attend:denom, data = gay, family = binomial(link = "logit"))
summary(glm.y)
## No

glm.x <- glm(support ~ educ + region + race + age + female, data = gay, family = binomial(link = "logit"))
summary(glm.x)

glm.w <- glm(support ~ orientation + friendsLGBT + ideology, data = gay, family = binomial(link = "logit"))
summary(glm.w)

glm.v <- glm(support ~ educ + region + race + age + female + orientation + friendsLGBT + ideology, data = gay, family = binomial(link = "logit"))
summary(glm.v)


glm.u <- glm(support ~ attend + denom + region + race + age + I(age^2) + female + orientation + friendsLGBT + ideology + I(ideology ^ 2), data = gay, family = binomial(link = "logit"))
summary(glm.u)

glm.t <- glm(support ~ ., data = gay, family = binomial(link = "logit"))
summary(glm.t)

glm.v <- glm(support ~ (educ + region + race + age + female + 
	orientation + friendsLGBT + ideology)^2, data = gay, family = binomial(link = "logit"))
summary(glm.v)























glm.d <- glm(noSupport ~ age + I(age^2), data = gay, family = binomial(link = "cloglog"))
summary(glm.d)
PseudoR2(glm.d)

new = data.frame(
	age = 18:95
	)
new$logitProb = predict(glm.b, newdata = new, type = "response")
new$loglogProb = predict(glm.d, newdata = new, type = "response")
new$clogProb = predict(glm.c, newdata = new, type = "response")

ggplot(data = new, aes(x = age, y = logitProb)) +
	geom_line(aes(y = clogProb), color = "darkgreen") +
	geom_line(aes(y = (1 - loglogProb)), color = "darkred") +
	geom_line() +
	theme_bw() +
	xlab("Age") +
	ylab("Probability")


summary(glm(noSupport ~ age, data = gay, family = binomial(link = "cloglog")))

#####################################
# Fit additive model
#####################################

glm.k <- glm(support ~ age + I(age^2), data = gay, family = poisson(link = "log"))
summary(glm.k)

drop1(glm.k, test = "LRT")
BIC(glm.k)

# LRT for coefficients
drop1(glm.c, test = "LRT")





#####################################
# McFadden's Pseudo R-Squared
#####################################

# Fitted model log-likelihood
517.8 / -2

# Null model log-likelihood
562.34 / -2

1 - (-258.9 / -281.17)


#####################################
# Cox and Snell's Pseudo R-Squared
#####################################

# Null model likelihood
exp(562.34 / -2)

# Fitted model likelihood
exp(517.8 / -2)

1 - (exp(562.34 / -2) / exp(517.8 / -2)) ^ (2/500)



#####################################
# Nagelkerke's Pseudo R-Squared
#####################################

(1 - (( exp(562.34 / -2) / exp(517.8 / -2)) ^ (2/500))) / (1 - exp(562.34 / -2) ^ (2/500))



#####################################
# Many pseudo R-Squared values
#####################################

#library(pscl)
pR2(glm.k)

#library(BaylorEdPsych)
PseudoR2(glm.k)



#####################################
# Classification Accuracy
#####################################

fitted(glm.k)	

# Classification of fitted values
table(fitted(glm.k) > 0.5)

#pi.i = predict(glm.k, type = "response")
#table(pi.i > 0.5)
       
# Cross-classify with fitted values and outcome (using c = 0.5) 
table(fitted(glm.k) > 0.5, glow$fracture)      

# Classification rate
373/500

# Specificity
364/375

9/125

# Cross-classify with fitted values and outcome (using c = 0.4) 
table(fitted(glm.k) > 0.40, glow$fracture)   

# Cross-classify with fitted values and outcome (using c = 0.30) 
table(fitted(glm.k) > 0.30, glow$fracture) 

# Examine distribution of fitted values
sm.density(fitted(glm.k) > 0.40)
describe(fitted(glm.k) > 0.40)



#####################################
# Dotplot of fitted values conditioned on observed values with vertical line at cutpoint
#####################################

cross.classify = data.frame(
	observed = factor(glow$fracture, labels = c("No Fracture", "Fracture")),
	fitted = fitted(glm.k),
	classified = factor(fitted(glm.k) > 0.50,  labels = c("Predict = No Fracture", "Predict = Fracture"))
	)


ggplot(data = cross.classify, aes(x = fitted, fill = observed)) +
	geom_dotplot(binwidth = 0.006, color = "black") +
	#geom_vline(xintercept = c(0.3, 0.4, 0.5), color = "black", lty = "dashed") +
	scale_y_continuous(name = "", breaks = NULL) +
	facet_wrap(~ observed, nrow = 2) +
	scale_fill_brewer(palette = "Set1") +
	theme_bw()



#####################################
# Dotplot of Classification Table
#####################################

ggplot(data = cross.classify, aes(x = fitted, fill = observed)) +
	geom_dotplot(binwidth = 0.006, color = "black") +
	scale_y_continuous(name = "", breaks = NULL) +
	facet_grid(classified ~ observed) +
	scale_fill_brewer(palette = "Set1") +
	theme_bw()



#####################################
# Plot of sensitivity/specificity versus cutpoint
#####################################

describe(fitted(glm.k))

new = data.frame(
	cutpoint = seq(0, 1, by = 0.01)
	)

for(i in 1:nrow(new)){
	new$sensitivity[i] = sum(fitted(glm.k) > new$cutpoint[i] & glow$fracture == 1) / 125
	new$specificity[i] = sum(fitted(glm.k) <= new$cutpoint[i] & glow$fracture == 0) / 375
	}

new

ggplot(data = new, aes(x = cutpoint, y = sensitivity)) +
	geom_line() +
	geom_line(aes(y = specificity), lty = 2) +
	theme_bw() +
	xlab("Probability Cutpoint") +
	ylab("Sensitivity/Specificity")	




cross.classify = data.frame(
	observed = factor(glow$fracture, labels = c("No Fracture", "Fracture")),
	fitted = fitted(glm.k),
	classified = factor(fitted(glm.k) > 0.26,  labels = c("Predict = No Fracture", "Predict = Fracture"))
	)


ggplot(data = cross.classify, aes(x = fitted, y = ..density..)) +
	geom_histogram(fill = "yellow", color = "black") +
	geom_vline(xintercept = c(0.26), color = "black", lty = "dashed") +
	facet_wrap(~ observed, nrow = 2) +
	theme_bw()




#####################################
# ROC Curve
#####################################

new$spec.1 = 1 - new$specificity


ggplot(data = new, aes(x = spec.1, y = sensitivity)) +
	geom_line(lwd = 0) +
	geom_segment(x = 0, y = 0, xend = 1, yend = 1, lty = "dashed", color = "darkred", lwd = 0.25) +
	#geom_abline(intercept = 0, slope = 1, lty = "dashed", color = "darkred") +
	theme_bw() +
	xlab("1 - Specificity") +
	ylab("Sensitivity")


#library(verification)
roc.plot(x = glow$fracture, pred = fitted(glm.k))


