#####################################
# Read in and prepare data
#####################################

glow = read.table(file = "http://www.tc.umn.edu/~zief0002/Data/GLOW.txt", header = TRUE)
head(glow)

#####################################
# Load libraries
#####################################

library(BaylorEdPsych)
library(ggplot2)
library(pscl)
library(sm)
library(verification)


#####################################
# Fit additive model
#####################################

glm.k <- glm(fracture ~ momfrac + prior + age + age:prior, data = glow, family = binomial(link = "logit"))
summary(glm.k)

drop1(glm.k, test = "LRT")
BIC(glm.k)

# LRT for coefficients
drop1(glm.c, test = "LRT")



#####################################
# Efron's Pseudo R-Squared
#####################################


y = glow$fracture
pi_hat = fitted(glm.k)

1 - sum((y - pi_hat) ^ 2) / sum((y - mean(y)) ^ 2)



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


