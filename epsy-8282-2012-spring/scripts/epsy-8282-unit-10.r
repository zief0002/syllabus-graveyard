###################################################
### Statistical Analysis of Longitudinal Data I
### Spring 2012
### Andrew Zieffler
###
### Read in Minneapolis-Long.csv and write to 
### object mpls.l.
###################################################

mpls.l <- read.csv(file = "/Users/andrewz/Dropbox/EPSY-8282/Data/Minneapolis-Long2.csv")

mpls.l$grade5 <- mpls.l$grade - 5
mpls.l$dadv <- as.factor(ifelse(mpls.l$risk == "ADV", "ADV", "DADV"))
mpls.l$ethW <- as.factor(ifelse(mpls.l$eth == "Whi", "W", "NW"))

library(ggplot2)  
library(lme4)
library(AICcmodavg)
library(plyr)
library(sm)


###################################################
### Restricted Maximum Likelihood
###################################################

## Estimate models
remlMod <- lmer(read ~ 1 + grade5 + (1 + grade5 | subid), data = mpls.l, REML = TRUE)
mlMod <- lmer(read ~ 1 + grade5 + (1 + grade5 | subid), data = mpls.l, REML = FALSE)


## Print RE table
print(summary(remlMod)@REmat, quote = FALSE)

VarCorr(remlMod)

attr(VarCorr(remlMod)$subid, "correlation")

## Print G matrix (REML)
data.frame(VarCorr(remlMod)$subid)

## Print G matrix (ML)
data.frame(VarCorr(mlMod)$subid)



###################################################
### Random Effects and Correlated Data
###################################################

## No random effects
lm.0 <- lm(read ~ 1 + grade5, data = mpls.l)	## Fit model
s0 <- summary(lm.0)$sigma 						## Estimated error SD
V0 <- diag(s0 ^ 2, nrow = 4, ncol = 4)          ## 4 by 4 diagonal matrix
round(V0, 2)                                    ## Round results

## V --> V*
Vstar <- function(x){
	diag(1 / sqrt(diag(x))) %*% x %*% diag(1 / sqrt(diag(x)))
	}

Vstar0 <- Vstar(V0)
round(Vstar0, 2)


## Random intercepts
lmer.1 <- lmer(read ~ 1 + grade5 + (1 | subid), data = mpls.l)
G1 <- matrix(as.numeric(VarCorr(lmer.1)$subid), ncol = 1)
s1 <- attr(VarCorr(lmer.1), "sc")
G1
s1

V <- function(w, x, y, z){
    nRE <- nrow(summary(z)@REmat) - 1                   ## Count random effects
    Z <- matrix(model.matrix(z)[1:4,1:nRE], ncol = nRE) ## Create Z matrix
    R <- diag(w) * y ^ 2                                ## Create R matrix
    V <- Z %*% x %*% t(Z) + R                           ## Compute V
	}

V1 <- V(4, G1, s1, lmer.1)
round(V1, 2)

Vstar1 <- Vstar(V1)
round(Vstar1, 2)


## Two Uncorrelated Random Effects
lmer.2 <- lmer(read ~ 1 + grade5 + (1 | subid) + (0 + grade5 | subid), data = mpls.l)

as.data.frame(VarCorr(lmer.2))

G2 <- diag(as.numeric(VarCorr(lmer.2)))
s2 <- attr(VarCorr(lmer.2), "sc")
G2
s2

V2 <- V(4, G2, s2, lmer.2)
round(V2, 2)

Vstar2 <- Vstar(V2)
round(Vstar2, 2)


## Two Correlated Random Effects
lmer.3 <- lmer(read ~ 1 + grade5 + (1 + grade5 | subid), data = mpls.l)
G3 <- matrix(as.numeric(VarCorr(lmer.3)$subid), ncol = 2)
s3 <- attr(VarCorr(lmer.3), "sc")
G3
s3

V3 <- V(4, G3, s3, lmer.3)
round(V3, 2)

Vstar3 <- Vstar(V3)
round(Vstar3, 2)


## Three Correlated Random Effects
lmer.4 <- lmer(read ~ 1 + grade5 + I(grade5 ^ 2) + (1 + grade5 + I(grade5 ^ 2) | subid), data = mpls.l)

G4 <- matrix(as.numeric(VarCorr(lmer.4)$subid), ncol = 3)
s4 <- attr(VarCorr(lmer.4), "sc")
G4
s4

attr(VarCorr(lmer.4)$subid, "correlation")

V4 <- V(4, G4, s4, lmer.4)
round(V4, 2)

Vstar4 <- Vstar(V4)
round(Vstar4, 2)



###################################################
### OLS Estimates
###################################################

## linear model
ols <- function(x){
	lm(read ~ 1 + grade5, data = x)
	}

## Fit linear model to each subject
my.ols <- dlply(.data = mpls.l, .variables = .(mpls.l$subid), .fun = ols)

## Get coefficients from output
my.coefs <- ldply(.data = my.ols, .fun = function(x){coef(x)})
my.coefs

## Covariance
cov(my.coefs[ ,-1])

## Create data frame of coefficients
plotdata <- data.frame(coef = c(my.coefs[ ,2], my.coefs[ ,3]), 					## vector of intercepts followed by slopes
	label = c(rep("Intercept", nrow(my.coefs)), rep("Slope", nrow(my.coefs))),  ## vector of labels
	ph = rep(" ", nrow(my.coefs)))												## vector of spaces
head(plotdata)

## Plot
ggplot(data = plotdata, aes(x = ph, y = coef)) + ## phantom character ph used to make plot look better
	geom_boxplot() + 
	theme_bw() +
	facet_grid(label ~ ., scales = "free") + ## Allow different scales
	xlab("") + 
	ylab("Scale")
	
## Correlations
cor(my.coefs[ ,-1])

## Scatterplot of relationship between coefficients
ggplot(data = my.coefs, aes(x = my.coefs[ , 2], y = my.coefs[ , 3])) + 
	geom_point() + 
	stat_smooth(method = "lm", se = FALSE) +
	theme_bw() + 
	xlab("Intercept") + 
	ylab("Slope")

inter.r2 <- summary(lm(my.coefs[ , 2] ~ mpls.l$dadv[mpls.l$grade == 5]))$r.squared
slope.r2 <- summary(lm(my.coefs[ , 3] ~ mpls.l$dadv[mpls.l$grade == 5]))$r.squared
round(inter.r2, 2)
round(slope.r2, 2)	
	
	
	
###################################################
### Examining Residuals
###################################################

## Estimate the models
lm.1 <- lm(read ~ 1 + grade5, data = mpls.l)
lmer.1 <- lmer(read ~ 1 + grade5 + (1 | subid), data = mpls.l)

## Residual variances
lm.s2 <- summary(lm.1)$sigma ^ 2
lmer.s2 <- attr(VarCorr(lmer.1), "sc")
names(lmer.s2) <- ""
data.frame(lm.s2, lmer.s2, row.names = "")

## PRE measure.
PRE <- (lm.s2 - lmer.s2) / lm.s2
PRE

## Create data frame of residuals
my.resid <- data.frame(
	subid = lmer.1@frame$subid,
	lm.resid = resid(lm.1),
	lmer.resid = resid(lmer.1),
	lm.l = I("LM"), 
	lmer.l = I("LMER")
	)
head(my.resid)

## Stack the variables into a new data frame
plotdata <- with(my.resid, data.frame(
	subid = c(subid, subid),
	resid = c(lm.resid, lmer.resid),
	model = c(lm.l, lmer.l))
	)
head(plotdata)

## Plot the residuals by subject ID
ggplot(data = plotdata, aes(x = subid, y = resid, group = subid)) + 
	theme_bw() +
	geom_boxplot(fill = "grey80") + 
	coord_flip() + 
	geom_hline(yintercept = 0) +
	facet_grid(. ~ model) + 
	ylab("Residual") + 
	xlab("Subject") + 
	ylim(-50, 50)


## Reduction in variance for random slopes model
lmer.2 <- lmer(read ~ 1 + grade5 + (1 + grade5 | subid), data = mpls.l)
lmer.1.s2 <- lmer.s2
lmer.2.s2 <- attr(VarCorr(lmer.2), "sc")
names(lmer.2.s2) <- ""
PRE <- (lmer.1.s2 - lmer.2.s2) / lmer.1.s2
PRE

## Create data frame
plotdata <- data.frame(
	subid = c(lmer.2@frame$subid, lmer.2@frame$subid),
	resid = c(resid(lmer.1), resid(lmer.2)),
	model = c(rep(1, nrow(lmer.2@frame)), rep(2, nrow(lmer.2@frame)))
	)
plotdata$model <- factor(plotdata$model, labels = c("Reduced", "Full"))
head(plotdata)

## Plot
ggplot(data = plotdata, aes(x = subid, y = resid, group = subid)) + 
	theme_bw() +
	geom_boxplot(fill = "grey80") + 
	coord_flip() + 
	geom_hline(yintercept = 0) +
	facet_grid(. ~ model) + 
	ylab("Residual") + 
	xlab("Subject")



###################################################
### Residuals and Normality
###################################################

## Fit model
lmer.2 <- lmer(read ~ 1 + grade5 + (1 + grade5 | subid), data = mpls.l)

## Examine residuals
library(sm)
sm.density(resid(lmer.2))

sm.density(resid(lmer.2), model = "norm")



###################################################
### LRT -- Test RE of Intercept
### H0: Var(b0i) = 0
### HA: Var(b0i) > 0
###################################################

library(RLRsim)

## reduced model 
## Random Intercepts model     var(b1i) = 0
lmer.0 <- lmer(read ~ 1 + grade5 + (1 | subid), data = mpls.l, REML = TRUE)

## full model 
## Random Intercepts and Slopes model    var(b1i) > 0
lmer.1 <- lmer(read ~ 1 + grade5 + (1 + grade5 | subid), data = mpls.l, REML = TRUE)

## fast bootstrap --- p < 0.001
exactRLRT(lmer.0)



###################################################
### LRT -- Test RE of Slope (Uncorrelated Random Effects)
### H0: Var(b1i) = 0
### HA: Var(b1i) > 0
###################################################

## Random intercepts and slope model
full <- lmer(read ~ 1 + grade5 + (1 | subid) + (0 + grade5 | subid), data = mpls.l)

## Random intercepts model
reduced.int <- lmer(read ~ 1 + grade5 + (1 | subid), data = mpls.l)

## Not a viable model of interest--needed for testing the full model
reduced.slope <- lmer(read ~ 1 + grade5 + (0 + grade5 | subid), data = mpls.l)

## Fast bootstrap --- p = 0.008
## m is model only containing VC to be tested
## mA is the full model containing all VC (model in HA)
## m0 is reduced model containing all VC of full model except that to be tested (model in H0)
exactRLRT(m = reduced.slope, mA = full, m0 = reduced.int)

## Analytic LRT --- p = 0.026
anova(reduced.int, full)

## Slow bootstrap function
slow.b <- function(x, y, z) {
	chisq.star <- numeric(x)                                 	  ## Storage vector.
	for(i in 1:x){                                                ## Loop for bootstrap.
		simDV <- simulate(y)                                      ## Simulate on reduced model.
    	full.s <- refit(z, simDV[ , 1])                           ## Refit full model.
    	reduced.s <- refit(reduced.int, simDV[ , 1])                  ## Refit reduced model.
    	chisq.star[i] <- -2*(logLik(reduced.s) - logLik(full.s))  ## Store the LR statistic.
		}
	mean(anova(y, z)[2, 5] < chisq.star)                          ## p-value.
	}

## Slow bootstrap --- p = 0.012
slow.b(999, reduced.int, full)



###################################################
### LRT -- Test RE of Slope (Correlated Random Effects)
### H0: Var(b1i) = Cov(b0i, b1i) = 0
### HA: at least Var(b1i) > 0
###################################################

lmer.2 <- lmer(read ~ 1 + grade5 + (1 + grade5 | subid), data = mpls.l)

## Get correlation
## If correlation is small, fast bootstrap is acceptable
attr(VarCorr(lmer.2)$subid, "correlation")

## Use slow bootstrap --- p =0.008
cor.re <- slow.b(999, reduced.int, lmer.2)
cor.re

## Analytic LRT --- p = 0.012
anova(reduced.int, lmer.2)



###################################################
### LRT -- Test of Covariance 
### H0: Cov(b0i, b1i) = 0
### HA: Cov(b0i, b1i) ≠ 0
###################################################

reduced <- lmer(read ~ 1 + grade5 + (1 | subid) + (0 + grade5 | subid), data = mpls.l)
full <- lmer(read ~ 1 + grade5 + (1 + grade5 | subid), data = mpls.l)

## Analytic LRT --- p = 0.047
anova(reduced, full)

## Slow bootstrap --- p = 0.040
cov.test <- slow.b(999, reduced, full)
cov.test


###################################################
### AICc
###################################################

## Random intercepts model
lmer.1 <- lmer(read ~ grade5 + (1 | subid), data = mpls.l)

## Random intercepts and slopes--Uncorrelated
lmer.2 <- lmer(read ~ grade5 + (1 | subid) + (0 + grade5 | subid), data = mpls.l)

## Random intercepts and slopes--Correlated
lmer.3 <- lmer(read ~ grade5 + (1 + grade5 | subid), data = mpls.l)

print(aictab(cand.set = list(lmer.1, lmer.2, lmer.3), modnames = c("M1", "M2", "M3")), LL = FALSE)



###################################################
### Variance Components and Static Predictors
### Choosing RE after FE have been selected
###################################################

## HA: Var(b1i) ≠ 0
full <- lmer(read ~ grade5 + dadv + ethW + (1 | subid) + (0 + grade5 | subid), data = mpls.l)

## H0: Var(b1i) = 0
reduced.int <- lmer(read ~ grade5 + dadv + ethW + (1 | subid), data = mpls.l)

## Model only containing VC for random slopes
reduced.slope <- lmer(read ~ grade5 + dadv + ethW + (0 + grade5 | subid), data = mpls.l)

## Fast bootstrap
## Test Var(b1i) = 0
exactRLRT(reduced.slope, full, reduced.int)



###################################################
### Predicted Random Effects
###################################################

my.lmer <- lmer(read ~ 1 + grade5 + (1 + grade5 | subid), data = mpls.l)

## Get corrected EBLUPs and put in data frame
my.c.re <- ldply(ranef(my.lmer))[-1]

## Give the columns a name
colnames(my.c.re) <- c("b0i", "b1i")
my.c.re

## Mean of the Random Effects
round(colMeans(my.c.re), 8)


###################################################
### Evaluating Normality Assumption of EBLUPs
###################################################

## Density plot b0i
sm.density(my.c.re$b0i, model = "norm")

## Boxplot b0i
boxplot(my.c.re$b0i)


## Q-Q plot b0
library(car)
qqPlot(my.c.re$b0i)

## Density plot b1
sm.density(my.c.re$b1i, model = "norm")

## Boxplot b1
boxplot(my.c.re$b1i)


## Q-Q plot b1
qqPlot(my.c.re$b1i)



###################################################
### Predicted Values for an Individual
###################################################

## Create 22 x 2 matrix using the fixed effects
my.beta <- data.frame(matrix(fixef(my.lmer), nrow = nrow(my.c.re), ncol = ncol(my.c.re), byrow = TRUE))
colnames(my.beta) <- c("B0", "B1")
head(my.beta)

## Sum FE and RE to obtain uncorrected EBLUPs
my.un.re <- my.beta + my.c.re
colnames(my.un.re) <- c("B0i", "B1i")
my.un.re

## Fit LMER model using REML
lmer.1 <- lmer(read ~ 1 + grade5 + (1 + grade5 | subid), data = mpls.l)

## Create data frame 1 of subid, read, grade and fitted values
plotdata <- data.frame(lmer.1@frame, fitted.re = fitted(lmer.1))
head(plotdata)

## Create data frame 2 of FE
fixed <- data.frame(fixef(lmer.1))
fixed

## Plot fitted lines and group line for each subject based on EBLUPs
ggplot(plotdata, aes(x = grade5, y = read)) + 
	geom_point() +
	facet_wrap(~ subid, nrow = 2) +								## Facet by subid
	geom_line(aes(y = fitted.re), linetype = 2) +               ## Individual fitted curve
	scale_x_continuous(breaks = 0:3) +
	geom_abline(intercept = fixed[1,1], slope = fixed[2,1]) +   ## Group fitted curve
	theme_bw()

## Plot fitted lines and group line for each subject based on OLS estimates
ggplot(plotdata, aes(x = grade5, y = read)) + 
	geom_point() +
	facet_wrap(~ subid, nrow = 2) +
	geom_line(aes(y = fitted.re), linetype = 2) + 		## EBLUP lines
	scale_x_continuous(breaks = 0:3) +
	stat_smooth(method = "lm", se = FALSE) + 				## OLS lines
	theme_bw()
