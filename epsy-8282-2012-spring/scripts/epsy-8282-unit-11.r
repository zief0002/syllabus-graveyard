###################################################
### Statistical Analysis of Longitudinal Data I
### Spring 2012
### Andrew Zieffler
###
### Read in Minneapolis-Long2.csv and write to 
### object mpls.l.
###################################################

mpls.l <- read.csv(file = "/Users/andrewz/Documents/EPSY-8282/Data/Minneapolis-Long2.csv")

mpls.l$grade5 <- mpls.l$grade - 5
mpls.l$dadv <- as.factor(ifelse(mpls.l$risk == "ADV", "ADV", "DADV"))
mpls.l$ethW <- as.factor(ifelse(mpls.l$eth == "Whi", "W", "NW"))

library(ggplot2)  
library(lme4)
library(AICcmodavg)



###################################################
### Graphing Fitted Curves
###################################################

## Compute fitted values
FittedFE <- function(x){
	model.matrix(x) %*% fixef(x)
	}

## Fit model
lmer.1 <- lmer(read ~ 1 + grade5 + (1 + grade5 | subid), data = mpls.l, REML = FALSE)

## Examine fitted values
head(FittedFE(lmer.1))

## Create data frame of data and fitted values
plotdata <- data.frame(lmer.1@frame, fitted = FittedFE(lmer.1))

## Add grade predictor to data frame
plotdata$grade <- plotdata$grade5 + 5

## Examine data frame
head(plotdata, n = 11)

## Plot observed points, mean values and fitted group curve
ggplot(data = plotdata, aes(x = grade, y = read)) + 
	geom_point(shape = 19) +											## observed points
	stat_summary(fun.y = mean, geom = "point", size = 5, shape = 1) +	## mean values
	geom_line(aes(y = fitted), lwd = 1.5) + 							## fitted curve
	theme_bw() +
	scale_x_continuous(breaks=5:8) + 
	opts("aspect.ratio" = 1)

## Fit interaction model
lmer.2 <- lmer(read ~ 1 + grade5 + dadv + ethW + grade5:dadv + grade5:ethW + 
	(1 + grade5 | subid), data = mpls.l, REML = FALSE)

## Create data frame
plotdata <- data.frame(lmer.2@frame, fitted = FittedFE(lmer.2))
plotdata$grade <- plotdata$grade5 + 5  ## Add grade predictor
head(plotdata)

## Compute mean fitted values
my.m <- ddply(data.frame(plotdata$fitted), .(plotdata$dadv, plotdata$grade), mean)
colnames(my.m) <- c("risk2", "grade", "mfitted")
my.m

## Plot mean values and fitted curves for risk groups averaging across ethnicity
ggplot(data = plotdata, aes(x = grade, y = read, shape = dadv)) + 
	stat_summary(fun.y = mean, geom = "point") + 
	stat_summary(fun.y = mean, geom = "line", aes(y = fitted, linetype = dadv)) +
	theme_bw() +
	scale_x_continuous(breaks=5:8) + 
	opts("aspect.ratio" = 1)


## Compute mean fitted values for ethW
my.m <- ddply(data.frame(plotdata$fitted), .(plotdata$ethW, plotdata$grade), mean)
colnames(my.m) <- c("ethW", "grade", "mfitted")
my.m

## Plot mean values and fitted curves for ethnicity averaging across risk
ggplot(data = plotdata, aes(x = grade, y = read, shape = ethW)) + 
	stat_summary(fun.y = mean, geom = "point") + 
	stat_summary(fun.y = mean, geom = "line", aes(y = fitted, linetype = ethW)) +
	theme_bw() +
	scale_x_continuous(breaks=5:8) + 
	opts("aspect.ratio" = 1)
	


###################################################
### Static Predictors with Multiple Levels
###################################################

## Print group labels
levels(mpls.l$risk)

## Fit model
lmer.3 <- lmer(read ~ 1 + grade5 + risk + grade5:risk + (1 + grade5 | subid), data = mpls.l, REML = FALSE)

## Examine summary output
print(lmer.3, cor = FALSE)

## Relevel the risk factor
mpls.l$rrisk <- relevel(mpls.l$risk, ref = "HHM")

## Fit model with HHM as reference group
lmer.3A <- lmer(read ~ 1 + grade5 + rrisk + grade5:rrisk + (1 + grade5 | subid), data = mpls.l, REML = FALSE)

## Examine summary output
print(lmer.3A, corr = FALSE)

## Examine overall model fit
summary(lmer.3)@AICtab
summary(lmer.3A)@AICtab

## Create data frame
plotdata <- data.frame(lmer.3@frame, fitted = FittedFE(lmer.3))
plotdata$grade <- plotdata$grade5 + 5

## Plot mean values and fitted curves
ggplot(data = plotdata, aes(x = grade, y = read, shape = risk)) +
	stat_summary(fun.y = mean, geom = "point") +
	stat_summary(fun.y = mean, geom = "line", aes(y = fitted, linetype = risk)) +
	theme_bw() +
	scale_x_continuous(breaks=5:8) + 
	opts("aspect.ratio" = 1) + 
	scale_shape(solid = FALSE)


###################################################
### Evaluating Sets of Dummy Variables
###################################################

## Fit model with no group effects
reduced <- lmer(read ~ 1 + grade5 + (1 + grade5 | subid), data = mpls.l, REML = FALSE)

## Fit model with group effects
full <- lmer(read ~ 1 + grade5 + risk + risk:grade5 + (1 + grade5 | subid), data = mpls.l, REML = FALSE)

## Compare models
anova(reduced, full)

## Fit model with group intercept effects
full.1 <- lmer(read ~ 1 + grade5 + risk + (1 + grade5 | subid), data = mpls.l, REML = FALSE)

## Fit model with group intercept and slope effects
full.2 <- lmer(read ~ 1 + grade5 + risk + grade5:risk + (1 + grade5 | subid), data = mpls.l, REML = FALSE)

## Compare models
anova(reduced, full.1, full.2)
	
	
	
###################################################
### Evaluating Individual Dummy Variables
###################################################

## Load multcomp library
library(multcomp)

## Create contrast matrix
K <- rbind(
		"Int: POV-ADV" = c(0, 0, 1, 0, 0, 0),
		"Int: HHM-ADV" = c(0, 0, 0, 1, 0, 0),
		"Int: HHM-POV" = c(0, 0, 1, -1, 0, 0),
		"Slo: POV-ADV" = c(0, 0, 0, 0, 1, 0),
		"Slo: HHM-ADV" = c(0, 0, 0, 0, 0, 1),
		"Slo: HHM-POV" = c(0, 0, 0, 0, 1, -1)
    )

## Use fit model using specified contrast matrix
glht.3 <- glht(lmer.3, linfct = K)

## Summary output with no adjustment for multiple comparisons
summary(glht.3, test = adjusted(type = "none"))
summary(glht.3, test = adjusted(type = "bonferroni"))

## Confidence intervals with half-length of 1.96*SE
confint(glht.3, calpha = 1.96)


###################################################
### Interactions Among Static Predictors
###################################################

## Examine cross-tabulation at first time point
with(mpls.l[mpls.l$grade == 5, ], table(risk2, eth2))

## Fit model with all interactions
lmer.4 <- lmer(read ~ 1 + grade5 + dadv + ethW + 
	grade5:dadv + grade5:ethW + dadv:ethW + grade5:dadv:ethW +
	(1 + grade5 | subid), data = mpls.l, REML = FALSE)

## Summary output
print(lmer.4, cor = FALSE)

## Fit model with only interactions between grade effects and group terms
lmer.0 <- lmer(read ~ 1 + grade5 + dadv + ethW + 
	grade5:dadv + grade5:ethW + (1 + grade5 | subid), data = mpls.l, REML = FALSE)

## Compare models
anova(lmer.0, lmer.4)



###################################################
### Interpreting Interactions
###################################################

## Create data frame
plotdata <- data.frame(lmer.4@frame, fitted = FittedFE(lmer.4))
plotdata$group4 <- with(plotdata, interaction(ethW, dadv))      ## Create new interaction factor
plotdata$grade <- plotdata$grade5 + 5							## Create grade predictor

## Examine data frame
head(plotdata)

## Get interaction factor group labels
levels(plotdata$group4)

## Observed data, mean values, fitted curves by interaction term
ggplot(data = plotdata, aes(x = grade, y = read)) +
	geom_line(aes(group = subid), colour = "grey80") +
	stat_summary(fun.y = mean, geom = "point", aes(group = 1)) +
	stat_summary(fun.y = mean, geom = "line", aes(y = fitted)) +
	facet_grid(ethW ~ dadv) +
	theme_bw() +
	scale_x_continuous(breaks=5:8)

## Superimpose group means and fitted curves on one plot
ggplot(data = plotdata, aes(x = grade, y = fitted, shape = group4, linetype = group4)) +
	stat_summary(fun.y = mean, geom = "point", size = 2.5) + 
	stat_summary(fun.y = mean, geom = "line") + 
	opts("aspect.ratio" = 1) +
	scale_shape(solid = FALSE) +
	theme_bw() +
	scale_x_continuous(breaks=5:8)



###################################################
### Indexes of Absolute Effect Size
###################################################

## Estimate models
lmer.1 <- lmer(read ~ 1 + grade5 + (1 + grade5 | subid), data = mpls.l, REML = FALSE)
lmer.2 <- lmer(read ~ 1 + grade5 + dadv + grade5:dadv + (1 + grade5 | subid), data = mpls.l, REML = FALSE)
lmer.3 <- lmer(read ~ 1 + grade5 + dadv + ethW + grade5:dadv + grade5:ethW + 
	(1 +grade5 | subid), data = mpls.l, REML = FALSE)

## Compute R-squared
my.rsq <- c(
	cor(y = lmer.1@frame$read, x = FittedFE(lmer.1)) ^ 2,
	cor(y = lmer.2@frame$read, x = FittedFE(lmer.2)) ^ 2,
	cor(y = lmer.3@frame$read, x = FittedFE(lmer.3)) ^ 2
	)
data.frame(model = c("lmer.1", "lmer.2", "lmer.3"), rsq = my.rsq)

## aictab()
myaicc <- as.data.frame(aictab(list(lmer.1, lmer.2, lmer.3), sort = FALSE, c("lmer.1", "lmer.2", "lmer.3"))[ ,-c(5,7)])

## Compute R-squared
myaicc$rsq <- c(
	cor(y = lmer.1@frame$read, x = FittedFE(lmer.1)) ^ 2,
	cor(y = lmer.2@frame$read, x = FittedFE(lmer.2)) ^ 2,
	cor(y = lmer.3@frame$read, x = FittedFE(lmer.3)) ^ 2
	)
	
myaicc



###################################################
### Random Intercepts Models
### PRV
###################################################

## Risk intercept effect model
reduced <- lmer(read ~ 1 + grade5 + (1 | subid), data = mpls.l, REML = FALSE)

## Risk intercept and slope effects model
full <- lmer(read ~ 1 + grade5 + dadv + (1 | subid), data = mpls.l, REML = FALSE)

v.r <- attr(VarCorr(reduced)$subid, "stddev")^2
v.f <- attr(VarCorr(full)$subid, "stddev")^2

v.r
v.f

## Proportion reduction in variance
PRV <- (v.r - v.f) / v.r
PRV



###################################################
### Random Slopes Models
### PRV
###################################################

## Risk intercept effect model
reduced <- lmer(read ~ 1 + grade5 + risk2 + (1 + grade5 | subid), data = mpls.l, REML = FALSE)

## Risk intercept and risk slope effects model
full <- lmer(read ~ 1 + grade5 + dadv + grade5:dadv + (1 + grade5 | subid), data = mpls.l, REML = FALSE)

## Compute model variances
v.r <- attr(VarCorr(reduced)$subid, "stddev")[2]^2
v.f <- attr(VarCorr(full)$subid, "stddev")[2]^2

## Proportion reduction in variance
PRV <- (v.r - v.f) / v.r
PRV



###################################################
### Omnibus PRV Model
### PRV
###################################################

reduced <- lmer(read ~ 1 + grade5 + (1 | subid), data = mpls.l, REML = FALSE)
full <- lmer(read ~ 1 + grade5 + dadv + grade5:dadv + (1 | subid), data = mpls.l, REML = FALSE)

## Determinant
v.r <- det(as.matrix(VarCorr(reduced)$subid))
v.f <- det(as.matrix(VarCorr(full)$subid))
v.r
v.f

## Proportion reduction in variance
PRV <- (v.r - v.f) / v.r
PRV


###################################################
### Simulation: Generation of Worthless Predictor (X1)
###################################################

## Create new data
mywide <- reshape(
	mpls.l, 
	v.names = "read", 
	timevar = "grade",
	idvar = "subid", 
	direction = "wide", 
	drop = "grade5"
	)

# Use to reproduce this example	
set.seed(1234)       
mywide$X1 <- round(rnorm(nrow(mywide), mean = 100, sd = 15), 0)
mylong <- reshape(mywide, direction = "long")
mylong <- mylong[order(mylong$subid), ]
head(mylong)

## Estimate the models
sim.1 <- lmer(read ~ 1 + grade + (1 + grade | subid), data = mpls.l, REML = FALSE)
sim.2 <- lmer(read ~ 1 + grade + dadv + grade:dadv + (1 + grade | subid), data = mpls.l, REML = FALSE)
sim.3 <- lmer(read ~ 1 + grade + dadv + ethW + grade:dadv + grade:ethW + (1 + grade | subid), data = mpls.l, REML = FALSE)
sim.4 <- lmer(read ~ 1 + grade + dadv + ethW + X1 + 
	grade:dadv + grade:ethW + grade:X1 + (1 + grade | subid), data = mylong, REML = FALSE)

## Extract the variances
myvar <- data.frame(
	rbind(
		attr(VarCorr(sim.1)$subid,"stddev") ^ 2,
		attr(VarCorr(sim.2)$subid,"stddev") ^ 2,
		attr(VarCorr(sim.3)$subid,"stddev") ^ 2,
		attr(VarCorr(sim.4)$subid,"stddev") ^ 2
		)
	)
		
rownames(myvar) <- c("(none) ", "risk2", "risk2.eth2", "risk2.eth2.X1")


## Compute the determinant of G
myvar$det <- c(
	det(as.matrix(VarCorr(sim.1)$subid)),
	det(as.matrix(VarCorr(sim.2)$subid)),
	det(as.matrix(VarCorr(sim.3)$subid)),
	det(as.matrix(VarCorr(sim.4)$subid))
	)
               
## Compute R-squared
myvar$rsq <- c(
	cor(x = sim.1@frame$read, y = FittedFE(sim.1))^2,
	cor(x = sim.2@frame$read, y = FittedFE(sim.2))^2,
	cor(x = sim.3@frame$read, y = FittedFE(sim.3))^2,
	cor(x = sim.4@frame$read, y = FittedFE(sim.4))^2
	)
	
## Deviance
myvar$deviance <- c(deviance(sim.1), deviance(sim.2), deviance(sim.3), deviance(sim.4))

## AIC
myvar$AIC <- c(AIC(sim.1), AIC(sim.2), AIC(sim.3), AIC(sim.4))

## Print the results
colnames(myvar)[1:2] <- c("Var(int)", "Var(slopes)")
myvar



###################################################
### Transforming for Standardized Change
###################################################





###################################################
### Standardizing and Compositing
###################################################

mpls.l$read.norm <- scale(mpls.l$read, center = 206.7, scale = 15.6)
with(mpls.l, head(data.frame(subid, grade, read, read.norm)))

norm.1 <- lmer(read.norm ~ 1 + grade5 + (1 + grade5 | subid), data = mpls.l, REML = FALSE)
summary(norm.1)@coefs

## Read in mytab.txt data
MTWO <- read.table("http://www.sagepub.com/long/chapters/datasets/82689_11ds.txt", header = TRUE)
head(MTWO)

## Reshape to long format
MTWO.L <- reshape(
	data = MTWO, 
	idvar = "Subject",
	varying = list(2:3, 4:5), 
	v.names = c("a", "b"),
	direction = "long"
	)

head(MTWO.L)

## Compute means and SD for each measure at both timepoints
with(MTWO.L, tapply(a, time, mean))
with(MTWO.L, tapply(a, time, sd))
with(MTWO.L, tapply(b, time, mean))
with(MTWO.L, tapply(b, time, sd))


## Composite then standardize
MTWO.L$c.z <- scale(MTWO.L$a + MTWO.L$b)

## Standardize then composite.
MTWO.L$z.c <- scale(MTWO.L$a) + scale(MTWO.L$b)

# Means

with(MTWO.L, tapply(c.z, time, mean))
with(MTWO.L, tapply(z.c, time, mean))

# Mean differences

with(MTWO.L, diff(tapply(c.z, time, mean)))
with(MTWO.L, diff(tapply(z.c, time, mean)))

round(data.frame(c.z.diff = diff(desc[, 2]), z.c.diff = diff(desc[ , 3])), 2)

