###################################################
### Statistical Analysis of Longitudinal Data I
### Spring 2012
### Andrew Zieffler
###
### Read in Minneapolis-ML.csv data using RStudio. Write
### to object mpls.ml. Also read in Minneapolis-Long.csv 
### data using RStudio. Write to object mpls.l.
###################################################

library(ggplot2)  
library(lme4)


###################################################
### Plot regression line 
###################################################

ggplot( data = mpls.ml, aes( x = grade, y = read ) ) + 
    geom_point() +
    stat_smooth( method = "lm", se = FALSE ) +
    theme_bw() +
    scale_x_continuous( name = "Grade", breaks = 5:8 ) +
    scale_y_continuous( name =  "Reading" )


###################################################
### Normal probabilities 
###################################################

dnorm(-1, mean = 0, sd = sqrt(1))


###################################################
### Plot of normal probabilities 
###################################################

norm.ex <- data.frame(eps = seq(from = -4, to = 4, by = 0.1) )
norm.ex$f.of.eps <- dnorm(norm.ex$eps, mean = 0, sd = 1)

ggplot(data = norm.ex, aes(x = eps, y = f.of.eps)) + 
	geom_line() +
	geom_abline(intercept = 0, slope = 0, linetype = 2) + 
	ylab(expression(italic(f)(epsilon))) + 
	xlab(expression(paste("Epsilon ", (epsilon)))) + 
	theme_bw()



###################################################
### Function to compute deviance for intercept only model
###################################################

dev.func <- function(B1) {
	4 * log(2 * pi * 49) + (1 / 49) * sum((mpls.ml$read - 102 - B1 * mpls.ml$grade) ^ 2)
	}
	
## Generate the values of the deviance and the store results.
dev.store <- mdply(data.frame(B1 = seq(from = 13, to = 17, by = 0.1)), dev.func)
colnames(dev.store)[2] <- "deviance"
head(dev.store)

## Find the minimum deviance.
subset(dev.store, deviance == min(deviance))

## Plot the deviance.
ggplot(data = dev.store, aes(x = B1, y = deviance)) + 
	geom_line() + 
	geom_abline(intercept = min(dev.store$deviance), slope = 0, linetype = 2) + 
	ylab(expression(italic(deviance))) + 
	xlab(expression(beta[1])) + 
	theme_bw()



###################################################
### Function to compute deviance for intercept and slope model
###################################################

dev.func <- function(B0, B1) {
	4 * log(2 * pi * 49) + (1 / 49) * sum((mpls.ml$read - B0 - B1 * mpls.ml$grade) ^ 2)
	}
	
## Generate the values of the deviance and store the results.
dev.store <- mdply(expand.grid(B0 = seq(98, 106, by = 0.1), B1 = seq(12, 18, by = 0.1)), dev.func)
colnames(dev.store)[3] <- "deviance"
head(dev.store)

## Find the minimum deviance.
subset(dev.store, deviance == min(deviance))

## Plot the deviance.
wireframe(deviance ~ B0 * B1, dev.store)


###################################################
### Compute deviance for linear model
###################################################

lm.1 <- lm(read ~ grade, data = mpls.ml)  
logLik(lm.1)
-2 * as.numeric(lm.1)



###################################################
### Compute deviance for lmer model
###################################################

lmer.0 <- lmer(read ~ 1 + grade + (1  | subid), data = mpls.l, REML = FALSE, verbose = TRUE)



###################################################
### Plot intercept-only and slope-intercept models and residuals
###################################################

ggplot( data = mpls.ml, aes( x = grade, y = read ) ) + 
    geom_segment( aes(x = grade, y = read, xend = grade, yend = fitted(lm.1), lty = 2 ) ) +
    geom_point( aes(size = 2) ) +
    stat_smooth( method = "lm", se = FALSE ) +
    theme_bw() +
    scale_x_continuous( name = "Grade", breaks = 5:8 ) +
    scale_y_continuous( name =  "Reading", limits = c(160, 230) ) +
    opts(legend.position="none")


ggplot( data = mpls.ml, aes( x = grade, y = read ) ) + 
    geom_segment( aes(x = grade, y = read, xend = grade, yend = fitted(lm.0), lty = 2 ) ) +
    geom_point( aes(size = 2) ) +
    geom_abline( intercept=lm.0$coeff, slope=0, color="blue" ) +
    theme_bw() +
    scale_x_continuous( name = "Grade", breaks = 5:8 ) +
    scale_y_continuous( name =  "Reading", limits = c(160, 230) ) +
    opts(legend.position="none")



###################################################
### Comparing log-likelihoods
###################################################

lm.0 <- lm(read ~ 1, data = mpls.ml)
logLik(lm.0)
-2 * as.numeric(lm.0)

logLik(lm.1)
-2 * as.numeric(lm.1)



###################################################
### Comparing log-likelihoods
###################################################

lmer.1 <- lmer(read ~ 1 + grade + (1 + grade | subid), data = mpls.l, REML = FALSE)
lmer.2 <- lmer(read ~ 1 + grade + dadv + (1 + grade | subid), data = mpls.l, REML = FALSE)

## Print ML information.
summary(lmer.1)@AICtab

## Likelihood ratio test.
anova(lmer.1, lmer.2)



###################################################
### MLE standard errors, CIs, and p-values
###################################################

mpls.l$white <- ifelse( mpls.l$eth == "Whi", 1, 0 )
lmer.3 <- lmer(read ~ 1 + grade + dadv + white + (1 + grade | subid), data = mpls.l, REML = FALSE)

## Examine the estimates and SEs
summary(lmer.2)@coefs
summary(lmer.3)@coefs

## Table and compute the limits for the CIs
mytable <- as.data.frame( summary( lmer.2 )@coefs )
mytable$LCI <- mytable$Estimate - 2 * mytable$"Std. Error"
mytable$UCI <- mytable$Estimate + 2 * mytable$"Std. Error"
mytable


## Estimate p-values based on the Wald statistic
mytable$p.value <- 2 * pnorm( q = abs( mytable$"t value" ), lower.tail = FALSE ) 
round(mytable, 4)

