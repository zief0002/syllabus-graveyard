###################################################
### Statistical Analysis of Longitudinal Data I
### Spring 2012
### Andrew Zieffler
###
### Read in Minneapolis-Long.csv and write to 
### object mpls.l.
###################################################

library(ggplot2)  
library(lme4)
library(AICcmodavg)

mpls.l$grade5 <- mpls.l$grade - 5
mpls.l$dadv <- as.factor(ifelse(mpls.l$risk == "ADV", "ADV", "DADV"))
mpls.l$eth2 <- as.factor(ifelse(mpls.l$eth == "Whi", "W", "NW"))

###################################################
### Simulate read scores from known true model 
###################################################

mysample <- subset( mpls.l, grade == 5 )
true.model <- lm( read ~ 1, data = mysample )


set.seed( 123 )
sim.dv <- unlist( simulate( true.model ) )
sample.a <- data.frame( read = sim.dv, att = mysample$att )
head( sample.a )

ggplot(data = sample.a, aes(x = att, y = read)) + 
	geom_point() +
	geom_abline(intercept = 205.1, slope = 0) + 
	ylab("Reading Achievement") + 
	xlab("Attendance") + 
	theme_bw()



###################################################
### Fit candidate models to Sample A 
###################################################

lm.1a <- lm(read ~ att, data = sample.a)
lm.2a <- lm( read ~ att + I( att ^ 2 ), data = sample.a )


deviance(lm.1a)
deviance(lm.2a)



###################################################
### Generate Sample B, fit models, compute deviance
###################################################

set.seed( 12 )
sim.dv <- unlist( simulate( true.model ) )
sample.b <- data.frame( read = sim.dv, att = mysample$att )


lm.1b <- lm( read ~ att, data = sample.b )
lm.2b <- lm( read ~ att + I( att ^ 2 ), data = sample.b )

deviance(lm.1b)
deviance(lm.2b)



###################################################
### Predictive deviance
###################################################

N <- nrow(sample.b)
prdev.1b <- N * ( log( 2 * pi * sum(( sample.b$read - lm.1a$fitted.values ) ^ 2 )) + 1 )

prdev.1b <- N *  log( 2 * pi * sum( (sample.b$read - fitted( lm.1a ) ) ^ 2 ) + 1 )


prdev.2b <- N * ( log( 2 * pi * sum(( sample.b$read - lm.2a$fitted.values ) ^ 2 )) + 1 )
prdev.b <- data.frame(preddev = c(prdev.1b, prdev.2b))
rownames(prdev.b) <- c("Model 1", "Model 2")
prdev.b





###################################################
### Extract AIC from many models
###################################################

## Estimate models.
model.1 <- lmer(read ~ 1 + grade5 + dadv + (1 + grade5 | subid), data = mpls.l, REML = FALSE)
model.2 <- lmer(read ~ 1 + grade5 + eth2  + (1 + grade5 | subid), data = mpls.l, REML = FALSE)
model.3 <- lmer(read ~ 1 + grade5 + dadv + eth2 + (1 + grade5 | subid), data = mpls.l, REML = FALSE)

## Compute AIC.
aictab(cand.set = list(model.1, model.2, model.3), modnames = c("M1", "M2", "M3"), second.ord = FALSE)



###################################################
### Extract AICc from lmer models
###################################################

aictab(cand.set = list(model.1, model.2, model.3), modnames = c("M1", "M2", "M3"), sort = FALSE)



###################################################
### Confidence sets
###################################################

confset(cand.set = list(model.1, model.2, model.3), modnames = c("M1", "M2", "M3"))


###################################################
### Evidence ratio
###################################################

myaicc <- aictab(cand.set = list(model.1, model.2, model.3), modnames = c("M1", "M2", "M3"), sort = FALSE)
evidence(myaicc)
evidence(myaicc, model.low="M1")
evidence(myaicc, model.high = "M3", model.low = "M1")



###################################################
### Multimodal inference
###################################################

## estimate models
model.1 <- lmer(read ~ 1 + grade5 + dadv + (1 + grade5 | subid), data = mpls.l, REML = FALSE)
model.2 <- lmer(read ~ 1 + grade5 + eth2  + (1 + grade5 | subid), data = mpls.l, REML = FALSE)
model.3 <- lmer(read ~ 1 + grade5 + dadv + eth2 + (1 + grade5 | subid), data = mpls.l, REML = FALSE)
model.4 <- lmer(read ~ 1 + grade5 + dadv + grade5:dadv + (1 + grade5 | subid), data = mpls.l, REML = FALSE)
model.5 <- lmer(read ~ 1 + grade5 + eth2 + grade5:eth2  + (1 + grade5 | subid), data = mpls.l, REML = FALSE)
model.6 <- lmer(read ~ 1 + grade5 + dadv + eth2 + dadv:grade5 + eth2:grade5 + (1 + grade5 | subid), data = mpls.l, REML = FALSE)


## effect sizes
myaicc <- aictab(cand.set = list(model.1, model.2, model.3, model.4, model.5, model.6), modnames = c("M1", "M2", "M3", "M4", "M5", "M6"))
myaicc
ERatio <- max(myaicc$AICcWt) / myaicc$AICcWt
ERatio

## create data frame
myaicc <- data.frame(myaicc, ERatio)

## Create bar graph (AICcWt)
ggplot( myaicc, aes (x = Modnames, y = AICcWt ) ) + 
	theme_set( theme_bw() ) +
	scale_x_discrete( name = "Model" ) +
	scale_y_continuous( limits = c(0,1), name = "Weight" ) + 
	geom_bar( fill = "grey80", color = "black" ) 


## Create bar graph (ERatio)
ggplot( myaicc, aes (x = Modnames, y = ERatio ) ) + 
	theme_set( theme_bw() ) +
	scale_x_discrete( name = "Model" ) +
	scale_y_continuous( name = "Evidence Ratio" ) + 
	geom_bar( fill = "grey80", color = "black" ) 

## confidence set
confset(cand.set = list(model.1, model.2, model.3, model.4, model.5, model.6), modnames = c("M1", "M2", "M3", "M4", "M5", "M6"))



###################################################
### Examine best approximating model
###################################################

mytab <- data.frame( summary( model.2 )@coefs )
mytab

mytab$LCI <- mytab$Estimate - 2 * mytab$Std..Error
mytab$UCI <- mytab$Estimate + 2 * mytab$Std..Error
round(mytab, 2)

fitted <- model.matrix(model.2) %*% fixef(model.2)
myplotdata <- data.frame(model.2@frame, fitted)
myplotdata$grade <- myplotdata$grade5 + 5
myplotdata$Ethnicity <- as.factor(myplotdata$eth2)
levels(myplotdata$Ethnicity) <- c("NW", "W")


ggplot(myplotdata, aes( x = grade, y = read, linetype = Ethnicity ) ) +
	stat_summary( fun.y = "mean", geom = "point", cex = 2 ) +
	stat_summary( aes( y = fitted ), fun.y = "mean", geom = "line" ) +
	theme_bw() +
	scale_x_continuous( name = "Grade", breaks = 5:8 ) +
    scale_y_continuous( name =  "Reading") 

