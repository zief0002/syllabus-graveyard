###################################################
### Statistical Analysis of Longitudinal Data I
### Spring 2012
### Andrew Zieffler
###
### Read in Minneapolis-Long.csv and write to 
### object mpls.l.
###################################################

mpls.l <- read.csv("/Users/andrewz/Dropbox/EPSY-8282/Data/Minneapolis-Long.csv")

library(ggplot2)  
library(lme4)
library(AICcmodavg)
library(plyr)

options(scipen = 999, digits=4) 

mpls.l$grade5 <- mpls.l$grade - 5
mpls.l$dadv <- as.factor(ifelse(mpls.l$risk == "ADV", "ADV", "DADV"))
mpls.l$ethW <- as.factor(ifelse(mpls.l$eth == "Whi", "W", "NW"))




###################################################
### Evaluation of time transformations
###################################################

## Estimate models
model.i <- lmer(read ~ 1 + (1 | subid), data = mpls.l, REML = FALSE)
model.l <- lmer(read ~ 1 + grade5 + (1 | subid), data = mpls.l, REML = FALSE)
model.q <- lmer(read ~ 1 + grade5 + I(grade5 ^ 2) + (1 | subid), data = mpls.l, REML = FALSE)


## Compute AIC
myaicc <- as.data.frame(aictab(cand.set = list(model.i, model.l, model.q), modnames = c("I", "L", "Q"), sort = FALSE))[ , -c(5,7)]

## Compute evidence ratio
myaicc$eratio <- max(myaicc$AICcWt) / myaicc$AICcWt

myaicc

## Examine quadratic model coefficients
summary(model.q)@coefs

## Examine linear model coefficients
summary(model.l)@coefs


###################################################
### Analysis with Static Predictors
###################################################

## Estimate models
model.1l <- lmer(read ~ 1 + grade5 + dadv + (1 + grade | subid), data = mpls.l, REML = FALSE)
model.1q <- lmer(read ~ 1 + grade5 + I(grade5 ^ 2) + dadv + (1 + grade | subid), data = mpls.l, REML = FALSE)

model.2l <- lmer(read ~ 1 + grade5 + dadv + ethW + (1 + grade | subid), data = mpls.l, REML = FALSE)
model.2q <- lmer(read ~ 1 + grade5 + I(grade5 ^ 2) + dadv + ethW + (1 + grade | subid), data = mpls.l, REML = FALSE)

## Compute AIC
myaicc <- as.data.frame(aictab(cand.set = list(model.1l, model.1q, model.2l, model.2q), 
	modnames = c("1L", "1Q", "2L", "2Q"), sort = FALSE))[ , -c(5,7)]

## Compute evidence ratio
myaicc$eratio <- max(myaicc$AICcWt) / myaicc$AICcWt

myaicc




###################################################
### Analysis with Static Predictors
###################################################

## Estimate models.
model.i <- lmer(read ~ dadv + ethW + (1 | subid), mpls.l, REML = FALSE)
model.l <- lmer(read ~ grade5 * dadv + grade5 * ethW + (1 | subid), data = mpls.l, REML = FALSE)
model.q <- lmer(read ~ grade5 * dadv + grade5 * ethW + I(grade5 ^ 2) * dadv + I(grade5 ^ 2) * ethW + (1 | subid), 
  data = mpls.l, REML = FALSE)

## Compute AIC
myaicc <- as.data.frame(aictab(cand.set = list(model.i, model.l, model.q), modnames = c("I", "L", "Q"), sort = FALSE))[ , -c(5,7)]

## Compute evidence ratio
myaicc$eratio <- max(myaicc$AICcWt) / myaicc$AICcWt

myaicc




###################################################
### LRT without Static Predictors
###################################################

## Estimate models
model.i <- lmer(read ~ 1 + (1 | subid), data = mpls.l, REML = FALSE)
model.l <- lmer(read ~ 1 + grade5 + (1 | subid), data = mpls.l, REML = FALSE)
model.q <- lmer(read ~ 1 + grade5 + I(grade5 ^ 2) + (1 | subid), data = mpls.l, REML = FALSE)

## LRT
anova(model.i, model.l, model.q)



###################################################
### Missing Data
###################################################

## Create a vector of 0/1 that indicates missingness of read for each row
mpls.l$miss <- as.numeric(is.na(mpls.l$read))

## Compute the number of missing values for each subject
mysel <- ddply(.data = data.frame(mpls.l$miss), .variables = .(mpls.l$subid), .fun = sum)

## Change variable names
colnames(mysel) <- c("subid", "totmiss")

## Merge the number of missing values with the original data frame
mpls.l2 <- merge(mpls.l, mysel, by = "subid")

head(mpls.l2)




###################################################
### Comparisons based on R2
###################################################

## Function to fit lm
fit.linear <- function(x) {
	lm(read ~ grade5, data = x)
	}

## Fit lm to each subjects' data
mylm.1 <- dlply(.data = mpls.l, .variables = .(mpls.l$subid), .fun = fit.linear)

## Function to obtain coefficients
get.coef <- function(x) {
	x$coefficients
	}

## Obtain coefficients from each subject's model	
ldply(.data = mylm.1, .fun = get.coef)

## Fit quadratic
fit.quad <- function(x) {
	lm(read ~ grade5 + I(grade5 ^ 2), data = x)
	}
mylm.2 <- dlply(.data = mpls.l, .variables = .(mpls.l$subid), .fun = fit.quad)
ldply(.data = mylm.2, .fun = get.coef)

## Function to obtain R2
get.R2 <- function(x) {
	summary(x)$r.squared
	}
linear.r2 <- ldply(.data = mylm.1, .fun = get.R2)	
	
colnames(linear.r2) <- c("subid", "Rsq")	
head(linear.r2)

## Merge missingness and R2
Rsq1 <- merge(mysel, linear.r2, by = "subid")
Rsq1

## Do same for quadratic model
quad.r2 <- ldply(.data = mylm.2, .fun = get.R2)		
colnames(quad.r2) <- c("subid", "Rsq")	
head(quad.r2)
Rsq2 <- merge(mysel, quad.r2, by = "subid")
Rsq2

## Combine the two data frames into a new frame called plotdata
## Add variables to indicate polynomial term and degree of missingness
N <- nrow(Rsq1)
plotdata <- data.frame(rbind(Rsq1, Rsq2), c(rep(1, N), rep(2, N)))
colnames(plotdata)[4] <- "poly"
plotdata$poly.f <- factor(plotdata$poly, labels = c("Linear", "Quadratic"))
plotdata$missing.f <- factor(plotdata$totmiss, labels = c("Complete", "Missing"))
head(plotdata)

## Plot the R2 values
ggplot(data = plotdata, aes(x = poly.f, y = Rsq)) + 
	geom_boxplot(fill = "grey80") + 
	geom_point(position = "jitter") + 
	facet_grid(. ~ missing.f) + 
	theme_bw() + 
	xlab("Polynomial") + 
	ylab(expression(R ^ 2))

## Obtain median values
tapply(plotdata$Rsq, list(plotdata$missing.f, plotdata$poly.f), median)


###################################################
### Comparisons based on Adjusted R2
###################################################

mysub <- subset(mpls.l2, totmiss == 0)
mylm.1 <- dlply(.data = mysub, .variables = .(mysub$subid), .fun = fit.linear)
mylm.2 <- dlply(.data = mysub, .variables = .(mysub$subid), .fun = fit.quad)

## Get adjusted R2
get.adj.R2 <- function(x){
	summary(x)$adj.r.squared
	}

adjRsq1 <- ldply(.data = mylm.1, .fun = get.adj.R2)
colnames(adjRsq1) <- c("subid", "adjRsq")

adjRsq2 <- ldply(.data = mylm.2, .fun = get.adj.R2)
colnames(adjRsq2) <- c("subid", "adjRsq")

## Create data frame
N <- nrow(adjRsq1)
plotdata <- data.frame(rbind(adjRsq1, adjRsq2), c(rep(1, N), rep(2, N)))
colnames(plotdata)[3] <- "poly"
plotdata$poly.f <- factor(plotdata$poly, labels = c("Linear", "Quadratic"))

## Plot adjusted R2
ggplot(data = plotdata, aes(x = poly.f, y = adjRsq)) + 
	geom_boxplot(fill = "grey80") + 
	geom_point(position = "jitter") + 
	theme_bw() + 
	xlab("") + 
	ylab(expression(bar(R)^2))
	


###################################################
### Pooled Measures of Fit
###################################################

## Estimate coefficients.
my1 <- dlply(.data = mpls.l, .variables = .(mpls.l$subid), .fun = fit.linear)
my2 <- dlply(.data = mpls.l, .variables = .(mpls.l$subid), .fun = fit.quad)

## SSE
ssResid <- function(x){
	sum(resid(x) ^ 2)
	}

sse1 <- sum(ldply(.data = my1, .fun = ssResid)[ ,2])
sse2 <- sum(ldply(.data = my2, .fun = ssResid)[ ,2])

## Residual df
dfResid <- function(x){
	x$df.residual
	}

df1 <- sum(ldply(.data = my1, .fun = dfResid)[, 2])
df2 <- sum(ldply(.data = my2, .fun = dfResid)[, 2])

## Compute RSE meta
RSEmeta.1 <- sqrt(sse1/df1)
RSEmeta.2 <- sqrt(sse2/df2)
RSEmeta.1
RSEmeta.2


## Function to compute meta values for R2 and adj. R2
## Only the first 3 lines need to be changed

mylm.1 <- dlply(.data = mpls.l, .variables = .(mpls.l$subid), .fun = fit.linear)
mylm.2 <- dlply(.data = mpls.l, .variables = .(mpls.l$subid), .fun = fit.quad)
lm.objects <- c("mylm.1", "mylm.2")

###################################################################################
Rsqmeta <- numeric(length(lm.objects))
adjRsqmeta <- numeric(length(lm.objects))
for(i in 1:length(lm.objects)){
    ## Define functions.
    myfunc1 <- function(x) sum((x$model[ ,1] - mean(x$model[ ,1])) ^ 2) # SST.
    myfunc2 <- function(x) sum(resid(x) ^ 2)                            # SSR.
    myfunc3 <- function(x) length(x$model[ ,1]) - 1                     # df Total.
    myfunc4 <- function(x) x$df.residual                                # df Resid.
    ## SST.
    SSTotal <- ldply(eval(parse(text = lm.objects[i])), myfunc1)
    ## SSE.
    SSResid <- ldply(eval(parse(text = lm.objects[i])), myfunc2)
    ## Rsq-meta.
    Rsqmeta[i] <- 1 - (sum(SSResid[ ,2]) / sum(SSTotal[ ,2]))
    ## Degrees of freedom.
    dfTotal <- sum(ldply(eval(parse(text = lm.objects[i])), myfunc3)[ ,2])
    dfResid <- sum(ldply(eval(parse(text = lm.objects[i])), myfunc4)[ ,2])
    ## Adjusted Rsq-meta.
    adjRsqmeta[i] <- 1 - ((sum(SSResid) / dfResid) / (sum(SSTotal) / dfTotal))
	}
Rsq.meta    <- data.frame(lm.objects, Rsqmeta)
adjRsq.meta <- data.frame(lm.objects, adjRsqmeta)
###################################################################################

Rsq.meta
adjRsq.meta


###################################################
### Clustering of Subject Curves
###################################################

temp <- subset(mpls.l, select = c(subid, read, grade))

## Convert tot wide format
read.wide <- reshape(temp, v.names = "read", timevar = "grade", idvar = "subid", direction = "wide")
head(read.wide)

library(kml)
mycld <- as.cld(read.wide, timeReal = 5:8)

## Show real-time plot of clustering
kml(Object = mycld, print.cal = TRUE, print.traj = TRUE)

## Plot results
par(mfrow = c(1, 1), pty = "s")
plotCriterion(mycld)
mtext("Cluster Number (k)", side = 1, line = 3)   # Set label in bottom margin.
mtext("CHC", side = 2, line = 3)                  # Set label in left margin.

## Plot results for 2, 3, 4, and 5 clusters
par(mfrow = c(2,2), pty = "s")                        # 2 x 2 matrix, square graphs.
plot(mycld, y = 2, ylab = "Response", main = "k = 2") # Optional labeling included.
plot(mycld, y = 3, ylab = "Response", main = "k = 3")
plot(mycld, y = 4, ylab = "Response", main = "k = 4")
plot(mycld, y = 5, ylab = "Response", main = "k = 5")
par(mfrow = c(1,1), pty = "m")                        # Default settings.


