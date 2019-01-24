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
library(plyr)

mpls.l$grade5 <- mpls.l$grade - 5
mpls.l$dadv <- as.factor(ifelse(mpls.l$risk == "ADV", "ADV", "DADV"))
mpls.l$eth2 <- as.factor(ifelse(mpls.l$eth == "Whi", "W", "NW"))




###################################################
### Evaluation of two nested models
###################################################

## Estimate models.
model.0 <- lmer(read ~ 1 + grade5 + (1 + grade5 | subid), data = mpls.l, REML = FALSE)
model.1 <- lmer(read ~ 1 + grade5 + eth2 + (1 + grade5 | subid), data = mpls.l, REML = FALSE)

anova(model.0, model.1)

## Compute AIC.




###################################################
### Delta AIC
###################################################

aictab(cand.set = list(model.0, model.1), modnames = c("Reduced", "Full"), second.ord = FALSE)

delta.aic <- 11.918 - 2 * (7 - 6)

weight.r <- exp(-0.5 * delta.aic) / (1 + exp(-0.5 * delta.aic))
weight.f <- 1 - weight.r

weight.f / weight.r


###################################################
### chi-squared distribution probabilities
###################################################

pchisq( q = 2, df = 1, lower.tail = FALSE )

# Display the Student's t distributions with various
# degrees of freedom and compare to the normal distribution

x <- seq(0, 4, length=1000)
hx <- dchisq(x, df=1)

plot(x, hx, type="l", lty=1, xlab="x value", ylab="Density", main="Chi-Squared Distribution (df=1)")
lb=2; ub=4
i <- x >= lb & x <= ub
lines(x, hx)
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red")


mychi <- data.frame(chisq = seq(from = 0, to = 12, by = 0.05))
mychi$alpha <- pchisq(q = mychi$chisq, df = 1, lower.tail = FALSE)
mychi$delta.aic <- mychi$chisq - 2
mychi$w.r <-  exp(-.5 * mychi$delta.aic) / (1 + exp(-.5 * mychi$delta.aic))
mychi$w.f <- 1 - mychi$w.r
head(mychi)
tail(mychi)

ggplot(mychi, aes(x = w.f, y = alpha)) + 
	geom_line(lwd = 1.5) + 
	theme_bw() +
	geom_hline(yintercept = c(0.01, 0.05), linetype = 2) +
	geom_vline(xintercept = c(0.50, 0.90, 0.95), linetype = 2) +
	scale_x_continuous(breaks = seq(0.20, 1, 0.05)) +
	scale_y_continuous(breaks = seq(0, 1, 0.05)) +
	xlab("Full model weight of evidence") +
	ylab(expression(paste("Alpha (", alpha, ")")))




###################################################
### Step-Up
###################################################

## Estimate models.
model.0 <- lmer(read ~ grade5 + (grade5 | subid), mpls.l, REML = FALSE)
model.1 <- lmer(read ~ grade5 + eth2 + (grade5 | subid), mpls.l, REML = FALSE)
model.2 <- lmer(read ~ grade5 + eth2 + dadv + (grade5 | subid), mpls.l, REML = FALSE)
model.3 <- lmer(read ~ grade5 + eth2 + dadv + grade5 * eth2 + (grade5 | subid), mpls.l, REML = FALSE)
model.4 <- lmer(read ~ grade5 + eth2 + dadv + grade5 * eth2 + grade5 * dadv + (grade5 | subid), mpls.l, REML = FALSE)

## LRT.
myout <- anova(model.0, model.1, model.2, model.3, model.4)

## Effect size.
myout$delta.aic <- c(myout$Chisq - 2 * myout$"Chi Df")
myout$weight.f <- (1 - exp(-0.5 * myout$delta.aic) / (1 + exp(-0.5 * myout$delta.aic)))
myout$eratio.f <- myout$weight.f / (1 - myout$weight.f)

## Print.
myout[ ,-c(3:4)]

print(model.2, cor = FALSE)




###################################################
### Top down
###################################################

model.1 <- lmer(read ~ grade5 + eth2 + dadv + grade5 * eth2 + grade5 * dadv + (grade5 | subid), mpls.l, REML = FALSE)
round(summary(model.1)@coefs, 2)


model.2 <- lmer(read ~ grade5 + eth2 + dadv + grade5 * eth2 + (grade5 | subid), mpls.l, REML = FALSE)
myout <- anova(model.2, model.1)
myout$delta.aic <- c(myout$Chisq - 2 * myout$"Chi Df")
myout$weight.f <- (1 - exp(-0.5 * myout$delta.aic) / (1 + exp(-0.5 * myout$delta.aic)))
myout$eratio.f <- myout$weight.f / (1 - myout$weight.f)
myout[ ,-c(3:4)]



model.3 <- lmer(read ~ grade5 + eth2 + dadv + (grade5 | subid), mpls.l, REML = FALSE)
myout <- anova(model.3, model.2)
myout$delta.aic <- c(myout$Chisq - 2 * myout$"Chi Df")
myout$weight.f <- (1 - exp(-0.5 * myout$delta.aic) / (1 + exp(-0.5 * myout$delta.aic)))
myout$eratio.f <- myout$weight.f / (1 - myout$weight.f)
myout[ ,-c(3:4)]
round(summary(model.3)@coefs, 2)


model.4 <- lmer(read ~ grade5 + eth2 + (grade5 | subid), mpls.l, REML = FALSE)
myout <- anova(model.4, model.3)
myout$delta.aic <- c(myout$Chisq - 2 * myout$"Chi Df")
myout$weight.f <- (1 - exp(-0.5 * myout$delta.aic) / (1 + exp(-0.5 * myout$delta.aic)))
myout$eratio.f <- myout$weight.f / (1 - myout$weight.f)
myout[ ,-c(3:4)]



model.5 <- lmer(read ~ grade5 + (grade5 | subid), mpls.l, REML = FALSE)
myout <- anova(model.5, model.4)
myout$delta.aic <- c(myout$Chisq - 2 * myout$"Chi Df")
myout$weight.f <- (1 - exp(-0.5 * myout$delta.aic) / (1 + exp(-0.5 * myout$delta.aic)))
myout$eratio.f <- myout$weight.f / (1 - myout$weight.f)
myout[ ,-c(3:4)]



###################################################
### Parametric Bootstrap
###################################################

## Estimate models
model.0 <- lmer(read ~ grade5 + (grade5 | subid), mpls.l, REML = FALSE)
model.1 <- lmer(read ~ grade5 + eth2 + (grade5 | subid), mpls.l, REML = FALSE)

## Simulate bootstrap sample data
simDV <- simulate(model.0)

## Fit full and reduced models
b.full <- refit(model.1, simDV[ ,1])
b.reduced <- refit(model.0, simDV[ ,1])

## Compute bootstrap chi-squared statistic
chisq.star <- deviance(b.reduced) - deviance(b.full)
chisq.star

## bootstrap function
LRT <- function(r, f){ 
	simDV <- simulate(r)
	b.full <- refit(f, simDV[ ,1])
	b.reduced <- refit(r, simDV[ ,1])
	chisq.star <- deviance(b.reduced) - deviance(b.full)
	return(chisq.star)
	}

# Allows reader to replicate results
set.seed(101)

# Run LRT function
LRT( r = model.0, f = model.1 )

# bootstrap results
myboot <- rdply(.n = 999, .expr = LRT(r = model.0, f = model.1), .progress = "text")

head(myboot)
plot( density( myboot$V1 ))

# Color more extreme area
x <- density(myboot$V1)$x[296:512]
hx <- density(myboot$V1)$y[296:512]
lb=296; ub=512
i <- 1:217
polygon( c(x[1], x[i], x[217]), c(0, hx[i], 0), col = "red")

# Obtain number  as extreme or more extreme than the observed value
length( myboot$V1 [myboot$V1 >= 15.105] )




###################################################
### Optional Reading: Power
###################################################

power.func <- function(r, f, sample.rep, power.rep){
	power.results <- data.frame(matrix(ncol = 4, nrow = sample.rep))
	for(k in 1:sample.rep){
		cat("", "\n")
		cat(paste("Sample Size =", k*length(unique(r@frame[,ncol(r@frame)]))), "\n")
		pvalue <- numeric(power.rep)
		pb <- txtProgressBar(max = power.rep, style=3)
		for(j in 1:power.rep){
			Sys.sleep(0.001); setTxtProgressBar(pb, j) # Update progress bar.
			simdv <- matrix(unlist(simulate(f, nsim = k)), ncol = 1)
			mm <- NULL; mm1 <- NULL; c <- 0
			for(i in 1:k) {
				mm1 <- f@frame
				mm1[ ,ncol(mm1)] <- as.integer(mm1[ ,ncol(mm1)] + c)
				c <- max(mm1[ ,ncol(mm1)])
				mm <- rbind(mm, mm1)
				}
			mm[ ,1] <- simdv
			s.full <- lmer(formula(f), mm, REML = F)
			s.reduced <- lmer(formula(r), mm, REML = F)
			pvalue[j] <- anova(s.reduced, s.full)[2,7]
			Sys.sleep(.002)
			close(pb)
			}
		power.results[k,1] <- max(mm[ ,ncol(mm)])
		power.results[k,2] <- mean(pvalue <= .01)
		power.results[k,3] <- mean(pvalue <= .05)
		power.results[k,4] <- mean(pvalue <= .15)
		cat("", "\n")
		}
	cat("", "\n")
	cat("Finished", "\n")
	cat("", "\n")
	colnames(power.results) <- c("N", "alpha.01", "alpha.05", "alpha.15")
	return(power.results)
	}

## Estimate models.
model.0 <- lmer(read ~ grade5 + eth2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.1 <- lmer(read ~ grade5 * eth2 + (grade5 | subid), MPLS.LS, REML = FALSE)


#######################################################
## power.func(r, f, sample.rep, power.rep):
## r = reduced model object
## f = full model object
## sub.rep = subject size replication (e.g., 10)
## pow.rep = power replication (e.g., 999).
#######################################################
power.results <- power.func(model.0, model.1, 10, 999)

ggplot(power.results, aes(x = N, y = alpha.15)) + 
	geom_point() + 
	theme_bw() +
	stat_smooth(se = FALSE) + 
	ylab("Power") +
	geom_point(aes(y = alpha.05)) + 
	stat_smooth(aes(y = alpha.05), se = FALSE) +
	geom_point(aes(y = alpha.01)) + 
	stat_smooth(aes(y = alpha.01), se = FALSE) +
	annotate("text", x = 45,  y = .7, label = "alpha == .15", parse = TRUE) +
	annotate("text", x = 115, y = .6, label = "alpha == .05", parse = TRUE) +
	annotate("text", x = 145, y = .5, label = "alpha == .01", parse = TRUE) +
	scale_x_continuous(breaks = seq(25, 225, 25))


