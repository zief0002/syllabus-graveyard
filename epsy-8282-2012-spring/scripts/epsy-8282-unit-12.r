## Chapter 12 R code.

require(ggplot2)
require(lme4)
require(AICcmodavg)

###################################################
### Read in data
### Tailor to your system
###################################################

MPLSW <- read.table(file = "/Users/andrewz/Documents/EPSY-8282/Data/MPLSW.txt", header = TRUE, na.strings = "-9")
MPLSW


###################################################
### Read in Minneapolis.csv data
### Tailor to your system
###################################################

mpls <- read.csv(file = "/Users/andrewz/Documents/EPSY-8282/Data/Minneapolis.csv", na.strings = "-9")


###################################################
### Define dadv
###################################################

mpls$dadv <- ifelse(mpls$risk == "ADV", "ADV", "DADV")
mpls$dadv <- factor(mpls$dadv)


###################################################
### Merge data sets
###################################################

mpls2 <- merge(MPLSW, subset(mpls, select = c(subid, dadv)), by = "subid")
head(mpls2)


###################################################
### Reshape data to long format
###################################################

mplsL <- reshape(mpls2, varying = 2:8, times = 2:8, idvar = "subid", timevar = "grade", direction = "long")

library(plyr)
mplsL <- arrange(mplsL, subid)
head(mplsL, n = 14)


###################################################
### Plot of data
###################################################

ggplot(mplsL, aes(x = grade, y = read, group = subid)) + 
	scale_x_continuous(breaks = 2:8) +
	theme_set(theme_bw()) +
	geom_line(colour = "grey80") + 
	stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3.5) +
	stat_summary(aes(group = 1), fun.y = mean, geom = "line", lwd=1.5) +
	facet_grid(. ~ dadv, margins = TRUE)


## For "(all)" panel in ggplot.
## mplsL2 <- mplsL # Copy data frame.
## mplsL2$dadv <- "(all)" # Relabel risk2.
## plotdata <- rbind(mplsL, mplsL2) # Stack data frames.

###################################################
### Plot of data
###################################################

## Create separate data frames

DADV <- subset(mplsL, dadv == "DADV")
ADV <- subset(mplsL, dadv == "ADV")

## DADV group

ggplot(DADV, aes(x = grade, y = read)) + 
	geom_line() + 
	facet_wrap( ~ subid) + 
	opts(title = "DADV") + 
	ylim(140, 240)

## ADV group

ggplot(ADV, aes(x = grade, y = read)) + 
	geom_line() + 
	facet_wrap( ~ subid) + 
	opts(title = "ADV") + 
	ylim(140, 240)


###################################################
### Find derivative of polynomial function
###################################################

quad <- expression(B0 + B1 * grade + B2 * grade ^ 2)
my.deriv <- D(quad, "grade")
my.deriv


###################################################
### Estimate model and extract fixed effects
###################################################

lmer.1 <- lmer(read ~ grade + I(grade ^ 2) + (grade | subid), data = mplsL, REML = FALSE)
my.fixef <- fixef(lmer.1)



## Assign values to B0, B1, B2

for(i in 1:3){
	assign(paste("B", i - 1, sep = ""), my.fixef[i])
	}
print(c(B0, B1, B2))

## Compute predicted values and derivatives.
grade <- 2:8
myd <- data.frame(grade, fitted = eval(quad), slp = eval(my.deriv))
myd

## Intercept of tangent line
myd$int <- myd$fitted - myd$slp * myd$grade

## Construct separate graphs for grades 3
ggplot(data = myd[grade == 3, ], aes(x = grade, y = fitted)) + 
	geom_line(data = myd, aes(x = grade, y = fitted)) + 
	geom_abline(aes(intercept = int, slope = slp), linetype = 2) + 
	geom_point(aes(x = grade, y = fitted), size = 3) + 
	ylab("read") + 
	opts(aspect.ratio = 1)

## Construct separate graphs for grades 5
ggplot(data = myd[grade == 5, ], aes(x = grade, y = fitted)) + 
	geom_line(data = myd, aes(x = grade, y = fitted)) + 
	geom_abline(aes(intercept = int, slope = slp), linetype = 2) + 
	geom_point(aes(x = grade, y = fitted), size = 3) + 
	ylab("read") + 
	opts(aspect.ratio = 1)

## Construct separate graphs for grades 7
ggplot(data = myd[grade == 7, ], aes(x = grade, y = fitted)) + 
	geom_line(data = myd, aes(x = grade, y = fitted)) + 
	geom_abline(aes(intercept = int, slope = slp), linetype = 2) + 
	geom_point(aes(x = grade, y = fitted), size = 3) + 
	ylab("read") + 
	opts(aspect.ratio = 1)	



###################################################
### Cubic model 
###################################################

cubic <- expression(B0 + B1 * grade + B2 * grade ^ 2 + B3 * grade ^ 3)
my.deriv <- D(cubic, "grade")
my.deriv

## Estimate fitted curve
lm.1 <- lm(read ~ grade + I(grade ^ 2) + I(grade ^ 3), data = mplsL, subset = (subid == 1))
myfe <- coef(lm.1)

for(i in 1:4){
	assign(paste("B", i-1, sep = ""), myfe[i])
	}
print(c(B0, B1, B2, B3))

## Compute fitted values, derivatives, and intercepts
grade <- 2:8
myd <- data.frame(grade, fitted = eval(cubic), slp = eval(my.deriv))
myd$int <- with(myd, fitted - slp * grade)
myd


## Construct separate graphs for grades 3
ggplot(data = myd[grade == 3, ], aes(x = grade, y = fitted)) + 
	geom_line(data = myd, aes(x = grade, y = fitted)) + 
	geom_abline(aes(intercept = int, slope = slp), linetype = 2) + 
	geom_point(aes(x = grade, y = fitted), size = 3) + 
	ylab("read") + 
	opts(aspect.ratio = 1)	

## Construct separate graphs for grades 5
ggplot(data = myd[grade == 5, ], aes(x = grade, y = fitted)) + 
	geom_line(data = myd, aes(x = grade, y = fitted)) + 
	geom_abline(aes(intercept = int, slope = slp), linetype = 2) + 
	geom_point(aes(x = grade, y = fitted), size = 3) + 
	ylab("read") + 
	opts(aspect.ratio = 1)

## Construct separate graphs for grades 7
ggplot(data = myd[grade == 7, ], aes(x = grade, y = fitted)) + 
	geom_line(data = myd, aes(x = grade, y = fitted)) + 
	geom_abline(aes(intercept = int, slope = slp), linetype = 2) + 
	geom_point(aes(x = grade, y = fitted), size = 3) + 
	ylab("read") + 
	opts(aspect.ratio = 1)		
	
	
	
	
	
###################################################
### Orthogonal Polynomials
###################################################

p <- subset(mplsL, subid == 1, select = -c(read, dadv))
p$rawp <- poly(p$grade, 3, raw = TRUE)
p$mcor <- poly(p$grade - mean(p$grade), 3, raw = TRUE)
p$orth <- poly(p$grade, 3)
round(p[ ,-1], 4)

round(cor(p$rawp), 2)
round(cor(p$mcor), 2)
round(cor(p$orth), 2)

lmer.2 <- lmer(read ~ poly(grade, degree= 3, raw = TRUE) + (poly(grade, degree = 3, raw = TRUE) | subid), data = mplsL, REML = FALSE)

## Create polynomials
mplsL$rp <- poly(mplsL$grade - 2, degree = 2, raw = TRUE)
mplsL$mc <- poly(mplsL$grade - mean(mplsL$grade), degree = 2, raw = TRUE)
mplsL$op <- poly(mplsL$grade, degree = 2)

## Estimate models
lmer.raw <- lmer(read ~ rp + (rp[ ,1] | subid), data = mplsL, REML = FALSE)
lmer.mcor <- lmer(read ~ mc + (mc[ ,1] | subid), data = mplsL, REML = FALSE)
lmer.orth <- lmer(read ~ op + (op[ ,1] | subid), data = mplsL, REML = FALSE)

## Print fixed effects
round(summary(lmer.raw)@coefs, 4)
round(summary(lmer.mcor)@coefs, 4)
round(summary(lmer.orth)@coefs, 4)

## Print random effects
print(summary(lmer.raw)@REmat, quote = FALSE)
print(summary(lmer.mcor)@REmat, quote = FALSE)
print(summary(lmer.orth)@REmat, quote = FALSE)

## AIC
print(summary(lmer.raw)@AICtab, quote = FALSE)
print(summary(lmer.mcor)@AICtab, quote = FALSE)
print(summary(lmer.orth)@AICtab, quote = FALSE)



###################################################
### Step Up Analysis with Orthogonal Polynomials
###################################################

## Construct orthogonal polynomials
op1 <- poly(mplsL$grade, 1)
op2 <- poly(mplsL$grade, 2)
op3 <- poly(mplsL$grade, 3)

## Estimate the models
l.out <- lmer(read ~ op1 + (op1 | subid), data = mplsL, REML = FALSE)
q.out <- lmer(read ~ op2 + (op2[ ,1] | subid), data = mplsL, REML = FALSE)
c.out <- lmer(read ~ op3 + (op3[ ,1] | subid), data = mplsL, REML = FALSE)

## Model fit
print(aictab(list(l.out, q.out, c.out), c("linear", "quadratic", "cubic")), LL = FALSE)

## LRT
anova(l.out, q.out, c.out)



###################################################
### Trigonometric Functions
###################################################

## Create c.grade
mplsL$c.grade <- {2 * pi * (mplsL$grade - min(mplsL$grade)) / max(mplsL$grade - min(mplsL$grade))}

## Show data for first subject
head(data.frame(subid = mplsL$subid, grade = mplsL$grade, c.grade = mplsL$c.grade), n = 7)

## Derivative of linear-cosine model
cos <- expression(B0 + B1 * c.grade + B2 * cos(c.grade))
my.deriv <- D(cos, "c.grade")
my.deriv


###################################################
### Subject-Level
###################################################

## Get subject 13's data
subid13 <- with(mplsL[mplsL$subid == 13, ], data.frame(read, grade, c.grade))

## Estimate linear-cosine model
lm.cos <- lm(read ~ c.grade + I(cos(c.grade)), subid13)

## Create data frame for graphing
plotdata <- data.frame(read = subid13$read, grade = subid13$grade, fitted = fitted(lm.cos))

## Linear-cosine graph
ggplot(data = plotdata, aes(x = grade, y = read)) + 
	geom_point() + 
	geom_line(aes(y = fitted)) + 
	opts(aspect.ratio = 1)

## Quadratic polynomial graph
ggplot(data = plotdata, aes(x = grade, y = read)) + 
	geom_point() + 
	opts(aspect.ratio = 1) + 
	stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE)

## AICc to empirically select model
quad <- lm(read ~ poly(grade, 2), data = subid13)
cosine <- lm(read ~ c.grade + I(cos(c.grade)), data = subid13)
sine <- lm(read ~ c.grade + I(sin(c.grade)), data = subid13)
print(aictab(list(quad, cosine, sine), c("quad", "cosine", "sine")), LL = FALSE)


###################################################
### LMER
###################################################

## Fit LMER models
quad <- lmer(read ~ poly(grade, 2) + (poly(grade, 1) | subid), data = mplsL, REML = FALSE)
lincos <- lmer(read ~ c.grade + I(cos(c.grade)) + (c.grade | subid), data = mplsL, REML = FALSE)

## Compare models
print(aictab(list(quad, lincos), c("Quad", "Linear-Cosine")), LL = FALSE)


###################################################
### Fitted curves
###################################################

## Estimate "dummy" model
lmer.d <- lmer(read ~ 1 + grade + (1 + grade | subid), data = mplsL, REML = FALSE)

## Create plotting data set
plotdata <- data.frame(
	grade = lmer.d@frame$grade, 
	read = lmer.d@frame$read,
	q.pred = model.matrix(quad) %*% fixef(quad),
	c.pred = model.matrix(lincos) %*% fixef(lincos)
	)

## Plot linear-cosine model
ggplot(data = plotdata, aes(x = grade, y = read)) + 
	geom_point(colour = "grey60") +
	stat_summary(fun.y = mean, geom = "point", size = 3) + 
	opts(aspect.ratio = 1) +
	stat_summary(fun.y = mean, geom = "line", lwd = 1.5, aes(y = c.pred)) +
	opts(title = "linear-cosine")

## Plot quadratic model
ggplot(data = plotdata, aes(x = grade, y = read)) + 
	geom_point(colour = "grey60") +
	stat_summary(fun.y = mean, geom = "point", size = 3) + 
	opts(aspect.ratio = 1) +
	stat_summary(fun.y = mean, geom = "line", lwd = 1.5, aes(y = q.pred)) +
	opts(title = "quadratic")


###################################################
### Symbolic Derivatives of FP
###################################################

D(expression(B0 + B1 * grade), "grade")
D(expression(B0 + B1 * grade ^ -1), "grade")
D(expression(B0 + B1 * log(grade)), "grade")


###################################################
### Data-Driven FP Examination
###################################################

## Quadratic Model
quad <-  lmer(read ~ poly(grade, 2, raw = TRUE) + (grade | subid), data = mplsL, REML = FALSE)

## 1st Order FP models
n3 <-  lmer(read ~ I(grade ^  -3) + (I(grade ^  -3) | subid), data = mplsL, REML = FALSE)
n2 <-  lmer(read ~ I(grade ^  -2) + (I(grade ^  -2) | subid), data = mplsL, REML = FALSE)
n1 <-  lmer(read ~ I(grade ^  -1) + (I(grade ^  -1) | subid), data = mplsL, REML = FALSE)
n05 <- lmer(read ~ I(grade ^ -.5) + (I(grade ^ -.5) | subid), data = mplsL, REML = FALSE)
ze <-  lmer(read ~ I(log(grade))  + (I(log(grade))  | subid), data = mplsL, REML = FALSE)
p05 <- lmer(read ~ I(grade ^ .5)  + (I(grade ^ .5)  | subid), data = mplsL, REML = FALSE)
p1 <-  lmer(read ~ grade          + (grade          | subid), data = mplsL, REML = FALSE)
p2 <-  lmer(read ~ I(grade ^ 2)   + (I(grade ^ 2)   | subid), data = mplsL, REML = FALSE)
p3 <-  lmer(read ~ I(grade ^ 3)   + (I(grade ^ 3)   | subid), data = mplsL, REML = FALSE)

## Fit
mymods <- list(n3, n2, n1, n05, ze, p05, p1, p2, p3, quad)
mynames <- c("-3", "-2", "-1", "-.5", "0", ".5", "1", "2", "3", "Quad")
print(aictab(mymods, mynames), LL = FALSE)

## Graph data
plotdata <- data.frame(
	read = p1@frame$read, 
	grade = p1@frame$grade,
	ze.pred = model.matrix(ze) %*% fixef(ze),
	quad.pred = model.matrix(quad) %*% fixef(quad)
	)

## Plot of log transformation
ggplot(data = plotdata, aes(x = grade, y = read)) + 
	geom_point(colour = "grey80") +
	geom_line(aes(y = ze.pred), size = 1.5) +
	stat_summary(aes(y = read), fun.y = mean, geom = "point", size = 4) +
	opts(aspect.ratio = 1)

## Plot of quadratic
ggplot(data = plotdata, aes(x = grade, y = read)) + 
	geom_point(colour="grey80") +
	geom_line(aes(y = quad.pred), size = 1.5) +
	stat_summary(aes(y = read), fun.y = mean, geom = "point", size = 4) +
	opts(aspect.ratio = 1)
	


###################################################
### Second Order FP
###################################################

n2n1 <- lmer(read ~ I(grade ^ -2) + I(grade ^ -1) + (I(grade ^ -2) | subid), data = mplsL, REML = FALSE)
zeze <- lmer(read ~ I(log(grade)) + I(log(grade) ^ 2) + (I(log(grade)) | subid), data = mplsL, REML = FALSE)
mymods <- list(ze, n2n1, zeze, quad)
mynames <- c("( 0,   )", "(-2, -1)", "( 0,  0)", "( 1,  2)")
print(aictab(mymods, mynames), LL = FALSE)


###################################################
### Second Order FP with Static Predictor
###################################################

quadrisk <- lmer(read ~ poly(grade, 2) * dadv + (poly(grade, 1) | subid), data = mplsL, REML = FALSE)
logrisk <- lmer(read ~ I(log(grade)) * dadv + (I(log(grade)) | subid), data = mplsL, REML = FALSE)
print(aictab(list(quadrisk, logrisk), c("Quad", "Log")), LL = FALSE)

## Plot of the fitted models
plotdata <- data.frame(
	read = logrisk@frame$read, 
	grade = exp(logrisk@frame[ ,2]),
	dadv = logrisk@frame$dadv,
	q.pred = model.matrix(quadrisk) %*% fixef(quadrisk),
	l.pred = model.matrix(logrisk) %*% fixef(logrisk)
	)

## logrisk model
ggplot(data = plotdata, aes(x = grade, y = read)) + 
	geom_point(color = "grey80") +
	stat_summary(fun.y = mean, geom = "point", size = 4) +
	geom_line(aes(y = l.pred), lwd = 1.5) + 
	facet_grid(. ~ dadv) +
	opts(aspect.ratio = 1, title = "Log")


## quadrisk model
ggplot(data = plotdata, aes(x = grade, y = read)) + 
	geom_point(color = "grey80") +
	stat_summary(fun.y = mean, geom = "point", size = 4) +
	geom_line(aes(y = q.pred), lwd = 1.5) + 
	facet_grid(. ~ dadv) +
	opts(aspect.ratio = 1, title = "Quadratic")
	


###################################################
### 
###################################################

grade1 <- MPLS2.LS$grade-1
cquad <-  lmer(read ~ poly(grade1, 2, raw=T) + (grade1 | subid),
               data = MPLS2.LS, REML = FALSE)
cn3 <-  lmer(read ~ I(grade1 ^ -3)  + (I(grade1 ^ -3)  | subid), MPLS2.LS, REML = FALSE)
cn2 <-  lmer(read ~ I(grade1 ^ -2)  + (I(grade1 ^ -2)  | subid), MPLS2.LS, REML = FALSE)
cn1 <-  lmer(read ~ I(grade1 ^ -1)  + (I(grade1 ^ -1)  | subid), MPLS2.LS, REML = FALSE)
cn05 <- lmer(read ~ I(grade1 ^ -.5) + (I(grade1 ^ -.5) | subid), MPLS2.LS, REML = FALSE)
cze <-  lmer(read ~ I(log(grade1))  + (I(log(grade1))  | subid), MPLS2.LS, REML = FALSE)
cp05 <- lmer(read ~ I(grade1 ^ .5)  + (I(grade1 ^ .5)  | subid), MPLS2.LS, REML = FALSE)
cp1 <-  lmer(read ~ grade1          + (grade1          | subid), MPLS2.LS, REML = FALSE)
cp2 <-  lmer(read ~ I(grade1 ^ 2)   + (I(grade1 ^ 2)   | subid), MPLS2.LS, REML = FALSE)
cp3 <-  lmer(read ~ I(grade1 ^ 3)   + (I(grade1 ^ 3)   | subid), MPLS2.LS, REML = FALSE)
mymods <- list(cn3, cn2, cn1, cn05, cze, cp05, cp1, cp2, cp3, cquad)
mynames <- c("-3", "-2", "-1", "-.5", "0", "0.5", "1", "2", "3", "quad")
print(aictab(mymods, mynames), LL = FALSE)



###################################################
### Spline Model
###################################################

## Create spline terms within the data frame
mplsL$spline1 <- ifelse(mplsL$grade > 6, mplsL$grade - 6, 0)
head(data.frame(grade = mplsL$grade, spline1 = mplsL$spline1), n = 7)

## Run lmer() with spline term as second predictor
sp1 <- lmer(read ~ 1 + grade + spline1 + (1 + grade | subid), data = mplsL, REML = FALSE)
sp1

## Reduced model
sp0 <- lmer(read ~ 1 + grade + (1 + grade | subid), data = mplsL, REML = FALSE)

## LRT
anova(sp0, sp1)

## AICc
print(aictab(list(sp1, sp0), c("Spline", "Linear")), LL = FALSE)

## Create data frame for plot
plotdata <- data.frame(
	read = sp1@frame$read, 
	grade = sp1@frame$grade,
	pred = model.matrix(sp1) %*% fixef(sp1)
	)

## Plot the model	
ggplot(data = plotdata, aes(x = grade, y = read)) + 
	geom_point(colour = "grey80") +
	stat_summary(fun.y = mean, geom = "point", size = 3) +
	geom_line(aes(y = pred), size = 1.5)

## Create spline terms
mplsL$spline1 <- ifelse(mplsL$grade > 4, mplsL$grade - 4, 0)
mplsL$spline2 <- ifelse(mplsL$grade > 6, mplsL$grade - 6, 0)

## Use spline terms in lmer().
sp2 <- lmer(read ~ grade + spline1 + spline2 + (grade | subid), data = mplsL, REML = FALSE)
sp2

plotdata <- data.frame(
	read = sp2@frame$read, 
	grade = sp2@frame$grade,
	sp2.pred = model.matrix(sp2) %*% fixef(sp2)
	)
	
ggplot(data = plotdata, aes(x = grade, y = read)) + 
	geom_point(colour = "grey80") +
	stat_summary(fun.y = mean, geom = "point", size = 4) + 
	opts(aspect.ratio = 1) +
	geom_line(aes( y = sp2.pred), size = 1.5)




###################################################
### Optional Section
###################################################

X <- matrix(c(rep(1,4), 1:4, (1:4) ^ 2, (1:4) ^ 3), ncol = 4)
X
T <- t(X) %*% X
C <- chol(T)
C
X.star <- X %*% solve(C)
X.star
## poly() output.
poly(1:4, 3)

