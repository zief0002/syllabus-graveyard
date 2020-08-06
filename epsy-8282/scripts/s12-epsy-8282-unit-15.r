###################################################
### Load Libraries
###################################################	

library(ggplot2)
library(lme4)
library(AICcmodavg)
library(plyr)


###################################################
### Read in Incentive.csv and Minneapolis Data
###################################################	

## Read in Financial-Incentive.txt data
incentive <- read.table( "/Users/andrewz/Documents/EPSY-8282/Data/Financial-Incentive.txt", header = TRUE)

## Read in Minneapolis-Long2.csv data
mpls.l <- read.csv(file = "/Users/andrewz/Documents/EPSY-8282/Data/Minneapolis-Long2.csv")
mpls.l$grade5 <- mpls.l$grade - 5
mpls.l$dadv <- as.factor(ifelse(mpls.l$risk == "ADV", "ADV", "DADV"))
mpls.l$ethW <- as.factor(ifelse(mpls.l$eth == "Whi", "W", "NW"))

## Reshape incentive data
incentive.l <- reshape(
	incentive, 
	varying = c("incent.5", "incent.6", "incent.7", "incent.8"),
	timevar = "grade", 
	times = 5:8,
	idvar = "subid", 
	direction = "long"
	)
	
## Merge Incentive and Minneapolis Data
MPLS <- merge(mpls.l, incentive.l, by = c("subid", "grade"))

## Sort by id
MPLS <- arrange(MPLS, subid)
head(MPLS, n = 8)

## Select the first 6 subjects
plotdata <- na.omit(MPLS[MPLS$subid < 7, ])

## Create the graphs
ggplot(data = plotdata, aes(x = grade, y = read, group = subid)) +
	geom_line() + 
	geom_point() + 
	facet_wrap(~ subid, ncol = 2) + 
	scale_x_continuous(breaks = 5:8) + 
	scale_y_continuous(breaks = 0:1) +
	theme_bw()

ggplot(data = plotdata, aes(x = grade, y = incent, group = subid)) +
	geom_line() + 
	geom_point() + 
	facet_wrap(~ subid, ncol = 2) + 
	scale_x_continuous(breaks = 5:8) + 
	scale_y_continuous(breaks = 0:1) +
	theme_bw()
	
## Intercept-only
dyn.0 <- lmer(read ~ 1 + (1 | subid), data = MPLS, REML = FALSE)

## Dynamic predictor
dyn.1 <- lmer(read ~ incent + (1 | subid), data = MPLS, REML = FALSE)

## Comparison of fit
anova(dyn.0, dyn.1)
print(aictab(list(dyn.0, dyn.1), c("Intercept", "Dynamic")), LL = FALSE)

dyn.int <- lmer(read ~ grade * incent + (grade | subid), data = MPLS, REML = FALSE)
print(dyn.int, corr = FALSE)

dyn.main <- lmer(read ~ grade + incent + (grade | subid), data = MPLS, REML = FALSE)
anova(dyn.main, dyn.int)
print(aictab(list(dyn.main, dyn.int), c("Main Effects", "Interaction")), LL = FALSE)
print(dyn.main, cor = FALSE)



###################################################
### Read in Math.txt Data
###################################################	

MATH.W <- read.table("C:/Mine/math.txt", header = TRUE, na.strings = "-99") ## Tailor to your system.
head(MATH.W)

## Reshape
MATH.L <- reshape(
	MATH.W, 
	varying = 2:5, 
	idvar= "subid",
    direction = "long", 
    timevar = "grade"
    )
    
MATH.LS <- MATH.L[order(MATH.L$subid), ]

## Merge
READ.MATH <- merge(MPLS.LS, MATH.LS, by = c("subid", "grade"))
head(READ.MATH)

## Stack the data
plotdata <- with(READ.MATH, data.frame(
	response = c(read, math),
	grade = c(grade, grade), 
	subid = c(subid, subid),
	label = c(rep("read", nrow(READ.MATH)), rep("math", nrow(READ.MATH))))
	)
	
## ggplot2
ggplot(data = plotdata, aes(x = grade, y = response, group = subid)) +
	geom_line(colour = "grey80") + 
	facet_grid(.~ label) + 
	stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) +
	stat_summary(aes(group = 1), fun.y = mean, geom = "line", lwd = 1.5) +
	scale_x_continuous(breaks = 5:8) + 
	scale_y_continuous(breaks = 0:1) +
	theme_bw()

## Double stack
stackdata <- with(READ.MATH, data.frame(
	subid = c(subid, subid),
	grade = c(grade, grade), 
	response = c(read, math),
	label = c(rep("read", nrow(READ.MATH)), rep("math", nrow(READ.MATH))))
	)
	
## Create predictors
stackdata$read.int <- ifelse(test = (stackdata$label == "read"), 1, 0)
stackdata$math.int <- ifelse(stackdata$label== "math", 1, 0)
stackdata$read.grade <- ifelse(stackdata$label == "read", stackdata$grade, 0)
stackdata$math.grade <- ifelse(stackdata$label == "math", stackdata$grade, 0)

## Sort and print
stackdata2 <- arrange(stackdata, subid)
head(stackdata2, n = 8)

twodvs.1 <- lmer(response ~ 0 + read.int + math.int + read.grade + math.grade +
	(0 + read.int + math.int | subid), data = stackdata, REML = FALSE)
twodvs.1

twodvs.2 <- lmer(response ~ 0 + read.int + math.int + I(read.grade + math.grade) +
	(0 + read.int + math.int | subid), data = stackdata, REML = FALSE)
summary(twodvs.2)@coefs

anova(twodvs.2, twodvs.1)
print(aictab(list(twodvs.1, twodvs.2), c("Two slopes", "One slope")), LL = FALSE)

twodvs.1 <- lmer(response ~ 0 + read.int + math.int + I(read.grade + math.grade) +
	(0 + I(read.int + math.int) | subid), data = stackdata, REML = FALSE)
twodvs.2 <- lmer(response ~ 0 + I(read.int + math.int) + I(read.grade + math.grade) +
	(0 + I(read.int + math.int) | subid), data = stackdata, REML = FALSE)
anova(twodvs.1, twodvs.2)
print(aictab(list(twodvs.1, twodvs.2), c("Two intercepts", "One intercept")), LL = FALSE)



###################################################
### Read in Classroom.txt Data
###################################################	

classroom <- read.table("C:/Mine/classroom.txt", header = TRUE)
head(classroom)

## Merge
MPLS.LS.CL <- merge(MPLS.LS, classroom, by = c("subid"))

## Print header of nesting variables.
with(MPLS.LS.CL, head(data.frame(classr, subid, grade), n = 24))

ggplot(data = MPLS.LS.CL, aes(x = grade, y = read, group = subid)) +
	geom_line(colour = "grey60") + 
	facet_grid(. ~ classr) + 
	opts(aspect.ratio = 2) +
	stat_smooth(aes(group = 1), method = "lm", se = F, lwd = 1.5) +
	scale_x_continuous(breaks = 5:8) + 
	scale_y_continuous(breaks = 0:1) +
	theme_bw()

## Create grade5 predictor
MPLS.LS.CL$grade5 <- MPLS.LS.CL$grade - 5


thrlvl.1 <- lmer(read ~ grade5 + (1 + grade5 | subid) + (1 + grade5 | classr), data = MPLS.LS.CL, REML = FALSE)
print(thrlvl.1, cor = FALSE)

## Create risk2
MPLS.LS.CL$riskC[MPLS.LS.CL$risk == "HHM"] <- "DADV"
MPLS.LS.CL$riskC[MPLS.LS.CL$risk == "POV"] <- "DADV"
MPLS.LS.CL$riskC[MPLS.LS.CL$risk == "ADV"] <- "ADV"
MPLS.LS.CL$risk2 <- factor(MPLS.LS.CL$riskC)

## Estimate model
thrlvl.2 <- lmer(read ~ grade5 * risk2 + (1 + grade5 | subid) + (1 + grade5 | classr), data = MPLS.LS.CL, REML = FALSE)
print(thrlvl.2, cor = FALSE)


