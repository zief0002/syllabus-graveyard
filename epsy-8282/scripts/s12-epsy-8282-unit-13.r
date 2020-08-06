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

## Interaction model
dyn.int <- lmer(read ~ grade * incent + (grade | subid), data = MPLS, REML = FALSE)
print(dyn.int, corr = FALSE)

## Main effects model
dyn.main <- lmer(read ~ grade + incent + (grade | subid), data = MPLS, REML = FALSE)

## Compare models (LRT)
anova(dyn.main, dyn.int)

## Compare models (AICc)
print(aictab(list(dyn.main, dyn.int), c("Main Effects", "Interaction")), LL = FALSE)

## Model results
print(dyn.main, cor = FALSE)



