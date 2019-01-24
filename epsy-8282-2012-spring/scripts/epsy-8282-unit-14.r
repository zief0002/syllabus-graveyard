###################################################
### Load Libraries
###################################################	

library(ggplot2)
library(lme4)
library(AICcmodavg)
library(plyr)


###################################################
### Read in Minneapolis-Long2.csv data
###################################################	

mpls.l <- read.csv(file = "/Users/andrewz/Documents/EPSY-8282/Data/Minneapolis-Long2.csv")
mpls.l$grade5 <- mpls.l$grade - 5
mpls.l$dadv <- as.factor(ifelse(mpls.l$risk == "ADV", "ADV", "DADV"))
mpls.l$ethW <- as.factor(ifelse(mpls.l$eth == "Whi", "W", "NW"))



###################################################
### Read in Classroom.txt Data
###################################################	

classroom <- read.table(file = "/Users/andrewz/Documents/EPSY-8282/Data/Classroom.txt", header = TRUE)
head(classroom)

## Merge
MPLS.3 <- merge(mpls.l, classroom, by = c("subid"))

## Print header of nesting variables
with(MPLS.3, head(data.frame(classr, subid, grade), n = 24))

ggplot(data = MPLS.3, aes(x = grade, y = read, group = subid)) +
	geom_line(colour = "grey60") + 
	facet_grid(. ~ classr) + 
	opts(aspect.ratio = 2) +
	stat_smooth(aes(group = 1), method = "lm", se = F, lwd = 1.5) +
	scale_x_continuous(breaks = 5:8) + 
	scale_y_continuous(breaks = 0:1) +
	theme_bw()

## Create grade5 predictor
MPLS.3$grade5 <- MPLS.3$grade - 5

## Fit three-level model
thrlvl.1 <- lmer(read ~ 1 + grade5 + (1 + grade5 | subid) + (1 + grade5 | classr), data = MPLS.3, REML = FALSE)
print(thrlvl.1, cor = FALSE)


## Estimate model with subject-level static predictor (dadv )
thrlvl.2 <- lmer(read ~ 1 + grade5 * dadv + (1 + grade5 | subid) + (1 + grade5 | classr), data = MPLS.3, REML = FALSE)

## Print model results
print(thrlvl.2, cor = FALSE)


