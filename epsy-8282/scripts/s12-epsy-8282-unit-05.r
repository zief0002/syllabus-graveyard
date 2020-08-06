###################################################
### Statistical Analysis of Longitudinal Data I
### Spring 2012
### Andrew Zieffler
###
### Read in Minneapolis-Long.csv data using RStudio. 
### Write to object mpls.l
###################################################

library(ggplot2)  



###################################################
### Linear model w/slope and intecept
###################################################

lm.1 <- lm(read ~ 1 + grade, data = mpls.l)
summary(lm.1)

## Optional to turn off scientific notation.
options(scipen = 999)  

## Create data frame from observations used to fit model.
mod.data <- fortify(lm.1)
mod.data

## Plot model
ggplot(data = mod.data, aes(x = grade, y = read)) + 
	geom_point(shape = 1) + 
	geom_line(aes(x = grade, y = .fitted)) +
	theme_bw() +
	scale_x_continuous(breaks = 5:8, name = "Grade") +
	scale_y_continuous(name = "Reading")



###################################################
### ANCOVA as regression
###################################################

## Create dummy coded predictor
mpls.l$dadv <- ifelse(mpls.l$risk == "ADV", 0, 1)
head(mpls.l)

## Fit ANCOVA model
lm.2 <- lm(read ~ 1 + grade + risk, data = mpls.l)
summary(lm.2)

## Create data frame from observations used to fit model.
mod.data2 <- fortify(lm.2)
head(mod.data2)

## Plot model
ggplot(data = mod.data2, aes(x = grade, y = read, group = dadv, color = as.factor(dadv))) + 
	geom_point(shape = dadv) + 
	geom_line(aes(x = grade, y = .fitted)) +
	theme_bw() +
	scale_x_continuous(breaks = 5:8, name = "Grade") +
	scale_y_continuous(name = "Reading")



###################################################
### Interaction model
###################################################

lm.3 <- lm(read ~ 1 + grade + dadv + grade:dadv, data = mpls.l)
summary(lm.3)

## Create data frame from observations used to fit model.
mod.data3 <- fortify(lm.3)
head(mod.data3)

## Plot model
ggplot(data = mod.data3, aes(x = grade, y = read, group = dadv, color = as.factor(dadv))) + 
	geom_point(shape = dadv) + 
	geom_line(aes(x = grade, y = .fitted)) +
	theme_bw() +
	scale_x_continuous(breaks = 5:8, name = "Grade") +
	scale_y_continuous(name = "Reading")



###################################################
### LMER Models
###################################################

library(lme4)

## Fit LMER model with random slopes and intercepts
lmer.1 <- lmer(read ~ 1 + grade + (1 + grade | subid), data = mpls.l, REML = FALSE)
summary(lmer.1)

## Examine coefficients from fixed slope and intercept model
round(summary(lm.1)$coefficients,4)

## Examine coefficents from random slopes and intercepts model
round(summary(lmer.1)@coefs, 4)

## Intercept-only (random intercepts) model.
lmer.0 <- lmer(read ~ 1 + (1 | subid), data = mpls.l, REML = FALSE)

## Error variance
round(summary(lmer.0)@sigma ^ 2, 2)



###################################################
### Anchor intercept
###################################################

## Anchor grade 5 as the intercept
mpls.l$grade5 <- mpls.l$grade - 5

lmer.1a <- lmer(read ~ 1 + grade5 + (1 + grade5 | subid), data = mpls.l, REML = FALSE)
summary(lmer.1a)

## Anchor grade 8 as the intercept within the model
lmer.1b <- lmer(read ~ 1 + I(grade - 8) + (1 + I(grade - 8) | subid), data = mpls.l, REML = FALSE)
summary(lmer.1b)



###################################################
### Random slopes and intercepts model w/static predictor
###################################################

lmer.2 <- lmer(read ~ 1 + grade + dadv + (1 + grade | subid), data = mpls.l, REML = FALSE)
summary(lmer.2)

## Interaction model
lmer.3 <- lmer(read ~ 1 + grade + dadv + dadv:grade + (1 + grade | subid), data = mpls.l, REML = FALSE)
summary(lmer.3)



###################################################
### Initial status as covariate
###################################################

## Extract 5th grade reading scores and subject IDs
grade5 <- subset(mpls.l, grade == 5, select = c(subid, read))

## Rename the 5th grade reading scores 'read.int'
names(grade5)[2] <- "read.int"

## Extract rows for 6th-8th grade
grade6to8 <- subset(mpls.l, grade != 5)

## Merge the two data frames together using the Subject ID as a connector
mpls.l2 <- merge(grade5, grade6to8, by = "subid")
head(mpls.l2)

## ANCOVA model
lmer.4 <- lmer(read ~ 1 + grade + read.int + (1 + grade | subid), data = mpls.l2, REML = FALSE)
summary(lmer.4)

## Interaction model
lmer.5 <- lmer(read ~ 1 + grade + read.int + read.int:grade + (1 + grade | subid), data = mpls.l2, REML = FALSE)
summary(lmer.5)



###################################################
### Syntax to accompany the optional reading
###################################################

head(model.matrix(lmer.1))
head(model.matrix(lmer.3))

## Estimate LMER model.
lmer.1 <- lmer(read ~ grade + (grade | subid), MPLS.LS, REML = FALSE)

## Random effects design matrix for first person.
Z <- model.matrix(lmer.1)[1:4, ]
Z

G <- VarCorr(lmer.1)$subid[1:2,1:2]
G

## Extract and save error variance.
sigma2 <- summary(lmer.1)@sigma ^ 2

## Create 4 x 4 Identity matrix.
Ident <- diag(4)

# Compute W.
W <- sigma2 * Ident
W

B <- Z %*% G %*% t(Z)
B

V <- B + W
V

## Create diagonal matrix, D.
D  <- diag(1 / sqrt(diag(V)))

## Compute Vstar.
Vstar <- D %*% V %*% D
Vstar

