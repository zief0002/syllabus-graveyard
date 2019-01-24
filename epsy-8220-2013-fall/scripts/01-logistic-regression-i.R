#####################################
# Read in and prepare data
#####################################

gay = read.csv(file = "http://www.tc.umn.edu/~zief0002/Data/Gay-Marriage.csv")
head(gay)           
str(gay)              
summary(gay$marriage) 

#####################################
# Load libraries
#####################################
library(arm)
library(binomTools)
library(ggplot2)
library(gmodels)
library(msm)
library(sm)



#####################################
# EDA
#####################################

CrossTable(gay$marriage, format = "SPSS")

# Examine response 
gay$support = ifelse(gay$marriage == 1, 1, 0) 
CrossTable(gay$support, format = "SPSS")

# Examine predictor
CrossTable(gay$attend, format = "SPSS")

# Examine response conditioned on predictor
CrossTable(gay$support, gay$attend, format = "SPSS")

new = data.frame(
	attend = c(1, 2, 3, 4, 5),
	prop.support = c(0.17, 0.31, 0.38, 0.49, 0.44)
	)

ggplot(data = new, aes(x = attend, y = prop.support)) +
	geom_point() +
	geom_line() +
	theme_bw()


# Reverse code attendance
gay$attend2 = 5 - gay$attend     

new = data.frame(
	attend = 5 - c(1, 2, 3, 4, 5),
	prop.support = c(0.17, 0.31, 0.38, 0.49, 0.44)
	)

ggplot(data = new, aes(x = attend, y = prop.support)) +
	geom_point() +
	geom_line() +
	theme_bw() +
	scale_x_continuous(
		name = "Religious Attendance", 
		breaks = c(0, 1, 2, 3, 4),
		labels = c("Never", "Few times per year", "Once/twice per month", "Almost every week", "Every week")
		) +
	ylab("Proportion supporting gay marriage")	       




#####################################
# Fit model
#####################################

glm.a <- glm(support ~ attend2, data = gay, family = binomial(link = "logit"))
summary(glm.a)


binnedplot(x = fitted(glm.a), y = resid(glm.a))

new = data.frame(
	attend = 5 - c(1, 2, 3, 4, 5),
	prop.support = c(0.17, 0.31, 0.38, 0.49, 0.44),
	logits = log(c(0.17, 0.31, 0.38, 0.49, 0.44) / (1 - c(0.17, 0.31, 0.38, 0.49, 0.44)))
	)

ggplot(data = new, aes(x = attend, y = logits)) +
	geom_point() +
	geom_line() +
	geom_smooth(method = "lm", se = FALSE) +
	theme_bw()


#####################################
# Examine quadratic relationship
#####################################

ggplot(data = new, aes(x = attend, y = logits)) +
	geom_point() +
	geom_line() +
	geom_smooth(method = "lm", se = FALSE, formula = y ~ poly(x, 2)) +
	theme_bw()
	
glm.b <- glm(support ~ attend2 + I(attend2 ^ 2), data = gay, family = binomial(link = "logit"))
summary(glm.b)


out = fortify(glm.a)

ggplot()

#####################################
# Examine other predictors
#####################################

cor(gay[c("support", "attend2", "denom", "educ", "orientation", "gender", "friends")])



#####################################
# Deviance
#####################################

deviance(glm.f)
summary(glm.f)

deviance(glm.r)



#####################################
# Fit age model
#####################################

glm.b = glm(fracture ~ age, data = glow, family = binomial(link = "logit"))
summary(glm.b)




#####################################
# Hosmer–Lemeshow test
#####################################

library(MKmisc)
HLgof.test(fit = fitted(glm.b), obs = glow$fracture)
HLgof.test(fit = fitted(glm.f), obs = glow$fracture)


#####################################
# Examine residuals
#####################################

head(resid(glm.b))
head(resid(glm.b, type = "pearson"))

plot(resid(glm.b))

library(car)
influenceIndexPlot(glm.b, vars = "hat", id.n = 10)
influenceIndexPlot(glm.b, vars = "Cook", id.n = 10) 

influenceIndexPlot(glm.b, vars = c("Cook", "hat"))


influencePlot(glm.b)

glm.c = update(glm.b, subset = -c(169, 460))
summary(glm.c)

AIC(glm.b)
BIC(glm.b)

glm.b = glm(fracture ~ age, data = glow, family = binomial(link = "logit"))


out.b = fortify(glm.b)

ggplot(data = out.b, aes(x = .fitted, y = .stdresid)) +
	geom_point() +
	theme_bw()

library(arm)
binnedplot(x = glm.b$.fitted, y = glm.b$.resid)


ggplot(data = out.b, aes(x = seq_along(.cooksd), y = .cooksd)) +
	geom_bar(stat="identity")









#####################################
# Fit more complex model
#####################################

glm.c = glm(fracture ~ age + momfrac, data = glow, family = binomial(link = "logit"))
summary(glm.c)
anova(glm.c, test = "LRT")
binnedplot(x = fitted(glm.c), y = resid(glm.c))
HLgof.test(fit = fitted(glm.c), obs = glow$fracture)

exp(coef(glm.c))

ggplot(data = glow, aes(
		x = age, y = fracture, 
		group = factor(momfrac), color = factor(momfrac)
		)
	) +
	geom_smooth(method = "glm", family = binomial(link = "logit"), se = FALSE) +
	theme_bw() +
	scale_color_manual(
			name = "Mother had a hip fracture?",
			values = c("#607582", "#95925F"),
			labels = c("No", "Yes")
	        ) +
	xlab("Age") +
	ylab("Probability of a Fracture")

