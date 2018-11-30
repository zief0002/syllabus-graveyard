###################################################
### Read in data
###################################################

snap = read.csv("/Users/andrewz/Documents/EPsy-8220/Data/SNAP/SNAP.csv")



###################################################
### Order factors
###################################################

snap$talk = ordered(snap$talk, levels = c("Never", "A few times", "Often", "All the time"))

snap$vegetables = factor(snap$vegetables, 
	levels = c(0, 1, 2, 3),
	labels = c("Never", "A few times", "Often", "All the time"),
	ordered = TRUE
	)

snap$fruits = factor(snap$fruits, 
	levels = c(0, 1, 2, 3),
	labels = c("Never", "A few times", "Often", "All the time"),
	ordered = TRUE
	)

snap$treatment2 = factor(snap$treatment, labels = c("Control", "Treatment"))




###################################################
### Examine the vegetable outcome
###################################################

library(ggplot2)
library(scales)

ggplot(data = snap, aes(x = vegetables, y = (..count..)/sum(..count..), fill = vegetables)) +
	geom_bar(color = "black", show_guide = FALSE) +
	theme_bw() +
	scale_y_continuous(labels = percent, name = "Percent") +
	xlab("I like to try new vegetables.")

library(gmodels)
CrossTable(snap$vegetables)



###################################################
### Fit separate fruit models
################################################### 

# 1 vs. 2, 3, 4
snap$beyond = ifelse(snap$vegetables == "Never", 0, 1)
mod1 = glm(beyond ~ 1, data = snap, family = binomial(link = "logit"))
summary(mod1)


# 2 vs. 3, 4
snap2 = snap[snap$vegetables >= "A few times", ]
snap2$beyond = ifelse(snap2$vegetables == "A few times", 0, 1)
mod2 = glm(beyond ~ 1, data = snap2, family = binomial(link = "logit"))
summary(mod2)


# 3 vs. 4
snap3 = snap[snap$vegetables >= "Often", ]
snap3$beyond = ifelse(snap3$vegetables == "Often", 0, 1)
mod3 = glm(beyond ~ 1, data = snap3, family = binomial(link = "logit"))
summary(mod3)


###################################################
### Fit unconditional CR model
################################################### 

crm.1 = vglm(vegetables ~ 1, data = snap, family = cratio(parallel = TRUE))
summary(crm.1)

exp(coef(crm.1))



###################################################
### Fit conditional CR model
################################################### 

crm.2 = vglm(vegetables ~ treatment, data = snap, family = cratio(parallel = TRUE))

#crm.3 = vglm(vegetables ~ treatment, data = snap, family = cratio(parallel = FALSE))
#lrtest(crm.3, crm.2)

summary(crm.2)
exp(coef(crm.2))



###################################################
### Fit unconditional AC model
###################################################

library(VGAM)

acm.1 = vglm(vegetables ~ 1, data = snap, family = acat(parallel = TRUE))
summary(acm.1)
exp(coef(acm.1))




###################################################
### Fit conditional AC model (treatment)
################################################### 

ggplot(data = snap, aes(x = vegetables, y = (..count..)/sum(..count..), fill = vegetables)) +
	geom_bar(color = "black", show_guide = FALSE) +
	theme_bw() +
	scale_y_continuous(labels = percent, name = "Percent") +
	xlab("I like to try new vegetables.") +
	facet_wrap(~treatment2)

acm.2 = vglm(vegetables ~ treatment, data = snap, family = acat(parallel = TRUE))
summary(acm.2)
exp(coef(acm.2))

acm.3 = vglm(vegetables ~ treatment, data = snap, family = acat(parallel = FALSE))
lrtest(acm.3, acm.2)

###################################################
### Fit conditional AC model (bmi)
################################################### 

acm.3 = vglm(vegetables ~ bmi, data = snap, family = acat(parallel = TRUE))
summary(acm.3)
exp(coef(acm.3))

acm.4 = vglm(vegetables ~ bmi, data = snap, family = acat(parallel = FALSE))
lrtest(acm.4,)

###################################################
### Check for proportional odds assumption
###################################################

acm.4 = vglm(vegetables ~ bmi, data = snap, family = acat(parallel = FALSE))

lrtest(acm.4, acm.3)



###################################################
### Fit conditional AC model (bmi)
################################################### 

# Visualize results
new = data.frame(
	bmi = 12:38
	)
probs = predict(acm.3, newdata = new)
new = cbind(new, probs)

library(reshape2)
new = melt(new, id.vars = "bmi")
levels(new$variable) = c("Odds 2 vs. 1", "Odds 3 vs. 2", "Odds 4 vs. 3")

ggplot(data = new, aes(x = bmi, y = exp(value), color = variable)) +
	geom_line() +
	facet_wrap(~variable, nrow = 1) +
	theme_bw() +
	xlab("BMI") +
	ylab("Odds") +
	scale_color_brewer(palette = "Set1") +
	guides(color = guide_legend(title = ""))








