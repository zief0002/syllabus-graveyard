###################################################
### Read in data
###################################################

snap = read.csv("/Users/andrewz/Documents/EPsy-8220/Data/SNAP/SNAP.csv")


library(ggplot2)
library(gmodels)
library(likert)
library(ltm)
library(reshape2)
library(scales)
library(VGAM)



###################################################
### Order factors
###################################################

snap$talk = ordered(snap$talk, levels = c("Never", "A few times", "Often", "All the time"))
str(snap)

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



###################################################
### Examine the three outcomes
###################################################

#library(devtools)
#install_github(repo = 'likert', username = 'jbryer')
library(likert)

outcomes = likert(snap[ , c("talk", "fruits", "vegetables")])

outcomes
summary(outcomes)

########
library(ggplot2)
library(scales)
########

ggplot(data = snap, aes(x = talk, y = (..count..)/sum(..count..), fill = talk)) +
	geom_bar(color = "black", show_guide = FALSE) +
	theme_bw() +
	scale_y_continuous(labels = percent, name = "Percent") +
	xlab("I talk to my family about foods I learn at school.")

### Stack
ggplot(data = snap, aes(x = factor(1), y = (..count..)/sum(..count..), fill = talk)) +
	geom_bar(color = "black", show_guide = FALSE) +
	theme_bw() +
	scale_y_continuous(labels = percent, name = "Percent") +
	ggtitle("I talk to my family about foods I learn at school.") +
	xlab("") +
	coord_flip()



###################################################
### Marginal cumulative frequencies and probabilities
###################################################

#Cell probabilities
new = data.frame(
	prop.table(table(snap$talk))
	)
names(new) = c("response", "prop")


#Cumulative probabilities
new$cumulative = cumsum(prop.table(table(snap$talk)))

# 1 - cumulative
new$minus_cumulative = 1 - cumsum(prop.table(table(snap$talk)))

#Odds
new$odds = cumsum(prop.table(table(snap$talk))) / (1 - cumsum(prop.table(table(snap$talk))))

#Logits
new$logits = log(cumsum(prop.table(table(snap$talk))) / (1 - cumsum(prop.table(table(snap$talk)))))


# Cumulative logit model
library(VGAM)

clm.1 = vglm(talk ~ 1, data = snap, family = cumulative(parallel = TRUE))
summary(clm.1)

#odds
exp(coef(clm.1))

# Model probabilities
predict(clm.1, newdata = data.frame(1), type = "response")



###################################################
### Condition on treatment
###################################################

# Create factor
snap$treatment2 = factor(snap$treatment, labels = c("Control", "Treatment"))

x = prop.table(table(snap$talk, snap$treatment2), margin = 2)

new = data.frame(
	apply(x, 2, cumsum)
	)
new = melt(new)
names(new) = c("treatment", "prop")
new$response = 
new$response = ordered(row.names(x), levels = c("Never", "A few times", "Often", "All the time"))

#Bar chart
ggplot(data = new, aes(x = treatment, y = prop, fill = response)) +
	geom_bar(stat = "identity", position="dodge", color = "black", show_guide = FALSE) +
	theme_bw() +
	scale_y_continuous(labels = percent, name = "Percent") +
	xlab("I talk to my family about foods I learn at school.")

### Stack
ggplot(data = new, aes(x = treatment, y = prop, fill = response)) +
	geom_bar(stat = "identity", position="fill", color = "black", show_guide = FALSE) +
	theme_bw() +
	scale_y_continuous(labels = percent, name = "Percent") +
	ggtitle("I talk to my family about foods I learn at school.") +
	xlab("") +
	coord_flip()


outcomes2 = likert(snap[ , c("talk", "fruits", "vegetables")], grouping = snap$treatment)
outcomes2
summary(outcomes2)






###################################################
### Cross-classified tables
###################################################

library(gmodels)
CrossTable(snap$treatment, snap$talk, format = "SPSS")




###################################################
### Correlations
###################################################

snap$talk2 = as.numeric(snap$talk)
cor(snap[c("talk2", "treatment")])

library(ltm)
rcor.test(snap[c("talk2", "treatment")])



###################################################
### Plot of the relationship
###################################################

# Prepare data
library(reshape2)
new = melt(dcast(snap, talk ~ treatment, length))
names(new) = c("Talk", "Treatment", "Freq")

# Scatterplot
ggplot(data = new, aes(x = Treatment, y = Talk, size = Freq)) +
	geom_point() +
	theme_bw() +
	scale_x_discrete(name = "", labels = c("Control", "Treatment")) +
	ylab("I talk to my family about foods I learn at school.") +
	guides(size = FALSE)


#Mosaicplot
library(vcd)
mosaic(Treatment ~ Talk, data = new, highlighting_fill = c("blue", "red"))

mosaic(Treatment ~ 1, data = new)




###################################################
### Cumulative logit model: Treatment (DUMMY)
###################################################

clm.2 = vglm(talk ~ treatment, data = snap, family = cumulative(parallel = TRUE))
summary(clm.2)

# Re-parameterize the model
clm.2 = vglm(talk ~ treatment, data = snap, family = cumulative(parallel = TRUE, reverse = TRUE))
summary(clm.2)

exp(coef(clm.2))


# Plot the predicted probabilities
new = data.frame(
	treatment = c(0, 1)
	)

probs = predict(clm.2, newdata = new, type = "response")
new = cbind(new, probs)
new = melt(new, id.vars = "treatment")

names(new)[2] = "response"
names(new)[3] = "probability"

new$response = ordered(new$response, levels = c("Never", "A few times", "Often", "All the time"))
new$treatment = factor(new$treatment, labels = c("Control", "Treatment"))

ggplot(data = new, aes(x = treatment, y = probability, fill = response)) +
	geom_bar(stat = "identity") +
	geom_bar(stat = "identity", color = "black", show_guide = FALSE) +
	theme_bw() +
	xlab("") +
	ylab("Probability") +
	scale_fill_brewer(type = "seq", palette = 7) +
	guides(fill = guide_legend(title = ""))


pdf(file="~/Desktop/Keynote-05.pdf", width = 8, height = 6)
dev.off()



###################################################
### Relationship with BMI
###################################################

# Compute point-polyserial correlation
cor(snap[c("talk2", "bmi")])
rcor.test(snap[c("talk2", "bmi")]) 

# Fit cumulative logit model
clm.3 = vglm(talk ~ bmi, data = snap, family = cumulative(parallel = TRUE, reverse = TRUE))
summary(clm.3)

exp(coef(clm.3))

# 1 unit change in BMI
exp(0.413084 + 0.021682 * 1)
exp(-0.908074 + 0.021682 * 1)
exp(-1.734996 + 0.021682 * 1)

# 5 unit change in BMI
exp(0.413084 + 0.021682 * 5)
exp(-0.908074 + 0.021682 * 5)
exp(-1.734996 + 0.021682 * 5)


###################################################
### Plot of probability of response category versus BMI
###################################################

new = data.frame(
	bmi = 12:38
	)

probs = predict(clm.3, newdata = new, type = "response")
new = cbind(new, probs)
new = melt(new, id.vars = "bmi")

names(new)[2] = "response"
names(new)[3] = "probability"

ggplot(data = new, aes(x = bmi, y = probability, color = response)) +
	geom_line() +
	theme_bw() +
	xlab("BMI") +
	ylab("Probability") +
	scale_color_brewer(palette = "Set1") +
	guides(color = guide_legend(title = ""))



###################################################
### Stacked barplot of probability of response category 
### for two prototypical values of BMI
###################################################

new = data.frame(
	bmi = c(10, 15, 20, 30)
	)

probs = predict(clm.3, newdata = new, type = "response")
new = cbind(new, probs)
new = melt(new, id.vars = "bmi")

names(new)[2] = "response"
names(new)[3] = "probability"

new$response = ordered(new$response, levels = c("Never", "A few times", "Often", "All the time"))
new$bmi = ordered(new$bmi, labels = c("Underweight", "Normal", "Overweight", "Obese"))

ggplot(data = new, aes(x = bmi, y = probability, fill = response)) +
	geom_bar(stat = "identity") +
	geom_bar(stat = "identity", color = "black", show_guide = FALSE) +
	theme_bw() +
	xlab("") +
	ylab("Probability") +
	scale_fill_brewer(type = "seq", palette = 7) +
	guides(fill = guide_legend(title = ""))



###################################################
### Check for proportional odds assumption
###################################################

clm.2.po = vglm(talk ~ bmi, data = snap, family = cumulative(parallel = TRUE, reverse = TRUE))
clm.2.npo = vglm(talk ~ bmi, data = snap, family = cumulative(parallel = FALSE, reverse = TRUE))



clm.2.po = vglm(talk ~ treatment, data = snap, family = cumulative(parallel = TRUE, reverse = TRUE))
clm.2.npo = vglm(talk ~ treatment, data = snap, family = cumulative(parallel = FALSE, reverse = TRUE))

#summary(clm.2.po)
#Residual deviance: 921.9958 on 1019 degrees of freedom

#summary(clm.2.npo)
#Residual deviance: 920.5138 on 1017 degrees of freedom

#921.9958 - 920.5138 = 1.482
#1019 - 1017 = 2

pchisq(1.482, df = 2, lower.tail = FALSE)

lrtest(clm.2.npo, clm.2.po)

# check for proportional odds assumption in BMI
clm.3.po = vglm(talk ~ bmi, data = snap, family = cumulative(parallel = TRUE, reverse = TRUE))
clm.3.npo = vglm(talk ~ bmi, data = snap, family = cumulative(parallel = FALSE, reverse = TRUE))

# Parallel: 921.8388 on 1019 degrees of freedom
# Not parallel: 921.2295 on 1017 degrees of freedom

lrtest(clm.3.npo, clm.3.po)



###################################################
### Sex as a predictor
###################################################

clm.4.po = vglm(talk ~ sex, data = snap, family = cumulative(parallel = TRUE, reverse = TRUE))
clm.4.npo = vglm(talk ~ sex, data = snap, family = cumulative(parallel = FALSE, reverse = TRUE))

lrtest(clm.4.npo, clm.4.po)

# Fit cumulative logit model with BMI, sex, and treatment
clm.5 = vglm(talk ~ bmi + sex + treatment, data = snap, family = cumulative(parallel = TRUE, reverse = TRUE))
summary(clm.5)


clm.6 = vglm(talk ~ sex + treatment, data = snap, family = cumulative(parallel = TRUE, reverse = TRUE))
summary(clm.6)




snap$binary = ifelse(snap$talk == "Never" | snap$talk == "A few times", 0, 1)
snap$binary2 = ifelse(snap$talk <= 2, 0 , 1)


glm.1 = glm(binary ~ bmi + treatment + sex, data = snap, family = binomial(link = "logit"))
summary(glm.1)


snap$talk3 = ifelse(snap$talk == "Never", 0 ,
	ifelse(snap$talk == "A few times", 2,
		ifelse(snap$talk == "Often", 7, 10)
		)
	)

ggplot(data = snap, aes(x = bmi, y = talk2)) +
	geom_jitter() +
	geom_smooth(se = FALSE)


