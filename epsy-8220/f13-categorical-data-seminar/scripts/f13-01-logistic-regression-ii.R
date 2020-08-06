#####################################
# Read in data
#####################################

glow = read.table(file = "http://www.tc.umn.edu/~zief0002/Data/GLOW.txt", header = TRUE)
head(glow)



#####################################
# Load libraries
#####################################
library(ggplot2)
library(sm)



#####################################
# Examine response variable
#####################################

summary(glow$fracture)
table(glow$fracture)           


#####################################
# Examine predictor
#####################################

summary(glow$age)
sm.density(glow$age)



#####################################
# Examine response conditioned on predictor
#####################################

ggplot(data = glow, aes(x = age, y = fracture)) +
	geom_point() +
	theme_bw() +
	xlab("Age") +
	scale_y_continuous(
		name = "Fracture", 
		breaks = c(0, 1),
		labels = c("No", "Yes")
		)

# jittered plot
ggplot(data = glow, aes(x = age, y = fracture)) +
	geom_point(position = position_jitter(width = 0.2, height = 0.05), alpha = 0.2) +
	theme_bw() +
	xlab("Age") +
	scale_y_continuous(
		name = "Fracture", 
		breaks = c(0, 1),
		labels = c("No", "Yes")
		)



#####################################
# Grouped predictor
#####################################

# Create the groups
glow$age2 = cut(glow$age, breaks = c(55, 60, 70, 80, 90), include.lowest = TRUE)

# Examine the grouped predictor
table(glow$age2)

# Condition the response on the grouped predictor
table(glow$fracture, glow$age2)





#####################################
# Plot proportion of subjects having a 
# fracture vs age group
#####################################

dat = data.frame(
	x = c(57.5, 65, 75, 85),
	y = c(0.12, 0.22, 0.30, 0.46)
	) 

ggplot(data = dat, aes(x = x, y = y)) +
	geom_point() +
	geom_line() +
	theme_bw() +
	ylab("Proportion having a fracture") +
	scale_x_continuous(
		name = "Age group", 
		labels = c("[55, 60]", "(60, 70])", "(70, 80]", "(80, 90]")
		)




#####################################
# Fit the linear probability model
#####################################

lm.a = lm(fracture ~ age, data = glow)                                  
summary(lm.a)

out.a = fortify(lm.a)

# Check normality assumption
sm.density(out.a$.stdresid, model = "normal")

# Check other assumptions
ggplot(data = out.a, aes(x = age, y = .stdresid)) +
	geom_point(alpha = 0.2) +
	geom_hline(yintercept = 0) +
	theme_bw() +
	ylab("Studentized Residuals") +
	xlab("Age")



dat = data.frame(
	x = c(55, 65, 75, 85),
	y = c(0.12, 0.22, 0.30, 0.46)
	)







#####################################
# Fit logistic model
#####################################

glm.a <- glm(fracture ~ age, data = glow, family = binomial(link = "logit"))
summary(glm.a)
 


#####################################
# Plot the fitted model
#####################################

plotdata <- data.frame(
	age = seq(from = 55, to = 90, by = 1)
	)

plotdata$fitted = predict(glm.a, newdata = plotdata, type = "response")

ggplot(data = plotdata, aes(x = age, y = fitted)) +
	geom_line(color = "blue") +
	xlab("Age") +
	ylab("Estimated Probability of a Fracture") +
	ylim(0, 1) +
	theme_bw()



#####################################
# Plot the fitted model (Shortcut)
#####################################

ggplot(data = glow, aes(x = age, y = fracture))+
	geom_smooth(method = "glm", family = binomial(link = "logit"), se = TRUE) +
	xlab("Age") +
	ylab("Estimated Probability of a Fracture") +
	ylim(0, 1) +
	theme_bw()





