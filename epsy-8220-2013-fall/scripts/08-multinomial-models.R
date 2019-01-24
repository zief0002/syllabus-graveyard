###################################################
### Read in data
###################################################

wine = read.csv(file = "http://www.tc.umn.edu/~zief0002/Data/Wine-Classification.csv")
#wine = read.csv(file = "/Users/andrewz/Documents/EPsy-8220/Data/Wine-Classification/Wine-Classification.csv")

## Create categorical variable for plotting
wine$Type = factor(wine$Type)
levels(wine$Type) = c("I", "II", "III")

## Reshape the data so it is useable for ggplot2
library(reshape2)
wine2 = melt(wine, id.vars = "Type")

head(wine2)
names(wine2) = c("Type", "Property", "Value")


## Plot of response by each predictor
library(ggplot2)
ggplot(data = wine2, aes(x = Type, y = Value, color = Type)) + 
	geom_point() +
	stat_summary(aes(group = 1), color = "black", fun.y = mean, geom = "line") +
	stat_summary(fun.y = mean, geom = "point", size = 4, color = "black") +
	facet_wrap(~Property, scales = "free") +
	scale_color_brewer(palette = "Set1") +
	theme_bw()


## Indicator variables
wine$Type1 = ifelse(wine$Type == "I", 1, 0)
wine$Type2 = ifelse(wine$Type == "II", 1, 0)


###################################################
### Fit Multinomial Model
###################################################

library(VGAM)
vglm.1 = vglm(Type ~ Color, data = wine, family = multinomial)
summary(vglm.1)	



###################################################
### Compute Predicted Values for Different Color Values
###################################################

fit = data.frame(
	Color = seq(1, 13, by = 0.01)
	)



###################################################
### Plot of Logits
###################################################

logits = data.frame(predict(vglm.1, newdata = fit, type = "terms"))
names(logits) = c("I", "II")

longdata = melt(logits)
longdata = cbind(fit, longdata)
names(longdata) = c("Color", "Type", "Logit")

ggplot(data = longdata, aes(x = Color, y = Logit, color = Type)) +
	geom_line() +
	scale_x_discrete(name = "Color Intensity") +
	scale_color_brewer(palette = "Set1", name = "Type") +
	ylab("Logits") +
	theme_bw()	


###################################################
### Plot of Odds
###################################################

longdata$Odds = exp(longdata$Logit)

ggplot(data = longdata, aes(x = Color, y = Odds, color = Type)) +
	geom_line() +
	scale_x_discrete(name = "Color Intensity") +
	scale_color_brewer(palette = "Set1", name = "Type") +
	ylab("Odds") +
	theme_bw()


 
###################################################
### Convert Logits to Probabilities
###################################################
exp(3.79 - 0.57 * 2)
exp(12.47 - 2.63 * 2)

## Type I
14 / (1 + 1353 + 14)

## Type II
1353 / (1 + 1353 + 14)

## Type III (reference)
1 / (1 + 1353 + 14)


###################################################
### Convert Logits to Probabilities Automatically
###################################################

## Create new data frame
new = data.frame(Color = 2)

## Predict probabilities
predict(vglm.1, type = "response", newdata = new)


###################################################
### Plot of Probabilities for Several Color Values
###################################################

## Create probabilities at each color intensity
probdata = data.frame(predict(vglm.1, newdata = fit, type = "response"))
longdata = melt(probdata)
longdata = cbind(fit, longdata)
names(longdata) = c("Color", "Type", "Probability")


## Plot probabilities vs. color values
ggplot(data = longdata, aes(x = Color, y = Probability, color = Type)) +
	geom_line(name = "Color Intensity") +
	scale_x_discrete() +
	scale_color_brewer(palette = "Set1", name = "Type") +
	ylab("Probability") +
	theme_bw() 


###################################################
### Fit Full Model
###################################################

vglm.2 = vglm(Type ~ Color + Hue + Alcohol, data = wine, family = multinomial)
summary(vglm.2)	


###################################################
### Compute Predicted Values for Different Color 
### Hues and Alcohol Values
###################################################

fit2 = expand.grid(
	Color = 1:13,
	Hue = mean(wine$Hue),
	Alcohol = c(11, 15)
	)

## Create probabilities at each color intensity
plotdata = data.frame(predict(vglm.2, newdata = fit2, type = "response"))
plotdata = stack(plotdata, select =c(I, II, III))
plotdata = cbind(fit2, plotdata)
plotdata$Alcohol = factor(plotdata$Alcohol)

## Plot probabilities vs. color values
ggplot(data = plotdata, aes(x = Color, y = values, color = ind)) +
	geom_point(size = 4) +
	geom_line() +
	scale_x_discrete() +
	scale_color_brewer(palette = "Set1", name = "Type") +
	xlab("Color Intensity") +
	ylab("Probability") +
	facet_wrap(~Alcohol) +
	theme_bw()  		


###################################################
### Create Data Frame of Table Frequencies
###################################################

## Enter table variables and categories
tab = data.frame(expand.grid(
	Contraception = c("Sterilization", "Other", "None"),
	Age = c(17, 22, 27, 32, 37, 42, 47)
	))
tab$Age2 = factor(tab$Age)

## Enter frequencies	
tab$Freq = c(
	  3,  61, 232, 
	 80, 137, 400, 
	216, 131, 301, 
	268,  76, 203, 
	197,  50, 188, 
	150,  24, 164, 
	 91,  10, 183
	)


###################################################
### Exploratory Analysis
###################################################

## Create contingency table
my.tab = xtabs(Freq ~ Age2 + Contraception, data = tab)

## Examine margins
apply(my.tab, 1, sum)
apply(my.tab, 2, sum)

## Compute empirical logits
logit1 = log(my.tab[ , 1] / my.tab[ , 3])
logit2 = log(my.tab[ , 2] / my.tab[ , 3])

## Create data frame
emp = data.frame(
	Age = c(17, 22, 27, 32, 37, 42, 47),
	Logit1 = logit1,
	Logit2 = logit2
	)

## Plot the empirical logits versus age
plot(Logit1 ~ Age, data = emp, pch = "S", type = "b")
points(Logit2 ~ Age, data = emp, pch = "O", type = "b")


###################################################
### Fit Model Using Frequencies as Weights
###################################################

model.int = vglm(Contraception ~ 1, data = tab, weights = Freq, family = multinomial)
model.linear = vglm(Contraception ~ Age, data = tab, weights = Freq, family = multinomial)
model.quad = vglm(Contraception ~ Age + I(Age ^ 2), data = tab, weights = Freq, family = multinomial)

lrtest(model.quad, model.linear, model.int)

## Obtain deviance
deviance(model.quad)
deviance(model.linear)
deviance(model.int)

## McFadden's pseudo R-squared
(deviance(model.int) - deviance(model.linear))/deviance(model.int)
(deviance(model.int) - deviance(model.quad))/deviance(model.int)



###################################################
### Get the parameter estimates from quadratic model
###################################################

coef(model.quad)


###################################################
### Final Plot
###################################################

## Create data frame to predict from
a =  data.frame(
	Age = c(17, 22, 27, 32, 37, 42, 47)
	)

## Compute probabilities from model
plotdata = data.frame(predict(model.quad, type = "response", newdata = a))

## Get data frame ready for ggplot
plotdata = stack(plotdata, select = c(Sterilization, Other, None))
plotdata = cbind(a, plotdata)

## Plot probabilities 
ggplot(data = plotdata, aes(x = Age, y = values, col = ind)) +
	geom_point(size = 4) +
	geom_line() +
	scale_color_brewer(palette = "Set1", name = "Type") +
	xlab("Age") +
	ylab("Probability") +
	theme_bw()  		


