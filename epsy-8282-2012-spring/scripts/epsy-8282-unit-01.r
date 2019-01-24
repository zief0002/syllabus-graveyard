# Read in Table 1.2 data
reading <- read.csv(file = "/Users/zief0002/Documents/EPSY-8282/data/table-01-02.csv")

# Fit the mean model for time
mean.model <- lm(read ~ grade, data = reading)
summary(mean.model)

# Fit the individual regression for subject id=1
model <- lm(read ~ grade, data = reading, subset = subid == 1)

# Fit the individual regressions
library(nlme)
fm1 <- lmList(read ~ grade | subid, data = reading)
coef(fm1)

# Variances and correlation between the random effects
var(coef(fm1)[ ,1:2])
cor(coef(fm1))

new <- matrix()
for (i in c(1, 3, 5, 7)){
	model <- lm(read ~ grade, data = reading, subset = subid == i)
	
	summary(model)$coef[1,1]
	summary(model)$coef[2,1]
	}
	
 	