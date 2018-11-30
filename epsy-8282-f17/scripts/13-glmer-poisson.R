library(tidyverse)
library(corrr)
library(gridExtra)
library(lme4)
library(AICcmodavg)


rapi = read_csv(file.choose()) 
head(rapi)

length(unique(rapi$id))
nrow(rapi)
summary(rapi)

rapi$Lrapi = log(rapi$rapi + 1)

lmer.1 = lmer(Lrapi ~ 1 + time + (1 + time | id), data = rapi, REML = FALSE) 
out1 = broom::augment(lmer.1)
ggplot(data = out1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  theme_bw()

sm.density(out1$.resid, model = "normal")


glmer.1 = glmer(rapi ~ 1 + time + (1 | id), data = rapi, family = poisson(link = "log")) 
summary(glmer.1)

my_data = data.frame(
  time = c(0, 6, 12, 18, 24)
)

my_data$yhat = exp(1.56355 - 0.03130*my_data$time)

ggplot(data = my_data, aes(x = time, y = yhat)) +
  geom_line()
