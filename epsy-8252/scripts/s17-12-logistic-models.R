# Read in player-level and team-level data
library(readr)
grad = read_csv(file = "~/Google Drive/Documents/EPsy-8252/data/graduation.csv")
head(grad)

# Load other libraries
library(AICcmodavg)
library(broom)
library(dplyr)
library(ggplot2)
library(sm)

grad %>% group_by(degree) %>% summarize(Count = n(), Prop = n() / nrow(grad))

sm.density(grad$act, xlab = "ACT score")
grad %>% summarize( M = mean(act), SD = sd(act) )

ggplot(data = grad, aes(x = act, y = degree)) +
  geom_point() +
  theme_bw() +
  xlab("ACT score") +
  ylab("Graduated") +
  geom_smooth(method = "lm", se = FALSE)

ggplot(data = grad, aes(x = act, y = jitter(degree))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("ACT score") +
  ylab("Graduated")

cor(grad[ , c("degree", "act")])

lm.1 = lm(degree ~ 1 + act, data = grad)
summary(lm.1)

out = augment(lm.1)
#head(out)

# Examine normality assumption
sm.density(out$.std.resid, xlab = "Standardized residuals")

# Examine linearity and homoskedasticity
ggplot(data = out, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Standardized residuals")


graduates = grad %>% 
  group_by(act, degree) %>% 
  summarize( N = n() ) %>% 
  mutate( Prop = N / sum (N) ) %>%
  filter(degree == 1)

# View data
graduates

# Plot proportions
ggplot(data = graduates, aes(x = act, y = Prop)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("ACT score") +
  ylab("Proportion of graduates")


glm.1 = glm(degree ~ 1 + act, data = grad, family = binomial(link = "logit"))
summary(glm.1)


exp(coef(glm.1))


plotData = expand.grid(
  act = seq(from = 10, to = 36, by = 1)
)

plotData$pi_hat = predict(glm.1, newdata = plotData, type = "response")

ggplot(data = plotData, aes(x = act, y = pi_hat)) +
  geom_line() +
  theme_bw() +
  xlab("ACT score") +
  ylab("Predicted probability of graduating") +
  ylim(0, 1)


glm.0 = glm(degree ~ 1, data = grad, family = binomial(link = "logit"))
deviance(glm.0)
deviance(glm.1)


anova(glm.0, glm.1, test = "LRT")


myAIC = aictab(
  cand.set = list(glm.0, glm.1),
  modnames = c("Intercept-only", "ACT score")
)

myAIC


glm.2 = glm(degree ~ 1 + act + firstgen, data = grad, family = binomial(link = "logit"))
summary(glm.2)


exp(coef(glm.2))


plotData = expand.grid(
  act = seq(from = 10, to = 36, by = 1),
  firstgen = c(0, 1)
)

plotData$pi_hat = predict(glm.2, newdata = plotData, type = "response")

plotData$firstgen = factor(plotData$firstgen, levels = c(0, 1), labels = c("Non First Generation Students", "First Geeration Students"))

ggplot(data = plotData, aes(x = act, y = pi_hat, color = firstgen)) +
  geom_line() +
  theme_bw() +
  xlab("ACT score") +
  ylab("Predicted probability of graduating") +
  ylim(0, 1) +
  scale_color_brewer(name = "", palette = "Set1")


