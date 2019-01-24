##################################################

library(tidyverse)
library(ez)
library(lme4)
library(AICcmodavg)

##################################################

rats = readr::read_csv(file = "~/Dropbox/epsy-8282/data/rat_weight.csv")

rats_wide = rats %>% spread(week, weight) %>% arrange(rat)

##################################################

# Check plot of growth trajectories
ggplot(data = rats, aes(x = week, y = weight, group = rat)) +
  geom_line(alpha = 0.4) +
  stat_summary(geom = "line", fun.y = mean, group = 1, lwd =2) +
  theme_bw() +
  xlab("Week") +
  ylab("Weight (in g)")


##################################################

# Fit unconditional means model
lmer.0 = lmer(weight ~ 1 + (1 | rat), data = rats, REML = FALSE)


##################################################

# Fit additional growth models based on plot
lmer.1 = lmer(weight ~ 1 + week + (1 | rat), data = rats, REML = FALSE)
lmer.2 = lmer(weight ~ 1 + poly(week, 2) + (1 | rat), data = rats, REML = FALSE)

# Compute evidence
myAIC = aictab(
  cand.set = list(lmer.0, lmer.1, lmer.2),
  modnames = c("lmer.0", "lmer.1", "lmer.2") 
)

myAIC


# Compute confidence set
confset(
  cand.set = list(lmer.0, lmer.1, lmer.2),
  modnames = c("lmer.0", "lmer.1", "lmer.2"),
  method = "ordinal"
)


##################################################

# Fit growth to saturation
lmer.3 = lmer(weight ~ 1 + poly(week, 3) + (1 | rat), data = rats, REML = FALSE)
lmer.4 = lmer(weight ~ 1 + poly(week, 4) + (1 | rat), data = rats, REML = FALSE)
lmer.5 = lmer(weight ~ 1 + poly(week, 5) + (1 | rat), data = rats, REML = FALSE)


myAIC = aictab(
  cand.set = list(lmer.0, lmer.1, lmer.2, lmer.3, lmer.4, lmer.5),
  modnames = c("lmer.0", "lmer.1", "lmer.2", "lmer.3", "lmer.4", "lmer.5") 
)

myAIC


# Compute confidence set
confset(
  cand.set = list(lmer.0, lmer.1, lmer.2, lmer.3, lmer.4, lmer.5),
  modnames = c("lmer.0", "lmer.1", "lmer.2", "lmer.3", "lmer.4", "lmer.5") ,
  method = "ordinal"
)


##################################################

# Evaluate the REs (Linear growth)
lmer.1.0 = lmer(weight ~ 1 + week + (1 | rat), data = rats, REML = FALSE)
lmer.1.1 = lmer(weight ~ 1 + week + (1 + week | rat), data = rats, REML = FALSE)

# Evaluate the REs (Quadratic growth)
lmer.2.0 = lmer(weight ~ 1 + poly(week, 2) + (1 | rat), data = rats, REML = FALSE)
lmer.2.1 = lmer(weight ~ 1 + poly(week, 2) + (1 + week | rat), data = rats, REML = FALSE)
lmer.2.2 = lmer(weight ~ 1 + poly(week, 2) + (1 + poly(week, 2) | rat), data = rats, REML = FALSE)

myAIC = aictab(
  cand.set = list(lmer.1.0, lmer.1.1, lmer.2.0, lmer.2.1, lmer.2.2),
  modnames = c("Linear - Int", "Linear - Linear", 
               "Quad - Int", "Quad - Linear", "Quad - Quad") 
)

myAIC


# Compute confidence set
confset(
  cand.set = list(lmer.1.0, lmer.1.1, lmer.2.0, lmer.2.1, lmer.2.2),
  modnames = c("Linear - Int", "Linear - Linear", 
               "Quad - Int", "Quad - Linear", "Quad - Quad") ,
  method = "ordinal"
)


##################################################

# Evaluate the effect of Diet (Linear growth; Linear RE)
lmer.1.1 = lmer(weight ~ 1 + week + regimen + (1 + week | rat), data = rats, REML = FALSE)
lmer.1.2 = lmer(weight ~ 1 + week + regimen + week:regimen + (1 + week | rat), data = rats, REML = FALSE)

# Evaluate the effect of Diet (Quadratic growth; Linear RE)
lmer.2.1 = lmer(weight ~ 1 + poly(week, 2) + regimen + (1 + week | rat), data = rats, REML = FALSE)
lmer.2.2 = lmer(weight ~ 1 + poly(week, 2) + regimen + week:regimen + (1 + week | rat), data = rats, REML = FALSE)


# Compute confidence set
confset(
  cand.set = list(lmer.1.1, lmer.1.2, lmer.2.1, lmer.2.2),
  modnames = c("Linear growth - ME of Diet", "Linear growth - Interaction", 
               "Quadratic growth - ME of Diet", "Quadratic growth - Interaction") ,
  method = "ordinal"
)

summary(lmer.1.2)
summary(lmer.2.2)



##################################################

lm.2 = lm(weight ~ 1 + poly(week, 2) + regimen + week:regimen, data = rats)
summary(lm2)

rat2 = model.matrix(lm.2)

solve(t(rat2)%*%rat2)


ncol(rat2)
qr(rat2)

##################################################


###################################################
### Plotting the fitted fixed-effects model
###################################################

plot_data = expand.grid(
  week = c(0, 1, 2, 4, 8, 12),
  regimen = unique(rats$regimen)
) 

plot_data$yhat = predict(lmer.1.2, newdata = plot_data, re.form = NA)
plot_data$yhat2 = predict(lmer.2.1, newdata = plot_data, re.form = NA)
plot_data$yhat3 = predict(lmer.2.2, newdata = plot_data, re.form = NA)

head(plot_data)

ggplot(data = plot_data, aes(x = week, y = yhat, color = regimen)) +
  geom_line(aes(y = yhat2), linetype = "dashed") +
  geom_line(aes(y = yhat2), linetype = "dotted") +
  geom_line() +
  theme_bw() +
  xlab("Week") +
  ylab("Predicted weight") + 
  facet_wrap(~regimen)


##################################################
# Final set of models


lmer.1 = lmer(weight ~ 1 + (1 | rat), data = rats, REML = FALSE)
lmer.2 = lmer(weight ~ 1 + week +  (1 + week | rat), data = rats, REML = FALSE)
lmer.3 = lmer(weight ~ 1 + week + regimen + (1 + week | rat), data = rats, REML = FALSE)
lmer.4 = lmer(weight ~ 1 + week + regimen + week:regimen + (1 + week | rat), data = rats, REML = FALSE)


myAIC = aictab(
  cand.set = list(lmer.1, lmer.2, lmer.3, lmer.4),
  modnames = c("Unconditional means model", 
               "Unconditional linear growth model", 
               "Linear growth model with ME of regimen", 
               "Linear growth model with week x regimen interaction")
)

confset(
  cand.set = list(lmer.1, lmer.2, lmer.3, lmer.4),
  modnames = c("Unconditional means model", 
               "Unconditional linear growth model", 
               "Linear growth model with ME of regimen", 
               "Linear growth model with week x regimen interaction") ,
  method = "ordinal"
)

evidence(myAIC) # COmpare top-ranked model to second-ranked model
