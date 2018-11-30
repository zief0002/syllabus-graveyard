##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(dplyr)
library(ez)
library(ggplot2)
library(readr)




###################################################
### Read in data
###################################################

backpain = read_csv(file = "~/Dropbox/epsy-8282/data/backpain.csv")
head(backpain)



###################################################
### Prepare data
###################################################

backpain_long = backpain %>% 
  gather(time, pain, t1:t4) %>%
  arrange(id, time) %>%
  mutate(treatment = factor(treatment, levels = c(0, 1), labels = c("Control", "Treatment")))

backpain_long



###################################################
### Plot and describe data
###################################################

ggplot(data = backpain_long, aes(x = time, y = pain, group = id)) + 
  geom_line(alpha = 0.4) +
  geom_point(alpha = 0.4) +
  stat_summary(group = 1, fun.y = mean, geom = "line", color = "blue", lwd = 1.5) +
  stat_summary(group = 1, fun.y = mean, geom = "point", color = "blue", size = 4) +
  theme_bw() +
  facet_wrap(~treatment) + 
  scale_x_discrete(name = "Time") + 
  scale_y_continuous(
    name = "Self-Reported pain", 
    breaks = seq(from = 0, to = 100, by = 10)
  )


backpain_long %>%
  na.omit() %>%
  group_by(time, treatment) %>%
  summarize(
    M = mean(pain), SD = sd(pain), N = n()
    ) %>%
  arrange(treatment, time)


backpain %>% 
  filter(treatment == 0) %>%
  select(t1:t4) %>%
  correlate() %>%
  shave() %>%
  fashion(decimals = 3)

backpain %>% 
  filter(treatment == 1) %>%
  select(t1:t4) %>%
  correlate() %>%
  shave() %>%
  fashion(decimals = 3)



###################################################
### Analysis of mean pain
###################################################

backpain_2 = backpain_long %>%
  na.omit() %>%
  group_by(id) %>%
  summarize(
    mean_pain = mean(pain)
   ) %>%
  mutate(treatment = factor(backpain$treatment, levels = c(0, 1), labels = c("Control", "Treatment")))

backpain_2


ggplot(data = backpain_2, aes(x = treatment, y = mean_pain)) +
  geom_point(alpha = 0.4) +
  stat_summary(fun.y = mean, geom = "point", color = "blue", size = 4) +
  theme_bw() +
  xlab("Time") +
  ylab("Mean self-Reported pain")

t.test(mean_pain ~ treatment, data = backpain_2, var.equal = TRUE)
t.test(mean_pain ~ treatment, data = backpain_2)



###################################################
### Analysis of wave 4 pain scores
###################################################

backpain_3 = backpain %>%
  select(id, t4, treatment) %>%
  mutate(treatment = factor(treatment, levels = c(0, 1), labels = c("Control", "Treatment")))

  
backpain_3


ggplot(data = backpain_3, aes(x = treatment, y = t4)) +
  geom_point(alpha = 0.4) +
  stat_summary(fun.y = mean, geom = "point", color = "blue", size = 4) +
  theme_bw() +
  xlab("Time") +
  ylab("Self-Reported pain (wave 4)")

t.test(t4 ~ treatment, data = backpain_3)



###################################################
### Analysis of slopes
###################################################

backpain_4 = backpain_long %>%
  mutate(wave = as.numeric(as.factor(time))) %>%
  select(id, treatment, wave, pain)

backpain_4

ggplot(data = backpain_4, aes(x = wave, y = pain, group = id)) + 
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Measurement wave") +
  scale_y_continuous(
    name = "Self-Reported pain", 
    breaks = seq(from = 0, to = 100, by = 10)
  ) +
  facet_wrap(~id)

backpain_5 = backpain_4 %>%
  group_by(id) %>%
  do(tidy(lm(pain ~ 1 + wave, data=.))) %>%
  filter(term == "wave") %>%
  select(id, Slope = estimate)

backpain_5 = data.frame(backpain_5) %>%
  mutate(treatment = factor(backpain$treatment, levels = c(0, 1), labels = c("Control", "Treatment")))

backpain_5


ggplot(data = backpain_5, aes(x = treatment, y = Slope)) +
  geom_point(alpha = 0.4) +
  stat_summary(fun.y = mean, geom = "point", color = "blue", size = 4) +
  theme_bw() +
  xlab("") +
  ylab("Fitted regression slope")


t.test(Slope ~ treatment, data = backpain_5, var.equal = TRUE)


# Is the population mean slope negative (pain improves) under one or both treatments?

control = backpain_5 %>%
  filter(treatment == "Control")

treatment = backpain_5 %>%
  filter(treatment == "Treatment")

t.test(control$Slope, alternative = "less", mu = 0)
t.test(treatment$Slope, alternative = "less", mu = 0)



###################################################
### RM-ANOVA
###################################################

bp = backpain %>% 
  na.omit() %>%
  gather(time, pain, t1:t4) %>%
  mutate(treatment = factor(treatment, levels = c(0, 1), labels = c("Control", "Treatment")))

  
# Fit RM-ANOVA
ezANOVA(data = bp, dv = pain, wid = factor(id),
        within = time, return_aov = TRUE)


# Fit RM-ANOVA with both within- and between-subjects factors
ezANOVA(data = bp, dv = pain, wid = factor(id), 
        within = time, between = treatment, return_aov = TRUE)


ggplot(data = bp, aes(x = time, y = pain, color = treatment)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line", group = 1) +
  theme_bw() +
  xlab("Time") +
  ylab("Mean pain") +
  facet_wrap(~treatment) +
  ggsci::scale_color_d3() +
  guides(color = FALSE)



###################################################
### MANOVA
###################################################

outcome = cbind(backpain$t1, backpain$t2, backpain$t3, backpain$t4)      
mod.mlm = lm(outcome ~ 1)

idata = data.frame(time = c("t1", "t2", "t3", "t4"))
manova1 = Anova(mod.mlm, idata = idata, idesign = ~time, type = 3)
summary(manova1)


# INclude treatment as a between-subject factor
mod.mlm2 = lm(cbind(t1, t2, t3, t4) ~ 1 + treatment, data = backpain)
manova2 = Anova(mod.mlm2, idata = idata, idesign = ~time, type = 3)
summary(manova2)




###########################################################
#                        					                        #
#				            HORMONE THERAPY DATA                  #
#  									                                      #
###########################################################

hormone = read_csv(file = "~/Dropbox/epsy-8282/data/hormone.csv")
head(hormone)

hormone_long = hormone %>% 
  gather(time, hdi, t1:t4) %>%
  arrange(id, time) %>%
  mutate(condition = factor(condition))

hormone_long

ggplot(data = hormone_long, aes(x = time, y = hdi, group = id)) + 
  geom_line(alpha = 0.4) +
  geom_point(alpha = 0.4) +
  stat_summary(group = 1, fun.y = mean, geom = "line", color = "blue", lwd = 1.5) +
  stat_summary(group = 1, fun.y = mean, geom = "point", color = "blue", size = 4) +
  theme_bw() +
  facet_wrap(~condition) + 
  xlab("Time") + 
  ylab("Hamilton Depression Inventory score")


# Fit RM-ANOVA with both within- and between-subjects factors
ezANOVA(data = hormone_long, dv = hdi, wid = factor(id), 
        within = time, between = condition, return_aov = TRUE)

# MANOVA
mod.mlm3 = lm(cbind(t1, t2, t3, t4) ~ 1 + condition, data = hormone)
idata = data.frame(time = c("t1", "t2", "t3", "t4"))
manova3 = Anova(mod.mlm3, idata = idata, idesign = ~time, type = 3)
summary(manova3)

