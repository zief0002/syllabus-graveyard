##################################################
### Load libraries
##################################################

library(tidyverse)
library(lme4)
library(stringr)



###################################################
### Read in and prepare data
###################################################

vocab = read_csv(file = "~/Dropbox/epsy-8282/data/vocab.csv")


# Convert to long data
vocab_long = vocab %>% 
  gather(time, score, t8:t11) %>%
  mutate(
    grade = as.integer(str_replace(time, pattern = "t", replacement = "")) - 8
  ) %>%
  select(id, grade, score, female) %>%
  arrange(id, grade) 

head(vocab_long, 8)



###################################################
### Fit mixed-effects model
###################################################

lmer.1 = lmer(score ~ 1 + grade + (1 + grade | id), data = vocab_long, REML = FALSE)



###################################################
### Obtain level-1 residuals
###################################################

library(broom)

# Augment the model to obtain residuals
out1 = augment(lmer.1)
head(out1)



###################################################
### Obtain REs
###################################################

# Put RE in data frame
r_effects = ranef(lmer.1)$id 

# Change the name of the first column from "(Intercept)" to "Intercept"
names(r_effects)[1] = "Intercept"

# Add ID (row number) to the data frame
r_effects = r_effects %>% mutate(ID = as.integer(row.names(r_effects)))

# Inspect the data frame
head(r_effects)



###################################################
### Normality assumptions
###################################################

library(sm)

# Level-1 residuals
sm.density(out1$.resid, model = "normal", xlab = "Level-1 residuals")

# Random effect of intercept
sm.density(r_effects$Intercept, model = "normal", xlab = "Intercept random-effect")

# Random effect of slope
sm.density(r_effects$grade, model = "normal", xlab = "Linear slope random-effect")


# Find observation(s) with extreme residual
r_effects %>% filter(Intercept > 5)



###################################################
### Scatterplots of level-1 residuals and REs vs ID
###################################################

# Scatterplot of residuals vs. ID
ggplot(data = out1, aes(x = id, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  theme_bw()

# Scatterplot of intercept RE vs. ID
ggplot(data = r_effects, aes(x = ID, y = Intercept)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  theme_bw()

# Scatterplot of slope RE vs. ID
ggplot(data = r_effects, aes(x = ID, y = grade)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  theme_bw()



###################################################
### Use scaled level-1 residuals
###################################################

# Add column of scaled level-1 residuals
out1 = out1 %>%
  mutate( scl_resid = resid(lmer.1, scaled = TRUE))

head(out1)

# Plot of the scaled level-1 residuals vs. ID
ggplot(data = out1, aes(x = id, y = scl_resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  theme_bw()



###################################################
### Use scaled REs
###################################################

# Obtain the scaled REs
r_effects = r_effects %>%
  mutate(
    scl_intercept = Intercept / 1.771334,
    scl_grade = grade / 0.946748
  )

head(r_effects)


ggplot(data = r_effects, aes(x = ID, y = scl_intercept)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  theme_bw()

ggplot(data = r_effects, aes(x = ID, y = scl_grade)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  theme_bw()


# Plot scaled level-1 residuals vs. predictor in level-1 model
ggplot(data = out1, aes(x = grade, y = scl_resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  theme_bw()



###################################################
### Remove observation #36
###################################################

# Omit ID=36
vocab_long_2 = vocab_long %>%
  filter(id != 36)

lmer.2 = lmer(score ~ 1 + grade + (1 + grade | id), data = vocab_long_2, REML = FALSE)

# Level-1 residuals
out2 = augment(lmer.2) %>%
  mutate( scl_resid = resid(lmer.2, scaled = TRUE))

# Random-effects
r_effects = ranef(lmer.2)$id 
names(r_effects)[1] = "Intercept"

# Add ID column and scaled REs
r_effects = r_effects %>% 
  mutate(ID = as.integer(row.names(r_effects))) %>%
  mutate(
    scl_intercept = Intercept / 1.541329,
    scl_grade = grade / 0.953593
  )

head(r_effects)


# Normality
sm.density(out2$.resid, model = "normal", xlab = "Level-1 residuals")
sm.density(r_effects$Intercept, model = "normal", xlab = "Intercept random-effect")
sm.density(r_effects$grade, model = "normal", xlab = "Linear slope random-effect")

# Scatterplot of scaled level-1 residuals vs. ID
ggplot(data = out2, aes(x = id, y = scl_resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  theme_bw() +
  ggtitle("Scaled Level-1 Residuals vs. ID")


# Scatterplot of scaled level-1 residuals vs. level-1 predictor(s)
ggplot(data = out2, aes(x = grade, y = scl_resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  theme_bw() +
  ggtitle("Scaled Level-1 Residuals vs. Grade")


# Scatterplot of scaled intercept RE vs. ID
ggplot(data = r_effects, aes(x = ID, y = scl_intercept)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  theme_bw() +
  ggtitle("Scaled RE of Intercept vs. ID")


# Scatterplot of scaled slope RE vs. ID
ggplot(data = r_effects, aes(x = ID, y = scl_grade)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  theme_bw() +
  ggtitle("Scaled RE of Grade vs. ID")


