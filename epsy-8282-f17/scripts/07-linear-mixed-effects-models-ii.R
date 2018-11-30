##################################################
### Load libraries
##################################################

library(tidyverse)
library(lme4)
library(stringr)

# library(broom)
# library(corrr)
# library(dplyr)
# library(ez)
# library(ggplot2)
# library(readr)

# library(tidyr)





###################################################
### Read in data
###################################################

vocab = read_csv(file = "~/Dropbox/epsy-8282/data/vocab.csv")
head(vocab)



###################################################
### Prepare data
###################################################

vocab_long = vocab %>% 
  gather(time, score, t8:t11) %>%
  mutate(
    grade = as.integer(str_replace(time, pattern = "t", replacement = "")) - 8
    ) %>%
  arrange(id, grade) 

vocab_long



###################################################
### Fit unconditional mixed-effects models
###################################################

# Fit unconditional means model
lmer.0 = lmer(score ~ 1 + (1 | id), data = vocab_long, REML = FALSE)
summary(lmer.0)

# Fit unconditional growth model
lmer.1 = lmer(score ~ 1 + grade + (1 | id), data = vocab_long, REML = FALSE)
summary(lmer.1)



###################################################
### Log-likelihoods and deviance
###################################################

logLik(lmer.0)
logLik(lmer.1)

-2 * logLik(lmer.0)[1]
-2 * logLik(lmer.1)[1]



###################################################
### Likelihood ratio test (LRT)
###################################################

anova(lmer.0, lmer.1)



###################################################
### Fit the mixed-effects model: fixed- and random-effects of intercept and grade
###################################################

lmer.2 = lmer(score ~ 1 + grade + I(grade^2) + (1 | id), data = vocab_long, REML = FALSE)
logLik(lmer.2)
-2 * logLik(lmer.2)[1]

# Output the estimated random-effects
anova(lmer.1, lmer.2)

summary(lmer.1)
summary(lmer.2)

###################################################
### Fit the mixed-effects model: Unconditional quadratic
###################################################
lmer.3 = lmer(score ~ 1 + grade + (1 + grade | id), data = vocab_long, REML = FALSE)
summary(lmer.3)

# Output the estimated random-effects
anova(lmer.1, lmer.3)
anova(lmer.2, lmer.3)


