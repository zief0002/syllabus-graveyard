###########################################################
# Load libraries
###########################################################

library(tidyverse)
library(corrr)
library(lme4)
library(stringr)
library(AICcmodavg)


# This is to disable scientific notation 
options(scipen = 99) 



###########################################################
# Read in and prepare the data
###########################################################

vocab = read_csv("~/Dropbox/epsy-8282/data/vocab.csv")


# Create long data
vocab_long = vocab %>% 
  gather(time, score, t8:t11) %>%
  mutate(
    grade = as.integer(str_replace(time, pattern = "t", replacement = "")) - 8
  ) %>%
  arrange(id, grade) 

head(vocab_long)



###########################################################
# Fit lmer model assuming independent error structure
###########################################################

lmer.1 = lmer(score ~ 1 + grade + (1 + grade | id), data = vocab_long, REML = FALSE)
summary(lmer.1)



###########################################################
# Examine correlations b/w residuals
###########################################################

out = broom::augment(lmer.1)
head(out)

out %>%
  select(id, grade, .resid) %>%  # Only work with the id, grade and .resid columns
  spread(grade, .resid) %>%       # Convert these columns to wide data
  select(-id) %>%               # Drop the Rat column
  correlate() %>%                # Compute correlation matrix
  fashion(decimals = 2)



###########################################################
# Fit lme model assuming independent error structure
###########################################################

library(nlme)

# Fit model with independent level-1 error structure
lme.independence = lme(score ~ 1 + grade, random = ~ 1 + grade | id, 
                       data = vocab_long)

summary(lme.independence)



###########################################################
# Examine correlational structure via autocorrelation
###########################################################

ACF(lme.independence)

plot(ACF(lme.independence), alpha = 0.05)



###########################################################
# Fit lme model with unstructured error structure
###########################################################

lme.us = lme(score ~ 1 + grade, random = ~ 1 + grade | id,         
             data = vocab_long, correlation = corSymm(),
             weights = varIdent(form = ~ 1 | grade) )

summary(lme.us)

getVarCov(lme.us, type = "conditional")


###########################################################
# Fit lme model with compound symmetry error structure (initial rho = 0.3)
###########################################################

lme.cs = lme(score ~ 1 + grade, random = ~ 1 + grade | id,         
             data = vocab_long, correlation = corCompSymm(.3))

summary(lme.cs)

getVarCov(lme.cs, type = "conditional")



###########################################################
# Fit lme model with compound symmetry error structure (initial rho = 0.5)
###########################################################

lme.cs2 = lme(score ~ 1 + grade, random = ~ 1 + grade | id,         
              data = vocab_long, correlation = corCompSymm(.5))

summary(lme.cs2)

getVarCov(lme.cs2, type = "conditional")



###########################################################
# Fit lme model with compound symmetry error structure (initial rho = 0)
###########################################################

lme.cs3 = lme(score ~ 1 + grade, random = ~ 1 + grade | id,         
              data = vocab_long, correlation = corCompSymm())

summary(lme.cs3)

getVarCov(lme.cs3, type = "conditional")



###########################################################
# Fit lme model with compound symmetry error structure (fix rho to 0.5)
###########################################################

lme.cs4 = lme(score ~ 1 + grade, random = ~ 1 + grade | id,         
              data = vocab_long, correlation = corCompSymm(.5, fixed = TRUE))

summary(lme.cs4)

getVarCov(lme.cs4, type = "conditional")



###########################################################
# Fit lme model with AR(1) error structure
###########################################################

lme.ar1 = lme(score ~ 1 + grade, random = ~ 1 + grade | id,         
              data = vocab_long, correlation = corAR1())

summary(lme.ar1)

getVarCov(lme.ar1, type = "conditional")



###########################################################
# Compare models fit with different error structures
###########################################################

aictab(
  cand.set = list(lme.independence, lme.us, lme.cs, lme.ar1),
  modnames = c("Independent", "Unstructured", "Compound Symmetry", "AR(1)")
)

