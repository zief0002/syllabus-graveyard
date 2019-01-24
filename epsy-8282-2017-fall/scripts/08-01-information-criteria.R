##################################################
### Load libraries
##################################################

library(tidyverse)
library(lme4)
library(stringr)
library(AICcmodavg)


###################################################
### Read in data
###################################################

vocab = read_csv(file = "~/Dropbox/epsy-8282/data/vocab.csv")
head(vocab)


### Prepare data
vocab_long = vocab %>% 
  gather(time, score, t8:t11) %>%
  mutate(
    grade = as.integer(str_replace(time, pattern = "t", replacement = "")) - 8
    ) %>%
  arrange(id, grade) 

vocab_long



###################################################
### See the iterations of the estimation
###################################################

# Fit unconditional means model
lmer.0 = lmer(score ~ 1 + (1 | id), data = vocab_long, REML = FALSE, verbose = 2)



###################################################
### Compute AIC
###################################################

# Fit unconditional means model and linear growth model
lmer.0 = lmer(score ~ 1 +         (1 | id), data = vocab_long, REML = FALSE)
lmer.1 = lmer(score ~ 1 + grade + (1 | id), data = vocab_long, REML = FALSE)

# Compute AIC values
AIC(lmer.0)
AIC(lmer.1)



###################################################
### Compute BIC
###################################################

BIC(lmer.0)
BIC(lmer.1)



###################################################
### Compute AICc
###################################################

AICc(lmer.0)
AICc(lmer.1)



###################################################
### Model selection: Fixed-effects
###################################################

# Fit unconditional means model, linear growth model, and quadratic growth model
lmer.0 = lmer(score ~ 1 +                      (1 | id), data = vocab_long, REML = FALSE)
lmer.1 = lmer(score ~ 1 + grade +              (1 | id), data = vocab_long, REML = FALSE)
lmer.2 = lmer(score ~ 1 + grade + I(grade^2) + (1 | id), data = vocab_long, REML = FALSE)


# AICc Table for Model Selection
myAIC = aictab(
  cand.set = list(lmer.0, lmer.1, lmer.2),
  modnames = c("Unconditional means model", "Linear effect of grade", "Quadratic effect of grade") 
  )


# View table
myAIC



###################################################
### Model selection: Random-effects
###################################################

# Fit the models with differeing RE structures
lmer.2.0 = lmer(score ~ 1 + grade + I(grade^2) + (1 | id), data = vocab_long, REML = FALSE)
lmer.2.1 = lmer(score ~ 1 + grade + I(grade^2) + (1 + grade | id), data = vocab_long, REML = FALSE)
lmer.2.2 = lmer(score ~ 1 + grade + I(grade^2) + (1 + grade + I(grade^2) | id), data = vocab_long, REML = FALSE)


# AICc Table for Model Selection
myAIC = aictab(
  cand.set = list(lmer.2.0, lmer.2.1, lmer.2.2),
  modnames = c("Intercept RE", "Intercept and Linear RE", "Intercept, Linear, and Quadratic RE")
)

# View table
myAIC


# Evidence ratio between lmer.2.0 and lmer.2.1
exp(-0.5*0) / exp(-0.5*4.18)



###################################################
### Model selection: Covariates
###################################################

# Fit the models
lmer.2.0   = lmer(score ~ 1 + grade + I(grade^2) +          (1 | id),         data = vocab_long, REML = FALSE)
lmer.2.0.f = lmer(score ~ 1 + grade + I(grade^2) + female + (1 | id),         data = vocab_long, REML = FALSE)

lmer.2.1   = lmer(score ~ 1 + grade + I(grade^2) +          (1 + grade | id), data = vocab_long, REML = FALSE)
lmer.2.1.f = lmer(score ~ 1 + grade + I(grade^2) + female + (1 + grade | id), data = vocab_long, REML = FALSE)


# AICc Table for Model Selection
myAIC = aictab(
  cand.set = list(lmer.2.0, lmer.2.1, lmer.2.0.f, lmer.2.1.f),
  modnames = c("Intercept RE", 
               "Intercept and Linear RE", 
               "Intercept RE + Female", 
               "Intercept and Linear RE + Female"
               )
)

# View table
myAIC


# Evidence ratio between lmer.2.0 and lmer.2.0.f
exp(-0.5*0) / exp(-0.5*2.04)



###################################################
### Pretty printing the table
###################################################

# Select the first 6 columns
x = data.frame(myAIC)[ , 1:6]

# Change the column names
names(x) = c("Model", "K", "AICc", "Delta(AICc)", "Rel. Lik.", "Model Prob.")


###################################################
### Pretty printing the table (if you are using Markdown)
###################################################

# Here we employ LaTeX math notation in the names to use the Greek letter Delta

knitr::kable(
  data.frame(myAIC)[ , 1:6],
  col.names = c("Model", "K", "AICc", "$\\Delta$AICc", "Rel. Lik.", "Model Prob."), 
  digits = 3
)




