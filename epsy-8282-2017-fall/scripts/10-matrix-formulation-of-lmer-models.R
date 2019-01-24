library(tidyverse)
library(lme4)
library(stringr)



# Read in wide data
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



lmer.1 = lmer(score ~ 1 + grade + (1 | id), data = vocab_long, REML = FALSE)

# Obtain fixed-effecta
fixef(lmer.1)

# Obtain random-effects (for first two subjects)
head(ranef(lmer.1)$id)

# Get predicted values

vocab_long %>%
  mutate( yhat = predict(lmer.1) )

vocab_long %>%
  mutate( yhat = predict(lmer.1, re.form = NA) )


# Design matrix of the fixed-effects
X = matrix(data = c(1, 1, 1, 1, 0, 1, 2, 3), ncol = 2)
X

# Design matrix of the random-effects
Z = matrix(data = c(1, 1, 1, 1), ncol = 1)
Z

# Column vector of the estimated fixed-effects
B = matrix(data = c(1.41, 0.75), ncol = 1)
B

# Column vector of the estimated random-effects
b = matrix(data = c(0.39), ncol = 1)
b

X %*% B + Z %*% b


Y = vocab %>%
  select(t8:t11) %>%
  cov(.)

Y


lmer.2 = lmer(score ~ 1 + grade + (1 + grade | id), data = vocab_long, REML = FALSE)


# Obtain variance estimate for the level-1 error
summary(lmer.2)$sigma ^ 2

# Obtain variance-covariance matrix of the random-effects
VarCorr(lmer.2)$id[1:2, 1:2]


# Error variance
sigma2error = 0.89633196

# Compute estimated within-subjects variance-covariance matrix
W = sigma2error * diag(4)
W


# Enter design matrix for random-effects
Z = matrix(data = c(1, 1, 1, 1, 0, 1, 2, 3), ncol = 2)
Z

# Enter variance-covariance matrix for the random-effects
G = matrix(data = c(3.13762586, 0.01535409, 0.01535409, 0.00007513581) , ncol = 2)
G

# Compute the estiated between-subjects variance-covariance matrix
B = Z %*% G %*% t(Z)
B


V = B + W
V


# Create the diagonal matrix based on the variances
D = diag( 1/sqrt(diag(V)) )
D

# Compute the standardized V_i
D %*% V %*% D


cov2cor(Y)


