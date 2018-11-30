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
### Plot and describe data
###################################################

ggplot(data = vocab_long, aes(x = grade, y = score, group = id)) + 
  geom_line(alpha = 0.4) +
  geom_point(alpha = 0.4) +
  stat_summary(group = 1, fun.y = mean, geom = "line", color = "blue", lwd = 1.5) +
  stat_summary(group = 1, fun.y = mean, geom = "point", color = "blue", size = 4) +
  theme_bw() +
  xlab("Centered grade (0 = 8th grade)") +
  ylab("Vocabulary score")



###################################################
### Fit the mixed-effects model
###################################################

lmer.1 = lmer(score ~ 1 + grade + (1 + grade | id), data = vocab_long, REML = FALSE)
summary(lmer.1)


# Output the estimated random-effects
ranef(lmer.1)



###################################################
### Plotting the fitted fixed-effects model
###################################################

plot_data = expand.grid(
  grade = seq(from = 0, to = 3, by = 0.1)
) 

plot_data$yhat = predict(lmer.1, newdata = plot_data, re.form = NA)
head(plot_data)

ggplot(data = plot_data, aes(x = grade, y = yhat)) +
  geom_line() +
  theme_bw() +
  xlab("Centered grade (0 = 8th grade)") +
  ylab("Predicted vocabulary score")



###################################################
### Plotting the individual lines
###################################################

plot_data = expand.grid(
  grade = seq(from = 0, to = 3, by = 0.1),
  id = 1:64
) 

plot_data$yhat = predict(lmer.1, newdata = plot_data)
head(plot_data)

ggplot(data = plot_data, aes(x = grade, y = yhat, group = id)) +
  geom_line(alpha = 0.5) +
  theme_bw() +
  xlab("Centered grade (0 = 8th grade)") +
  ylab("Predicted vocabulary score")



###################################################
### Fit the mixed-effects model (quadratic)
###################################################

lmer.2 = lmer(score ~ 1 + grade + I(grade^2) + 
                (1 + grade + I(grade^2) | id), 
                data = vocab_long, REML = FALSE)
summary(lmer.2)


# Output the estimated random-effects
ranef(lmer.2)



###################################################
### Plotting the fitted fixed-effects model
###################################################

plot_data = expand.grid(
  grade = seq(from = 0, to = 3, by = 0.1)
) 

plot_data$yhat = predict(lmer.2, newdata = plot_data, re.form = NA)
head(plot_data)

ggplot(data = plot_data, aes(x = grade, y = yhat)) +
  geom_line() +
  theme_bw() +
  xlab("Centered grade (0 = 8th grade)") +
  ylab("Predicted vocabulary score")



###################################################
### Plotting the individual lines
###################################################

plot_data = expand.grid(
  grade = seq(from = 0, to = 3, by = 0.1),
  id = 1:64
) 

plot_data$yhat = predict(lmer.2, newdata = plot_data)
head(plot_data)

ggplot(data = plot_data, aes(x = grade, y = yhat, group = id)) +
  geom_line(alpha = 0.5) +
  theme_bw() +
  xlab("Centered grade (0 = 8th grade)") +
  ylab("Predicted vocabulary score")



###################################################
### Fit the mixed-effects model (quadratic)
###################################################

lmer.3 = lmer(score ~ 1 + grade + female + (1 + grade | id), 
              data = vocab_long, REML = FALSE)
summary(lmer.3)


# Plot the fixed-effects model
plot_data = expand.grid(
  grade = seq(from = 0, to = 3, by = 0.1),
  female = 0:1
) 

plot_data$yhat = predict(lmer.3, newdata = plot_data, re.form = NA)
plot_data$sex = factor(plot_data$female, levels = c(0, 1), labels = c("Male", "Female"))

head(plot_data)

ggplot(data = plot_data, aes(x = grade, y = yhat, color = sex)) +
  geom_line(alpha = 0.5) +
  theme_bw() +
  xlab("Centered grade (0 = 8th grade)") +
  ylab("Predicted vocabulary score") +
  ggsci::scale_color_d3(name = "")

# Output the estimated random-effects
ranef(lmer.3)


# Plot individual-specific lines
plot_data = expand.grid(
  grade = seq(from = 0, to = 3, by = 0.1),
  id = 1:64
) 

id_sex = vocab %>% select(id, female)
head(id_sex)


plot_data2 = plot_data %>%
  left_join(id_sex, by = "id")
head(plot_data2)

plot_data2$yhat = predict(lmer.3, newdata = plot_data2)
plot_data2$sex = factor(plot_data2$female, levels = c(0, 1), labels = c("Male", "Female"))


ggplot(data = plot_data2, aes(x = grade, y = yhat, group = id, color = sex)) +
  geom_line(alpha = 0.5) +
  theme_bw() +
  xlab("Centered grade (0 = 8th grade)") +
  ylab("Predicted vocabulary score") +
  ggsci::scale_color_d3(name = "")

