##################################################
### Load libraries
##################################################

library(corrr)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)



##################################################
### Read in data
##################################################

mpls = read_csv(file = "~/Dropbox/epsy-8282/data/minneapolis.csv")
head(mpls)



##################################################
### Wide to long
##################################################

mpls_long = mpls %>% 
  gather(grade, read, read.5:read.8) %>%
  arrange(studentID, grade)

mpls_long



##################################################
### Deal with missing data
##################################################

mpls_miss = mpls %>% 
  gather(variable, value, read.5:att) %>%
  mutate(missing = is.na(value))

# Count missing data for each variable
mpls_miss %>% 
  group_by(variable) %>% 
  summarize(
    n_miss = sum(missing), 
    prop_miss = sum(missing) / length(missing)
  )

# Plot of missingness
ggplot(data = mpls_miss, aes(x = variable, y = studentID)) +
  geom_tile(aes(fill = missing), color = "black") +
  scale_fill_manual(name = "", labels = c("Observed", "Missing"), values = c("lightgrey", "darkred")) +
  theme_minimal() + 
  theme(axis.text.x  = element_text(angle = 70, vjust = 0.5)) + 
  xlab("") +
  ylab("Student")

# Explore whether missingness is related to covariates
mpls %>% 
  mutate(missing = is.na(read.8)) %>%
  correlate() %>%
  focus(-studentID, -read.8, mirror = TRUE) %>%
  select(rowname, missing) 



##################################################
### Individual curves (profiles)
##################################################

ggplot(mpls_long, aes(x = grade, y = read, group = studentID)) +
  geom_line() + 
  theme_bw() +
  xlab("Grade") +
  ylab("Reading score")



##################################################
### Individual curves (profiles) -- Use grade as integer
##################################################

# Create grade integer
mpls_long = mpls_long %>% mutate(grade2 = as.integer(as.factor(grade)) + 4)
head(mpls_long)

# Plot indovidual profiles
ggplot(mpls_long, aes(x = grade2, y = read)) +
  geom_line(aes(group = studentID)) + 
  theme_bw() +
  xlab("Grade") +
  ylab("Reading score")



##################################################
### Individual curves (profiles) and points
##################################################

ggplot(mpls_long, aes(x = grade2, y = read)) +
  geom_line(aes(group = studentID)) + 
  geom_point(aes(group = studentID)) + 
  theme_bw() +
  xlab("Grade") +
  ylab("Reading score")



##################################################
### Individual curves (profiles) -- Facet by student
##################################################

ggplot(mpls_long, aes(x = grade2, y = read)) +
  geom_line(aes(group = studentID)) + 
  geom_point(aes(group = studentID)) + 
  theme_bw() +
  xlab("Grade") +
  ylab("Reading score") +
  facet_wrap(~studentID)



##################################################
### Individual curves (profiles), facet by student, OLS regression lines
##################################################

ggplot(mpls_long, aes(x = grade2, y = read)) +
  geom_line(aes(group = studentID)) + 
  geom_point(aes(group = studentID)) + 
  geom_smooth(aes(group = studentID), method = "lm", se = FALSE) + 
  theme_bw() +
  xlab("Grade") +
  ylab("Reading score") +
  facet_wrap(~studentID)



##################################################
### Individual curves (profiles), facet by student, OLS quadratic regression curves
##################################################

ggplot(mpls_long, aes(x = grade2, y = read)) +
  geom_line(aes(group = studentID)) + 
  geom_point(aes(group = studentID)) + 
  geom_smooth(aes(group = studentID), method = "lm", se = FALSE, formula = y~poly(x, 2)) + 
  theme_bw() +
  xlab("Grade") +
  ylab("Reading score") +
  facet_wrap(~studentID)





##################################################
### Only plot Student 2
##################################################

mpls_long %>% 
  filter(studentID == 2) %>%
  ggplot(., aes(x = grade2, y = read)) +
    #geom_line() + 
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE, color = "blue") + 
    geom_smooth(method = "lm", se = FALSE, formula = y~poly(x, 2), color = "red") + 
    geom_smooth(method = "lm", se = FALSE, formula = y~poly(x, 3), color = "green") + 
    theme_bw() +
    xlab("Grade") +
    ylab("Reading score")



##################################################
### Mean profile, points, OLS linear and quadratic curves
##################################################

ggplot(mpls_long, aes(x = grade2, y = read)) +
  stat_summary(group = 1, fun.y = mean, geom = "line", lwd = 1.5) +
  stat_summary(group = 1, fun.y = mean, geom = "point", size = 3) +
  geom_smooth(group = 1, method = "lm", se = FALSE) + 
  geom_smooth(group = 1, method = "lm", se = FALSE, formula = y~poly(x, 2), color = "red") + 
  theme_bw() +
  xlab("Grade") +
  ylab("Reading score")



##################################################
### Individual profiles, mean profile with points
##################################################

ggplot(mpls_long, aes(x = grade2, y = read)) +
  geom_line(aes(group = studentID), alpha = 0.3) + 
  stat_summary(group = 1, fun.y = mean, geom = "line", lwd = 1.5) +
  stat_summary(group = 1, fun.y = mean, geom = "point", size = 3) +
  theme_bw() +
  xlab("Grade") +
  ylab("Reading score")



##################################################
### Profiles by sex
##################################################

# Convert female (dummy-coded) to factor
mpls_long = mpls_long %>% 
  mutate(sex = factor(female, levels = c(0, 1), labels = c("Female", "Male")))
head(mpls_long)

# Plot both sexes on same panel
ggplot(mpls_long, aes(x = grade2, y = read, color = sex)) +
  geom_line(aes(group = studentID), alpha = 0.4) + 
  stat_summary(aes(group = sex), fun.y = mean, geom = "line", lwd = 1.5) +
  stat_summary(aes(group = sex), fun.y = mean, geom = "point", size = 3) +
  theme_bw() +
  xlab("Grade") +
  ylab("Reading score") +
  ggsci::scale_color_d3(name = "")

# Facet by sex
ggplot(mpls_long, aes(x = grade2, y = read, color = sex)) +
  geom_line(aes(group = studentID), alpha = 0.4) + 
  stat_summary(aes(group = sex), fun.y = mean, geom = "line", lwd = 1.5) +
  stat_summary(aes(group = sex), fun.y = mean, geom = "point", size = 3) +
  theme_bw() +
  xlab("Grade") +
  ylab("Reading score") +
  ggsci::scale_color_d3() +
  facet_wrap(~sex) +
  guides(color = FALSE)



##################################################
### Profiles by attendance
##################################################

ggplot(mpls_long, aes(x = grade2, y = read)) +
  geom_line(aes(group = studentID), alpha = 0.4) + 
  stat_summary(aes(group = att), fun.y = mean, geom = "line", lwd = 1.5) +
  stat_summary(aes(group = att), fun.y = mean, geom = "point", size = 3) +
  theme_bw() +
  xlab("Grade") +
  ylab("Reading score") +
  facet_wrap(~att)

# Discretize attendance (3 categories)
mpls_long = mpls_long %>% mutate(att_category = cut_interval(att, n = 3))
head(mpls_long)

# PLot
ggplot(mpls_long, aes(x = grade2, y = read)) +
  geom_line(aes(group = studentID), alpha = 0.4) + 
  stat_summary(aes(group = att_category), fun.y = mean, geom = "line", lwd = 1.5) +
  stat_summary(aes(group = att_category), fun.y = mean, geom = "point", size = 3) +
  theme_bw() +
  xlab("Grade") +
  ylab("Reading score") +
  facet_wrap(~att_category, nrow = 1)



##################################################
### Summaries by measurement wave
##################################################

mpls_long %>%
  group_by(grade2) %>%
  summarize(M = mean(read), SD = sd(read))

# Omit missing data
mpls_long %>%
  na.omit() %>%
  group_by(grade2) %>%
  summarize(M = mean(read), SD = sd(read))



##################################################
### Summaries by sex and measurement wave
##################################################

mpls_long %>%
  na.omit() %>%
  group_by(grade2, sex) %>%
  summarize(M = mean(read), SD = sd(read)) %>%
  arrange(sex)
  


##################################################
### Correlations between the repeated measures
##################################################

mpls %>% 
  select(read.5:read.8) %>%
  correlate() %>%
  shave() %>%
  fashion(decimals = 3)


# Omit missing data
mpls %>% 
  select(read.5:read.8) %>%
  na.omit() %>%
  correlate() %>%
  shave() %>%
  fashion(decimals = 3)



##################################################
### Covariance between the repeated measures
##################################################

# Pairwise deletion
mpls %>% 
  select(read.5:read.8) %>%
  cov(., use = "pairwise.complete.obs")

# Listwise deletion
mpls %>% 
  select(read.5:read.8) %>%
  cov(., use = "complete.obs")



##################################################
### Variance at each measurement wave
##################################################

mpls_long %>% 
  group_by(grade) %>% 
  na.omit() %>% 
  summarize(Var = var(read))
  
