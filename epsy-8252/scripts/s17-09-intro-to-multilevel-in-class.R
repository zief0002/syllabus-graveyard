#########################################
## Load libraries we will need
#########################################

library(broom) # Need for the tidy() function
library(haven) # Need for the read_sav() function
library(dplyr)
library(ggplot2)
library(lme4) # Need for the lmer() function



#########################################
## Read in the SAV files
#########################################

# Read in player-level data
nbaL1 = read_sav(file = "~/Google Drive/Documents/EPsy-8252/data/nbaLevel1.sav")
head(nbaL1)

# Read in team-level data
nbaL2 = read_sav(file = "~/Google Drive/Documents/EPsy-8252/data/nbaLevel2.sav")
head(nbaL2)



#########################################
## Merge the two datasets together
#########################################

nba = left_join(nbaL1, nbaL2, by = "Team_ID")
head(nba)



#########################################
## Fit AVERAGE model
#########################################

lm.1 = lm(Life_Satisfaction  ~ 1 + Shots_on_five, data = nba)
summary(lm.1)



#########################################
## Fit TEAM-SPECIFIC models
#########################################

models = nba %>%
  group_by(Team_ID) %>%
  do( mod = lm(Life_Satisfaction  ~ Shots_on_five, data = .) ) %>%
  tidy(mod)

# Show output
models



#########################################
## Fit multilevel model
#########################################

lmer.1 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + (1 + Shots_on_five | Team_ID), data = nba)

# Get fixed-effects
fixef(lmer.1)

# Get random-effects
ranef(lmer.1)


