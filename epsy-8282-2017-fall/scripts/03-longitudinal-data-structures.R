##################################################
### Load libraries
##################################################

library(ggjoy)
library(readr)
library(tidyr)



###################################################
### Read in the data
###################################################

bigten_wide = read_csv(file = "~/Dropbox/epsy-8282/data/big-ten-wide.csv")
head(bigten_wide)



###################################################
### Wide to long
###################################################

bigten_long = bigten_wide %>% gather(year, gsr, gsr2008:gsr2015)
bigten_long

bigten_long = bigten_wide %>% 
  gather(year, gsr, gsr2008:gsr2015) %>%
  arrange(school)

bigten_long



###################################################
### Long to wide
###################################################

bigten_long %>% spread(year, gsr)



###################################################
### Create mpls data
###################################################

mpls = data.frame(
  student = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
  read = c(172, 185, 179, 194, 200, 210, 209, NA, 191, 199, 203, 215),
  grade = c(5, 6, 7, 8, 5, 6, 7, 8, 5, 6, 7, 8),
  sex = c(NA, NA, NA, NA, "F", "F", "F", "F", "M", "M", "M", "M")
)
mpls



###################################################
### Deleting cases
###################################################

mpls %>% drop_na()
mpls %>% drop_na(read)
mpls %>% drop_na(read, sex)



###################################################
### Joyplots
###################################################

ggplot(bigten_long, aes(x = gsr, y = year)) +
  geom_joy(fill = "skyblue") + 
  theme_joy() +
  scale_y_discrete(expand = c(0.01, 0)) +   # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) +
  coord_flip()


