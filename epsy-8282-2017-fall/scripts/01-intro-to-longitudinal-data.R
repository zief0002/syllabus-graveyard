##################################################
### Load libraries
##################################################

library(dplyr)
library(ggplot2)
library(readr)
library(sm)



###################################################
### Read in the data
###################################################

big10_wide = read_csv(file = "~/Dropbox/epsy-8282/data/big-ten-wide.csv")
head(big10_wide)

fb2 = read.csv(file = "/Users/zief0002/Desktop/big-ten-long.csv")
head(fb2) 

library(ggplot2)
library(dplyr)


# Show the data
fb %>% dplyr::select(school, gsr2008, gsr2011) %>% na.omit() %>% dplyr::select(school, gsr2008, gsr2011)

# Summarize
fb %>% dplyr::select(school, gsr2008, gsr2011) %>% 
  na.omit() %>% 
  tidyr::gather(., year, gsr, -school) %>%
  group_by(year) %>% 
  summarize(
    M = mean(gsr, na.rm = TRUE), 
    SD = sd(gsr, na.rm = TRUE), 
    N = sum(!is.na(gsr))
    )
  

# PLot assuming independence
fb2 %>% filter(year %in% c(2008, 2011)) %>%
  ggplot(data = ., aes(x = year, y = gsr)) +
    geom_point(pch = 21, fill = "skyblue") +
    stat_summary(fun.y = "mean", geom = "line", lwd = 1.5, group = 1) +
    stat_summary(fun.y = "mean", geom = "point", pch = 24, fill = "orange", size = 5) +
    theme_bw() +
    xlab("Year") +
    ylab("Graduation success rate (GSR)") +
    ylim(50, 100) +
    xlim(2007, 2012)


# Inappropriate analysis
fb3 = fb %>% dplyr::select(school, gsr2008, gsr2011) %>% na.omit()
t.test(x = fb3$gsr2008, y = fb3$gsr2011, var.equal = TRUE)

fb4 = tidyr::gather(fb3, year, gsr, -school)
summary(lm(gsr ~ 1 + year, data = fb4))


# Correlation
cor(fb[c("gsr2008", "gsr2011")], use = "pairwise.complete.obs")

# Plot: Link the measurements
fb3 = fb2 %>% filter(year == 2008)

fb2 %>% filter(year %in% c(2008, 2011)) %>%
  ggplot(data = ., aes(x = year, y = gsr)) +
  geom_line(aes(group = school), linetype = "dashed") +
  geom_point(pch = 21, fill = "skyblue") +
  stat_summary(fun.y = "mean", geom = "line", group = 1, lwd = 1.5) +
  stat_summary(fun.y = "mean", geom = "point", pch = 24, fill = "orange", size = 5) +
  theme_bw() +
  xlab("Year") +
  ylab("Graduation success rate (GSR)") +
  ylim(50, 100) +
  geom_text(data = fb3, aes(label = school), hjust = 1, size = 3, nudge_x = -0.1) +
  xlim(2007, 2012)


# Paired samples t-test
t.test(x = fb$gsr2008, y = fb$gsr2011, var.equal = TRUE, paired = TRUE)


# Draw t-distributions
x = seq(from = -4, to = 4, by = .001)
y10 = dt(x, df = 3)
y20 = dt(x, df = 20)


cord.x = c(3.33, seq(from = 3.33, to = 4, by = 0.01), 4) 
cord.y = c(0, dt(seq(from = 3.33, to = 4, by = 0.01), df = 3), 0)

cord.x2 = c(-4, seq(from = -4, to = -3.33, by = 0.01), -3.33) 
cord.y2 = c(0, dt(seq(from = -4, to = -3.33, by = 0.01), df = 3), 0)


plot(x, y20, axes = "n", type = "l", bty = "n", ylab = "", xlab = "")
axis(side = 1, at = -4:4, pos = 0)
lines(x, y10, type = "l", lty = "dotted")

polygon(x = cord.x, y = cord.y, col = "skyblue")
polygon(x = cord.x2, y = cord.y2, col = "skyblue")
