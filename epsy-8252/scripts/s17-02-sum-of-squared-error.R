# Enter data into vectors
x = c(4, 0, 3, 4, 7, 0, 0, 3, 0, 2)
y = c(53, 56, 37, 55, 50, 36, 22, 75, 37, 42)

# Input values for b0 (intercept) and b1 (slope)
b0 = 4
b1 = 0

# Compute sum of squared errors
sse = sum( (y - b0 - b1*x)^2 )
sse




# Wrap this up into a function
x = c(4, 0, 3, 4, 7, 0, 0, 3, 0, 2)
y = c(53, 56, 37, 55, 50, 36, 22, 75, 37, 42)

sse = function(b0 = 0, b1 = 0){
  sse = sum( (y - b0 - b1*x)^2 )
  return(sse)
}

# Use the function
sse(b0 = 1, b1 = 0)



# Use to search across values for b1

# Set up search space
search_space = data.frame(
  b0 = -25:25
)

search_space

library(tidyverse)

search_space %>% 
  rowwise() %>%
  mutate( SSE = sse(b0, b1 = 0) )

# OR...

data.frame( b0 = -25:25 ) %>%
  rowwise() %>%
  mutate( SSE = sse(b0, b1 = 0) )


library(tidyverse)

# Plot results
data.frame( b0 = -25:75 ) %>% 
  rowwise() %>%
  mutate( SSE = sse(b0, b1 = 0) ) %>%
  ggplot(data = ., aes(x = b0, y = SSE)) +
  geom_point()



sse(b0 = 0, b1 = 1:10)

city$resid = ifelse(residuals(lm.3) < 0, "#F39D4177", "steelblue")

# Add regression plane
my.lm = lm(city$income ~ city$education + city$seniority)
s3d$plane3d(my.lm, lty = 1, col = "red")





library(scatterplot3d)
s3d = scatterplot3d(x = new$b0, y = new$b1, z = new$SSE, type = "p", 
                    angle = 60, pch = 16, box = FALSE, xlab = "b0", ylab = "b1", zlab = "SSE", 
                    cex.symbols = 2, label.tick.marks = FALSE)


persp(x = new$b0, y = new$b1, z = new$SSE)


%>%
  ggplot(data = ., aes(x = b0, y = SSE)) +
  geom_point()


lm(y~1+x)


new = expand.grid( 
  b0 = seq(from = 30, to = 50, by = 0.1), 
  b1 = seq(from = -10, to = 10, by = 0.1)
  ) %>% 
  rowwise() %>%
  mutate( SSE = sse(b0, b1) ) %>%
  arrange(SSE)

new

library(rgl)
library(car)
scatter3d(SSE ~ b0 + b1, data = new, fit = "smooth", grid = FALSE)
scatter3d(x = 39, y = 3, z = 1570, add = TRUE, col = "black")



# Image Plot
ggplot(data = new, aes(x = b0, y = b1)) +
  geom_point(aes(color = SSE)) +
  theme_bw() +
  scale_colour_gradientn(colours = rainbow(50)) +
  geom_point(x = 40.1, y = 2.7, size = 4)


ggplot(data = new, aes(x = b0, y = b1, z = SSE)) +
  stat_contour(binwidth = 50) +
  theme_bw()


ggplot(data = new, aes(x = b0, y = b1, z = SSE)) +
  geom_tile(aes(fill = SSE)) + 
  stat_contour() +
  theme_bw()
  
