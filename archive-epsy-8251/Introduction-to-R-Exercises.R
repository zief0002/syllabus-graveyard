##################################################
### Read in data
##################################################

city = read.csv(file = "~/Google Drive/andy/epsy-8251/data/riverside.csv") 

head(city)
tail(city)



##################################################
### Compute the mean income for the 32 employees.
##################################################

mean(city$income)



##################################################
### Compute the standard deviation of incomes for the 32 employees.
##################################################

sd(city$income)



##################################################
### Load the sm package and use the sm.density() 
### function to plot the incomes for the 32 employees. 
### Export the plot, and import it •into a word-processing program.
##################################################

library(sm)
sm.density(city$income)



##################################################
### Use indexing to view the 15th row.
##################################################

city[15, ]



##################################################
### Use the table() function to count the number of employees of each gender.
##################################################

table(city$gender)

