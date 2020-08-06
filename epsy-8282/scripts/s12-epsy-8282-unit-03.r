###################################################
### Statistical Analysis of Longitudinal Data I
### Spring 2012
### Andrew Zieffler
###
### Read in Minneapolis data using RStudio. Write
### to object mpls.
###################################################



###################################################
### Examine Data 
###################################################
head( mpls )
str( mpls )
summary( mpls )



###################################################
### Missing Data
###################################################
mpls == -99
mpls[mpls == -99] <- NA
head( mpls )
summary( mpls )

mean( mpls[ , 2] )
mean( mpls[ , 3] )
mean( mpls[ , 4] )

mean( mpls[ , 5] )  #Error
mean( mpls[ , 5], na.rm=TRUE ) 
mean( mpls[ , 2:5], na.rm=TRUE )

apply( mpls[ , 2:5], 2, mean, na.rm=TRUE )



###################################################
### Correlations
###################################################
cor( mpls[ , 2:5], use="complete.obs" )



###################################################
### Condition on Static Predictors
###################################################
tapply( mpls[ , 2], mpls[ , 6], mean, na.rm=TRUE )
tapply( mpls[ , 2], mpls[ , 11], mean, na.rm=TRUE )



###################################################
### Splitting Varaibles
###################################################
median( mpls[ , 11] )
ifelse( mpls[ , 11] <= 0.97, 1, 0 )
mpls$att2 <- ifelse( mpls[ , 11] <= 0.97, 1, 0 )
tapply( mpls[ , 2], mpls[ , 12], mean, na.rm=TRUE )

library( car )
cut( mpls[ , 11], c(0, 0.95, 0.97, 0.98, 1) )
tapply( mpls[ , 2], cut( mpls[ , 11], c(0, 0.95, 0.97, 0.98, 1) ), mean, na.rm=TRUE )



###################################################
### Reshaping Data
###################################################
mpls.l <- reshape( data=mpls, 
                   idvar="subid", 
                   varying=2:5,
                   v.names="read", 
                   times=5:8, 
                   timevar="grade",
                   direction="long" )

head( mpls.l, n=10 )



###################################################
### Sort by Subject ID
###################################################
library ( plyr )  #needs to be installed
arrange( mpls.l, subid )
mpls.l <- arrange( mpls.l, subid )
head( mpls.l, n=10 )



###################################################
### Mising Data II
###################################################
mpls.l <- na.omit( mpls.l )
head( mpls.l, n=10 )