###################################################
### Statistical Analysis of Longitudinal Data I
### Spring 2012
### Andrew Zieffler
###
### Read in Minneapolis-Long data using RStudio. Write
### to object mpls.l.
###################################################



###################################################
### Prepare
###################################################
head( mpls.l )
library( ggplot2 )  #needs to be installed


###################################################
### Spaghetti Plot
###################################################
ggplot( data=mpls.l, aes( x=grade, y=read, group=subid ) ) +
  geom_line()

ggplot( data=mpls.l, aes( x=grade, y=read, group=subid ) ) +
  geom_line() +
  theme_bw() +
  scale_x_continuous( name="Grade", breaks=5:8 ) +
  scale_y_continuous( name="Reading" )



###################################################
### Facet Plots
###################################################
ggplot( data=mpls.l, aes( x=grade, y=read, group=subid ) ) +
  geom_line() +
  geom_point() +
  facet_wrap( ~subid ) +
  theme_bw() +
  scale_x_continuous( name="Grade", breaks=5:8 ) +
  scale_y_continuous( name="Reading" )

ggplot( data=mpls.l, aes( x=grade, y=read, group=subid ) ) +
  geom_line() +
  geom_point() +
  facet_wrap( ~subid, nrow=2 ) +
  theme_bw() +
  scale_x_continuous( name="Grade", breaks=5:8 ) +
  scale_y_continuous( name="Reading" )



###################################################
### Plotting Subsets of Subjects
###################################################
sub1 <- subset( mpls.l, subid<6 )

ggplot( data=sub1, aes( x=grade, y=read, group=subid ) ) +
  geom_line() +
  geom_point() +
  facet_wrap( ~subid ) +
  theme_bw() +
  scale_x_continuous( name="Grade", breaks=5:8 ) +
  scale_y_continuous( name="Reading" )



###################################################
### Choose and Plot Random Sample of Subjects
###################################################
samp <- sample( 1:22, size=4)

sub2 <- subset( mpls.l, subid %in% samp )

ggplot( data=sub2, aes( x=grade, y=read, group=subid ) ) +
  geom_line() +
  geom_point() +
  facet_wrap( ~subid ) +
  theme_bw() +
  scale_x_continuous( name="Grade", breaks=5:8 ) +
  scale_y_continuous( name="Reading" )



###################################################
### Plotting Individual Fitted Curves using OLS
###################################################
ggplot( data=sub1, aes( x=grade, y=read, group=subid ) ) +
  geom_point() +
  facet_wrap( ~subid ) +
  stat_smooth( method="lm", se=FALSE ) +
  theme_bw() +
  scale_x_continuous( name="Grade", breaks=5:8 ) +
  scale_y_continuous( name="Reading" )

ggplot( data=sub1, aes( x=grade, y=read, group=subid ) ) +
  geom_point() +
  facet_wrap( ~subid ) +
  stat_smooth( method="lm", se=FALSE, formula=y~poly( x, 2 ) ) +
  theme_bw() +
  scale_x_continuous( name="Grade", breaks=5:8 ) +
  scale_y_continuous( name="Reading" )

ggplot( data=sub1, aes( x=grade, y=read, group=subid ) ) +
  geom_point() +
  facet_wrap( ~subid ) +
  stat_smooth( method="lm", se=FALSE, formula=y~poly( x, 3 ) ) +
  theme_bw() +
  scale_x_continuous( name="Grade", breaks=5:8 ) +
  scale_y_continuous( name="Reading" )  ##should produce error



###################################################
### Plotting Individual Fitted Curves using Local Smoothing
###################################################
ggplot( data=sub1, aes( x=grade, y=read, group=subid ) ) +
  geom_point() +
  facet_wrap( ~subid ) +
  stat_smooth( se=FALSE ) +
  theme_bw() +
  scale_x_continuous( name="Grade", breaks=5:8 ) +
  scale_y_continuous( name="Reading" )



###################################################
### Plotting Group Level Curves
###################################################
ggplot( data=mpls.l, aes( x=grade, y=read ) ) +
  stat_summary( fun.y=mean, geom="line" ) +
  stat_summary( fun.y=mean, geom="point" ) +
  theme_bw() +
  scale_x_continuous( name="Grade", breaks=5:8 ) +
  scale_y_continuous( name="Reading" )

ggplot( data=mpls.l, aes( x=grade, y=read ) ) +
  stat_summary( fun.y=mean, geom="line", lwd=1.5, lty=5 ) +
  stat_summary( fun.y=mean, geom="point", pch=19, cex=3 ) +
  geom_point( pch=1 ) +
  theme_bw() +
  scale_x_continuous( name="Grade", breaks=5:8 ) +
  scale_y_continuous( name="Reading" )



###################################################
### Jittering Points
###################################################
ggplot( data=mpls.l, aes( x=jitter( grade ), y=read ) ) +
  geom_point( pch=1 ) +
  theme_bw() +
  scale_x_continuous( name="Grade", breaks=5:8 ) +
  scale_y_continuous( name="Reading" )





###################################################
### Adding Individual Points and Group Curve
###################################################

#not correct
ggplot( data=mpls.l, aes( x=jitter( grade ), y=read ) ) +
  stat_summary( fun.y=mean, geom="line", lwd=1.5, lty=5 ) +
  geom_point( pch=1 ) +
  theme_bw() +
  scale_x_continuous( name="Grade", breaks=5:8 ) +
  scale_y_continuous( name="Reading" )

#linear group level curve
ggplot( data=mpls.l, aes( x=jitter( grade ), y=read ) ) +
  stat_smooth( method="lm", se=FALSE, lwd=1.5) +
  geom_point( pch=1 ) +
  theme_bw() +
  scale_x_continuous( name="Grade", breaks=5:8 ) +
  scale_y_continuous( name="Reading" ) +
  opts( "aspect.ratio"=1 )

#quadratic group level curve
ggplot( data=mpls.l, aes( x=jitter( grade ), y=read ) ) +
  stat_smooth( method="lm", se=FALSE, lwd=1.5, formula=y~poly( x, 2 ) ) +
  geom_point( pch=1 ) +
  theme_bw() +
  scale_x_continuous( name="Grade", breaks=5:8 ) +
  scale_y_continuous( name="Reading" ) +
  opts( "aspect.ratio"=1 )

#local smoothed group level curve (default span=0.75)
ggplot( data=mpls.l, aes( x=jitter( grade ), y=read ) ) +
  stat_smooth( se=FALSE, lwd=1.5 ) +
  geom_point( pch=1 ) +
  theme_bw() +
  scale_x_continuous( name="Grade", breaks=5:8 ) +
  scale_y_continuous( name="Reading" ) +
  opts( "aspect.ratio"=1 )

#local smoothed group level curve (span=0.4)
ggplot( data=mpls.l, aes( x=jitter( grade ), y=read ) ) +
  stat_smooth( se=FALSE, lwd=1.5, span=0.4 ) +
  geom_point( pch=1 ) +
  theme_bw() +
  scale_x_continuous( name="Grade", breaks=5:8 ) +
  scale_y_continuous( name="Reading" ) +
  opts( "aspect.ratio"=1 )

#local smoothed group level curve (span=0.9)
ggplot( data=mpls.l, aes( x=jitter( grade ), y=read ) ) +
  stat_smooth( se=FALSE, lwd=1.5, span=0.9 ) +
  geom_point( pch=1 ) +
  theme_bw() +
  scale_x_continuous( name="Grade", breaks=5:8 ) +
  scale_y_continuous( name="Reading" ) +
  opts( "aspect.ratio"=1 )




###################################################
### Adding Individual Curves and Group Curve
###################################################
ggplot( data=mpls.l, aes( x=grade, y=read, group=subid ) ) +
  geom_line( lty=3 ) +
  stat_summary( aes( group=1 ), fun.y=mean, geom="point", pch=19, cex=3 ) +
  stat_summary( aes( group=1 ), fun.y=mean, geom="line", lwd=1.5, lty=5 ) +
  theme_bw() +
  scale_x_continuous( name="Grade", breaks=5:8 ) +
  scale_y_continuous( name="Reading" ) +
  opts( "aspect.ratio"=1 )



###################################################
### Categorical Static Predictors
###################################################
table( mpls.l$risk )

ggplot( data=mpls.l, aes( x=grade, y=read, shape=risk ) ) +
  geom_point() +
  geom_line( lty=3 ) +
  stat_summary( aes( line=risk ), fun.y=mean, geom="line" ) +
  scale_shape_manual( values=c( 1, 8, 19 ) ) +
  theme_bw() +
  scale_x_continuous( name="Grade", breaks=5:8 ) +
  scale_y_continuous( name="Reading" ) +
  opts( "aspect.ratio"=1 )




###################################################
### Categorical Static Predictors
###################################################
table( mpls.l$gen, mpls.l$eth )

ggplot( data=mpls.l, aes( x=grade, y=read ) ) +
  geom_point() +
  geom_line( lty=3 ) +
  stat_summary( aes( line=gen:eth ), fun.y=mean, geom="line" ) +
  stat_summary( aes( shape=gen:eth ), fun.y=mean, geom="point", cex=2 ) +
  theme_bw() +
  scale_x_continuous( name="Grade", breaks=5:8 ) +
  scale_y_continuous( name="Reading" ) +
  opts( "aspect.ratio"=1, legend.position=c( 0.52, 0.27 ), legend.background=theme_rect() )



###################################################
### Faceting
###################################################
table( mpls.l$gen, mpls.l$eth )

ggplot( data=mpls.l, aes( x=grade, y=read, group=subid ) ) +
  geom_line( lty=3 ) +
  stat_summary( aes( group=1 ), fun.y=mean, geom="line" ) +
  stat_summary( aes( group=1 ), fun.y=mean, geom="point" ) +
  facet_grid( gen~. ) +
  theme_bw() +
  scale_x_continuous( name="Grade", breaks=5:8 ) +
  scale_y_continuous( name="Reading" ) +
  opts( "aspect.ratio"=1 )

ggplot( data=mpls.l, aes( x=grade, y=read, group=subid ) ) +
  geom_line( lty=3 ) +
  stat_summary( aes( group=1 ), fun.y=mean, geom="line" ) +
  stat_summary( aes( group=1 ), fun.y=mean, geom="point" ) +
  facet_grid( .~gen ) +
  theme_bw() +
  scale_x_continuous( name="Grade", breaks=5:8 ) +
  scale_y_continuous( name="Reading" ) +
  opts( "aspect.ratio"=1 )

ggplot( data=mpls.l, aes( x=grade, y=read, group=subid ) ) +
  geom_line( lty=3 ) +
  stat_summary( aes( group=1 ), fun.y=mean, geom="line" ) +
  stat_summary( aes( group=1 ), fun.y=mean, geom="point" ) +
  facet_grid( gen~eth ) +
  theme_bw() +
  scale_x_continuous( name="Grade", breaks=5:8 ) +
  scale_y_continuous( name="Reading" ) +
  opts( "aspect.ratio"=1 )



###################################################
### Faceting
###################################################
ggplot( data=mpls.l, aes( x=grade, y=read ) ) +
  stat_summary( aes( line=gen ), fun.y=mean, geom="line" ) +
  stat_summary( aes( shape=gen ), fun.y=mean, geom="point" ) +
  facet_grid( .~eth ) +
  theme_bw() +
  scale_x_continuous( name="Grade", breaks=5:8 ) +
  scale_y_continuous( name="Reading" ) +
  opts( "aspect.ratio"=1 )



###################################################
### Quantitative Predictors
###################################################
set.seed( 123 )
x <- rnorm( n=100, sd=15 )
table( cut_interval( x, n=4 ) )
table( cut_number( x, n=4 ) )

#two attendance group
table( cut_number( mpls.l$att, n=2 ) )
mpls.l$att2 <- table( cut_number( mpls$att, n=2 ) )
levels( mpls.l$att2 )
levels( mpls.l$att2 ) <- c( "Low Attendance", "High Attendance" )

ggplot( data=mpls.l, aes( x=grade, y=read, group=subid ) ) +
  geom_line( lty=3 ) +
  stat_summary( aes( group=1 ), fun.y=mean, geom="line" ) +
  stat_summary( aes( group=1 ), fun.y=mean, geom="point" ) +
  facet_grid( .~att2 ) +
  theme_bw() +
  scale_x_continuous( name="Grade", breaks=5:8 ) +
  scale_y_continuous( name="Reading" ) +
  opts( "aspect.ratio"=1 )

#four attendance groups
table( cut_number( mpls.l$att, n=4 ) )
mpls.l$att4 <- table( cut_number( mpls$att, n=4 ) )
levels( mpls.l$att4 )
levels( mpls.l$att4 ) <- c( "Attend 1", "Attend 2", "Attend 3", "Attend 4" )

ggplot( data=mpls.l, aes( x=grade, y=read, group=subid ) ) +
  geom_line( lty=3 ) +
  stat_summary( aes( group=1 ), fun.y=mean, geom="line" ) +
  stat_summary( aes( group=1 ), fun.y=mean, geom="point" ) +
  facet_grid( .~att4 ) +
  theme_bw() +
  scale_x_continuous( name="Grade", breaks=5:8 ) +
  scale_y_continuous( name="Reading" ) +
  opts( "aspect.ratio"=1 )




