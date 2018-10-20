library(sm)

opar <- par(no.readonly = TRUE)
par(mfrow=c(2, 1))
d <- density(mtcars$mpg)

plot(d)

d <- density(mtcars$mpg)

plot(d, main='Kernel Density of Miles Per Gallon')
polygon(d, col='red', border='blue')
rug(mtcars$mpg, col='brown')
par(opar)

# comparable kernel density plot
attach(mtcars)

cyl.f <- factor(cyl, levels=c(4,6,8), 
                labels=c('4 cylinder', '6 cylinder', 
                         '8 cylinder'))
sm.density.compare(mpg, cyl, xlab='Miles Per Gallon')
title(main='MPG Distribution by Car Cylinders')
colfill <- c(2:(1+length(levels(cyl.f))))
legend(locator(1), levels(cyl.f), fill=colfil)


