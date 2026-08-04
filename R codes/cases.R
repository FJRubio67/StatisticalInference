rm(list-ls())

par(mfrow = c(2,2))

################################################################
# Unbiased and finite variance
################################################################

case1 <- Vectorize(function(t) dnorm(t,0,0.25))

set.seed(123)
hist(rnorm(10000, 0 , 0.25), probability = T, xlab = "Estimates" , ylab = "Density", main = "Unbiased and finite variance",
     cex.lab = 1.5, cex.axis = 1.5, xlim = c(-4,4), ylim = c(0,1.7))
curve(case1, -5, 5, lwd = 3, n=1000, add = T)
box()
abline(v=0, lwd = 3, col = "red")


################################################################
# Biased and finite variance
################################################################

case2 <- Vectorize(function(t) dnorm(t,0.1,0.25))

set.seed(123)
hist(rnorm(10000, 0.1 , 0.25), probability = T, xlab = "Estimates" , ylab = "Density", main = "Biased and finite variance",
     cex.lab = 1.5, cex.axis = 1.5, xlim = c(-4,4), ylim = c(0,1.7))
curve(case2, -5, 5, lwd = 3, n=1000, add = T)
box()
abline(v=0, lwd = 3, col = "red")


################################################################
# Unbiased and infinite variance
################################################################

case3 <- Vectorize(function(t) dt(t/0.25,df=1.5)/0.25)

set.seed(123)
hist(0.25*rt(10000, df=1.5), probability = T, xlab = "Estimates" , ylab = "Density", main = "Unbiased and infinite variance",
     cex.lab = 1.5, cex.axis = 1.5, xlim = c(-4,4), ylim = c(0,1.5), breaks = 10000)
curve(case3, -5, 5, lwd = 3, n=1000, add = T)
box()
abline(v=0, lwd = 3, col = "red")



################################################################
# Biased and infinite variance
################################################################

case4 <- Vectorize(function(t) dt((t-0.1)/0.25,df=1.5)/0.25)

set.seed(123)
hist(0.25*rt(10000, df=1.5)+0.1, probability = T, xlab = "Estimates" , ylab = "Density", main = "Biased and infinite variance",
     cex.lab = 1.5, cex.axis = 1.5, xlim = c(-4,4), ylim = c(0,1.5), breaks = 10000)
curve(case4, -5, 5, lwd = 3, n=1000, add = T)
box()
abline(v=0, lwd = 3, col = "red")

