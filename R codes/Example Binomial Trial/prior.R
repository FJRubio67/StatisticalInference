rm(list=ls())

##############################################################################
# Priors
##############################################################################

# Testing the parameters for the informative prior
b0 <- 5
a0 <- 3*b0

pbeta(0.9, a0, b0) - pbeta(0.6, a0, b0)

# Priors
prior.inf <- Vectorize(function(t) dbeta(t,a0,b0)) # informative
prior.j <- Vectorize(function(t) dbeta(t,0.5,0.5)) # Jeffreys
prior.u <- Vectorize(function(t) dbeta(t,1,1)) # uniform

# Plots of the priors
curve(prior.inf, 0,1, n= 1000, lwd = 2, cex.axis = 1.5, xlab= ~theta, ylab = "Density", main = "Priors", col = "black", cex.lab = 1.5)
curve(prior.j, 0,1, n= 1000, lwd = 2, cex.axis = 1.5, xlab= ~theta, ylab = "Density", main = "Priors", col = "red", add = T)
curve(prior.u, 0,1, n= 1000, lwd = 2, cex.axis = 1.5, xlab= ~theta, ylab = "Density", main = "Priors", col = "blue", add = T)
# Add a legend
legend(0.35, 4.25, legend=c("Informative", "Jeffreys", "Uniform"),
       col=c("black","red", "blue"), lty=1, cex=1)



##############################################################################
# Posteriors 
##############################################################################

# Testing the parameters for the informative prior
n <- 10
x <- 3

# Priors
posterior.inf <- Vectorize(function(t) dbeta(t,x+a0,n-x+b0)) # informative
posterior.j <- Vectorize(function(t) dbeta(t,x+0.5,n-x+0.5)) # Jeffreys
posterior.u <- Vectorize(function(t) dbeta(t,x+1,n-x+1)) # uniform

# Plots of the priors
curve(posterior.inf, 0,1, n= 1000, lwd = 2, cex.axis = 1.5, xlab= ~theta, ylab = "Density", main = "Posteriors", col = "black", cex.lab = 1.5)
curve(posterior.j, 0,1, n= 1000, lwd = 2, cex.axis = 1.5, xlab= ~theta, ylab = "Density", main = "Posteriors", col = "red", add = T)
curve(posterior.u, 0,1, n= 1000, lwd = 2, cex.axis = 1.5, xlab= ~theta, ylab = "Density", main = "Posteriors", col = "blue", add = T)
# Add a legend
legend(0.75, 4.5, legend=c("Informative", "Jeffreys", "Uniform"),
       col=c("black","red", "blue"), lty=1, cex=1)



##############################################################################
# Posterior Means and MLE 
##############################################################################
library(knitr)

# Mean and variance of the Beta distribution
# a, b : shape parameters
MeanBeta <- function(a,b) a/(a+b)
VarianceBeta <- function(a,b) (a*b)/((a+b)^2*(a+b+1))

MLE <- x/n
PM.inf <- MeanBeta(x+a0,n-x+b0)
PM.j <- MeanBeta(x+0.5,n-x+0.5)
PM.u <- MeanBeta(x+1,n-x+1)

Estimators <- c(MLE, PM.inf, PM.j, PM.u)
Variances <- c(x*(n-x)/(n^3), VarianceBeta(x+a0,n-x+b0), VarianceBeta(0.5+x,n-x+0.5), VarianceBeta(1+x,n-x+1))
tab <- rbind(Estimators,Variances)
colnames(tab) <- c("Frequentist", "Informative", "Jeffreys", "Uniform")
kable(tab)


##############################################################################
# Predictive distributions
##############################################################################

# Beta Binomial Predictive distribution function
BetaBinom <- Vectorize(function(xp){
  log.val <- lchoose(np, xp) + lbeta(xp+a+x,b+n-x+np-xp) - lbeta(a+x,b+n-x)
  return(exp(log.val))
})

# Frequentist
np <- 100
plot(0:100,dbinom(0:100, size = np, prob = MLE),type="b",xlab="r*",ylab="P(R=r*|Data)", main = "Frequentist predictive",cex.axis= 1.5,cex.lab=1.5,lwd=4)


# Informative
n <- 10; x <- 3; np <- 100; a <- a0; b <- b0;
plot(0:np,BetaBinom(0:np),type="b",xlab="r*",ylab="P(R=r*|Data)", main = "Posterior predictive: a0=15, b0=5",cex.axis= 1.5,cex.lab=1.5,lwd=4)

# Jeffreys
n <- 10; x <- 3; np <- 100; a <- 0.5; b <- 0.5;
plot(0:np,BetaBinom(0:np),type="b",xlab="r*",ylab="P(R=r*|Data)", main = "Posterior predictive: a0=0.5, b0=0.5",cex.axis= 1.5,cex.lab=1.5,lwd=4)

# Uniform
n <- 10; x <- 3; np <- 100; a <- 1; b <- 1;
plot(0:np,BetaBinom(0:np),type="b",xlab="r*",ylab="P(R=r*|Data)", main = "Posterior predictive: a0=1, b0=1",cex.axis= 1.5,cex.lab=1.5,lwd=4)


# Overplot
np <- 100
plot(0:100,dbinom(0:100, size = np, prob = MLE),type="b",xlab="r*",ylab="P(R=r*|Data)", main = "Predictive",cex.axis= 1.5,cex.lab=1.5,lwd=4)
n <- 10; x <- 3; np <- 100; a <- a0; b <- b0;
points(0:np,BetaBinom(0:np),type="b", col = "red")
n <- 10; x <- 3; np <- 100; a <- 0.5; b <- 0.5;
points(0:np,BetaBinom(0:np),type="b", col = "blue")
n <- 10; x <- 3; np <- 100; a <- 1; b <- 1;
points(0:np,BetaBinom(0:np),type="b",col = "green")
# Add a legend
legend(60,0.08,legend=c("Frequentist","Informative", "Jeffreys", "Uniform"),
       col=c("black","red", "blue", "green"), lty=1, cex=1)



##############################################################################
# Credible intervals 
##############################################################################
library(knitr)

freq.CI <- c( MLE - 1.96*sqrt(x*(n-x)/(n^3)), MLE + 1.96*sqrt(x*(n-x)/(n^3)) )
Q.inf <- c(qbeta(0.025,x+a0,n-x+b0), qbeta(0.975,x+a0,n-x+b0))
Q.j <- c(qbeta(0.025,x+0.5,n-x+0.5),qbeta(0.975,x+0.5,n-x+0.5))
Q.u <- c(qbeta(0.025,x+1,n-x+1),qbeta(0.975,x+1,n-x+1))

Intervals <- cbind(freq.CI, Q.inf, Q.j, Q.u)
colnames(Intervals) <- c("Frequentist", "Informative", "Jeffreys", "Uniform")
rownames(Intervals) <- c("L","U")
kable(Intervals)
