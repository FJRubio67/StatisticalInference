# Clear memory
rm(list = ls())
# True value of the parameter
theta <- seq(0.1, 0.9, by = 0.1)
# Sample sizes
ns <- c(5,10,30,50,100,250,500,1000)
# Matrix of proportions of true inclusions
prop.mc <- matrix(0, nrow = length(theta), ncol = length(ns))
# Matrix of proportions of undefined intervals
prop.na <- matrix(0, nrow = length(theta), ncol = length(ns))
# Z_{0.975} 
Z <- qnorm(0.975)
# logit function
logit <- Vectorize(function(t)  log(t/(1-t)))

# Simulation study
N <- 20000 # Monte Carlo iterations


for(j in 1:length(theta)){
  for(k in 1:length(ns)){
    inc <- vector() # temporal vector
    for(i in 1:N){
      set.seed(i)
      x <- rbinom(n = 1, size = ns[k], prob = theta[j])
      theta.hat <- x/ns[k]
      if(theta.hat>0 & theta.hat < 1){
        L <- logit(theta.hat) - Z/( sqrt(ns[k]*theta.hat*(1-theta.hat)))
        U <- logit(theta.hat) + Z/( sqrt(ns[k]*theta.hat*(1-theta.hat)))
        true.logit <- logit(theta[j])
        inc[i] <- ifelse(L<= true.logit & U>=true.logit,1,0)
      }
      if(theta.hat==0 | theta.hat == 1){
        inc[i] <- NA
      }
    }
    prop.mc[j,k] <- mean(inc,na.rm = TRUE)
    prop.na[j,k] <- mean(is.na(inc))
  }
}

# Required package
library(knitr)

# Displaying the proportion of intervals that include the true value
colnames(prop.mc) <- ns
rownames(prop.mc) <- theta

kable(prop.mc, digits = 4)

plot(ns,prop.mc[1,],ylim=c(0.75,1), type="b",col=1, ylab = "Proportion", xlab = "Sample size", 
     main = "Empirical Coverage", xaxt = "n")
axis(1, at=ns)
for(i in 2:length(theta)){
  points(ns,prop.mc[i,],ylim=c(0.75,1), type="b",col=i)
}
abline(h = 0.95, col="red", lwd=3)

# Displaying the proportion of intervals with undefined normal approximation
colnames(prop.na) <- ns
rownames(prop.na) <- theta

kable(prop.na, digits = 4)

plot(ns,prop.na[1,],ylim=c(0,1), type="b",col=1, ylab = "Proportion", xlab = "Sample size", 
     main = "Undefined intervals", xaxt = "n")
axis(1, at=ns, cex.axis = 1)
for(i in 2:length(theta)){
  points(ns,prop.na[i,],ylim=c(0,1), type="b",col=i)
}