################################################################
# Sensitivity Curve for the sample mean
################################################################
rm(list=ls())
# Sample sizes
ns <- 1000

set.seed(1234) 
# Simulated data
data <- rnorm(ns, mean = 0, sd = 1)
# mean of the simulated data
mean(data)

SC <- Vectorize( function(x) x - mean(data) )
curve(SC,-5,5, lwd = 2, n = 1000, col = "red", cex.axis = 1.5, cex.lab = 1.5,
      xlab = "x", ylab = "SC", main = "Sensitivy curve for the sample mean")

################################################################
# Sensitivity Curve for the sample median
################################################################
rm(list=ls())
# Sample sizes
ns <- 1000
M <- 1000


# Monte Carlo approximation for M iterations
SC <- Vectorize(function(x){
  # Monte Carlo approximation
  SC.V <- vector()
  for(i in 1:M){
    # Simulated data
    set.seed(i)
    data <- rnorm(ns, mean = 0, sd = 1)
    med <- median(data)
    SC.temp <- Vectorize( function(x) (ns+1)*(median(c(data,x)) - med ) )
    SC.V[i] = SC.temp(x)
  }
  return(mean(SC.V))
})


curve(SC,-5,5, lwd = 2, n = 1000, col = "red", cex.axis = 1.5, cex.lab = 1.5,
      xlab = "x", ylab = "SC", main = "Sensitivy curve for the sample median")

# Evaluated at x = 1
SC(1)
# Theoretical Limit
1/(2*dnorm(qnorm(0.5)))