################################################################################################
# Profile likelihood: Binomial case
################################################################################################

# Example 1
rm(list = ls())

# Data
n <- 10
set.seed(4)
t <- rbinom(n = 1, size = n, prob = 0.3)
theta.hat <- t/n
theta.hat

# Profile likelihood
Rp <- Vectorize(function(theta)  theta^t*(1-theta)^(n-t)/( theta.hat^t*(1-theta.hat)^(n-t) ))

# Plot
curve(Rp,0,1, lwd = 2, xlab = expression(theta), ylab = "Rp",
      cex.axis = 1.5, cex.lab = 1.5, n = 1000)
abline(h = 0.147, col="red", lwd = 2)

# Finding the 0.147-level points
Rpl <- Vectorize(function(theta) Rp(theta)-0.147)

uniroot(Rpl, c(0.05,0.2))$root
uniroot(Rpl, c(0.5,0.8))$root


# Example 2
rm(list = ls())

# Data
n <- 100
set.seed(4)
t <- rbinom(n = 1, size = n, prob = 0.3)
theta.hat <- t/n
theta.hat

# Profile likelihood
Rp <- Vectorize(function(theta)  theta^t*(1-theta)^(n-t)/( theta.hat^t*(1-theta.hat)^(n-t) ))

# Plot
curve(Rp,0,1, lwd = 2, xlab = expression(theta), ylab = "Rp",
      cex.axis = 1.5, cex.lab = 1.5, n = 1000)
abline(h = 0.147, col="red", lwd = 2)

# Finding the 0.147-level points
Rpl <- Vectorize(function(theta) Rp(theta)-0.147)

uniroot(Rpl, c(0.15,0.3))$root
uniroot(Rpl, c(0.35,0.5))$root



################################################################################################
# Profile likelihood: Exponential case
################################################################################################

# Example 1
rm(list = ls())

# Data
set.seed(123)
x <- rexp(n = 10, rate = 1)
x.bar <- mean(x)
n <- length(x)
theta.hat <- x.bar
theta.hat

# Profile likelihood
Rp <- Vectorize(function(theta)  ( exp(-n*x.bar/theta)/theta^n )/( exp(-n*x.bar/theta.hat)/theta.hat^n )  )

# Plot
curve(Rp,0,2, lwd = 2, xlab = expression(theta), ylab = "Rp",
      cex.axis = 1.5, cex.lab = 1.5, n = 1000)
abline(h = 0.147, col="red", lwd = 2)

# Finding the 0.147-level points
Rpl <- Vectorize(function(theta) Rp(theta)-0.147)

uniroot(Rpl, c(0.2,0.5))$root
uniroot(Rpl, c(1,1.5))$root




# Example 2
rm(list = ls())

# Data
set.seed(123)
x <- rexp(n = 100, rate = 1)
x.bar <- mean(x)
n <- length(x)
theta.hat <- x.bar
theta.hat

# Profile likelihood
Rp <- Vectorize(function(theta)  ( exp(-n*x.bar/theta)/theta^n )/( exp(-n*x.bar/theta.hat)/theta.hat^n )  )

# Plot
curve(Rp,0,2, lwd = 2, xlab = expression(theta), ylab = "Rp",
      cex.axis = 1.5, cex.lab = 1.5, n = 1000)
abline(h = 0.147, col="red", lwd = 2)

# Finding the 0.147-level points
Rpl <- Vectorize(function(theta) Rp(theta)-0.147)

uniroot(Rpl, c(0.7,1))$root
uniroot(Rpl, c(1,1.5))$root



