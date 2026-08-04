rm(list=ls())

# Normalised Median Absolute Deviation
b <- 1/qnorm(0.75) # Normalisation constant
NMAD <- function(data)  b*median( abs(data - median(data)))
# Sample standard deviation
s.sigma <- function(data) sqrt( mean((data - mean(data))^2) )

# Number of samples
M <- 10000
# Sample size
ns <- 100

##############################################################################
# Data generated from a standard normal distribution
##############################################################################
sim.nmad <- sim.sigma <- vector()

for(i in 1:M){
  set.seed(i)
  data0 <- rnorm(ns, 0, 1)
  sim.nmad[i] <- NMAD(data0)
  sim.sigma[i] <- s.sigma(data0)
}

# Histogram of the NMADs
hist(sim.nmad, probability = T, xlab = "NMAD", ylab = "Density", main = "Simulated NMAD", breaks = 50, 
     cex.axis = 1.5, cex.lab = 1.5)
box()
abline(v = 1, lwd = 2, col = "red")

# Histogram of the SDs
hist(sim.sigma, probability = T, xlab = "SD", ylab = "Density", main = "Simulated SD", breaks = 50, 
     cex.axis = 1.5, cex.lab = 1.5)
box()
abline(v = 1, lwd = 2, col = "red")


# Smooth density comparison
plot(density(sim.nmad), col="black", ylim = c(0,6), lwd = 2, 
     xlab = "Estimates", ylab = "Density", main = "NMAD vs. SD",
     cex.axis = 1.5, cex.lab = 1.5)
points(density(sim.sigma), col="blue", type = "l", lwd = 2)
abline(v = 1, lwd = 2, col = "red")
legend(1.2, 5, legend=c("NMAD", "SD"),
       col=c("black", "blue"), lty=1, cex=1, lwd = 2)

# Boxplot comparison
boxplot(sim.nmad, sim.sigma, names = c("NMAD","SD"), col=c('lightgray','blue'),
        xlab = "Estimates", ylab = "Density", main = "NMAD vs. SD")
abline(h = 1, lwd = 2, col = "red")
legend(1.3, 1.4, legend=c("NMAD", "SD"),
       col=c('lightgray','blue'), lty=1, cex=1, lwd = 2)        


##############################################################################
# Data generated from a normal distribution with sd = 3
##############################################################################
sim.nmad <- sim.sigma <- vector()

for(i in 1:M){
  set.seed(i)
  data0 <- rnorm(ns, 0, 3)
  sim.nmad[i] <- NMAD(data0)
  sim.sigma[i] <- s.sigma(data0)
}

# Histogram of the NMADs
hist(sim.nmad, probability = T, xlab = "NMAD", ylab = "Density", main = "Simulated NMAD", breaks = 50, 
     cex.axis = 1.5, cex.lab = 1.5)
box()
abline(v = 3, lwd = 2, col = "red")

# Histogram of the SDs
hist(sim.sigma, probability = T, xlab = "SD", ylab = "Density", main = "Simulated SD", breaks = 50, 
     cex.axis = 1.5, cex.lab = 1.5)
box()
abline(v = 3, lwd = 2, col = "red")


# Smooth density comparison
plot(density(sim.nmad), col="black", ylim = c(0,2), lwd = 2, 
     xlab = "Estimates", ylab = "Density", main = "NMAD vs. SD",
     cex.axis = 1.5, cex.lab = 1.5)
points(density(sim.sigma), col="blue", type = "l", lwd = 2)
abline(v = 3, lwd = 2, col = "red")
legend(3.7, 2, legend=c("NMAD", "SD"),
       col=c("black", "blue"), lty=1, cex=1, lwd = 2)

# Boxplot comparison
boxplot(sim.nmad, sim.sigma, names = c("NMAD","SD"), col=c('lightgray','blue'),
        xlab = "Estimates", ylab = "Density", main = "NMAD vs. SD")
abline(h =3, lwd = 2, col = "red")
legend(1.3, 4.25, legend=c("NMAD", "SD"),
       col=c('lightgray','blue'), lty=1, cex=1, lwd = 2)  


#############################################################################################
# Data simulated from a contaminated normal data
# 95% simulated from a normal distribution and 5% simulated from a t distribution with 2.5 df
#############################################################################################

sim.nmad <- sim.sigma <- vector()

for(i in 1:M){
  set.seed(i)
  data0 <- c(rnorm(ns*0.95, 0, 1), rt(ns*0.05, df = 2.5) )
  sim.nmad[i] <- NMAD(data0)
  sim.sigma[i] <- s.sigma(data0)
}

# Histogram of the NMADs
hist(sim.nmad, probability = T, xlab = "NMAD", ylab = "Density", main = "Simulated NMAD", breaks = 50, 
     cex.axis = 1.5, cex.lab = 1.5)
box()
abline(v = 1, lwd = 2, col = "red")

# Proportion of estimates smaller than 1
mean(sim.nmad < 1)

# Histogram of the SDs
hist(sim.sigma, probability = T, xlab = "SD", ylab = "Density", main = "Simulated SD", breaks = 100, 
     cex.axis = 1.5, cex.lab = 1.5)
box()
abline(v = 1, lwd = 2, col = "red")

# Proportion of estimates smaller than 1
mean(sim.sigma < 1)


# Smooth density comparison
plot(density(sim.nmad), col="black", xlim = c(0.5,1.6), ylim = c(0,5), lwd = 2, 
     xlab = "Estimates", ylab = "Density", main = "NMAD vs. SD",
     cex.axis = 1.5, cex.lab = 1.5)
points(density(sim.sigma), col="blue", type = "l", lwd = 2)
abline(v = 1, lwd = 2, col = "red")
legend(1.2, 5, legend=c("NMAD", "SD"),
       col=c("black", "blue"), lty=1, cex=1, lwd = 2)


# Boxplot comparison
boxplot(sim.nmad, sim.sigma, names = c("NMAD","SD"), col=c('lightgray','blue'),
        xlab = "Estimates", ylab = "Density", main = "NMAD vs. SD")
abline(h =1, lwd = 2, col = "red")
legend(1.3, 4.25, legend=c("NMAD", "SD"),
       col=c('lightgray','blue'), lty=1, cex=1, lwd = 2)  

###################################################################################################
# Data simulated from a contaminated normal data
# 90% simulated from a normal distribution and 10% simulated from a t distribution with 2.5 df
###################################################################################################

sim.nmad <- sim.sigma <- vector()

for(i in 1:M){
  set.seed(i)
  data0 <- c(rnorm(ns*0.90, 0, 1), rt(ns*0.10, df = 2.5) )
  sim.nmad[i] <- NMAD(data0)
  sim.sigma[i] <- s.sigma(data0)
}

# Histogram of the NMADs
hist(sim.nmad, probability = T, xlab = "NMAD", ylab = "Density", main = "Simulated NMAD", breaks = 50, 
     cex.axis = 1.5, cex.lab = 1.5)
box()
abline(v = 1, lwd = 2, col = "red")

# Proportion of estimates smaller than 1
mean(sim.nmad < 1)

# Histogram of the SDs
hist(sim.sigma, probability = T, xlab = "SD", ylab = "Density", main = "Simulated SD", breaks = 100, 
     cex.axis = 1.5, cex.lab = 1.5)
box()
abline(v = 1, lwd = 2, col = "red")

# Proportion of estimates smaller than 1
mean(sim.sigma < 1)

# Smooth density comparison
plot(density(sim.nmad), col="black", xlim = c(0.5,2), ylim = c(0,5), lwd = 2, 
     xlab = "Estimates", ylab = "Density", main = "NMAD vs. SD",
     cex.axis = 1.5, cex.lab = 1.5)
points(density(sim.sigma), col="blue", type = "l", lwd = 2)
abline(v = 1, lwd = 2, col = "red")
legend(1.2, 5, legend=c("NMAD", "SD"),
       col=c("black", "blue"), lty=1, cex=1, lwd = 2)

# Boxplot comparison
boxplot(sim.nmad, sim.sigma, names = c("NMAD","SD"), col=c('lightgray','blue'),
        xlab = "Estimates", ylab = "Density", main = "NMAD vs. SD")
abline(h =1, lwd = 2, col = "red")
legend(1.3, 10, legend=c("NMAD", "SD"),
       col=c('lightgray','blue'), lty=1, cex=1, lwd = 2)  
