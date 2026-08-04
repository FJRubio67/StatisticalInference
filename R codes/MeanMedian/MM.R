rm(list=ls())

# Number of samples
M <- 10000
# Sample size
ns <- 100

##############################################################################
# Data generated from a N(20,1)
##############################################################################
sim.mean <- sim.median <- vector()

for(i in 1:M){
  set.seed(i)
  data0 <- rnorm(ns, 20, 1)
  sim.mean[i] <- mean(data0)
  sim.median[i] <- median(data0)
}

# Histogram of the Means
hist(sim.mean, probability = T, xlab = "Mean", ylab = "Density", main = "Simulated Mean", breaks = 50, 
     cex.axis = 1.5, cex.lab = 1.5)
box()
abline(v = 20, lwd = 2, col = "red")

# Histogram of the Medians
hist(sim.median, probability = T, xlab = "Median", ylab = "Density", main = "Simulated Median", breaks = 50, 
     cex.axis = 1.5, cex.lab = 1.5)
box()
abline(v = 20, lwd = 2, col = "red")


# Smooth density comparison
plot(density(sim.mean), col="black", ylim = c(0,4), lwd = 2, 
     xlab = "Estimates", ylab = "Density", main = "Mean vs. Median",
     cex.axis = 1.5, cex.lab = 1.5)
points(density(sim.median), col="blue", type = "l", lwd = 2)
abline(v = 20, lwd = 2, col = "red")
legend(20.2, 4, legend=c("Mean", "Median"),
       col=c("black", "blue"), lty=1, cex=1, lwd = 2)

# Boxplot comparison
boxplot(sim.mean, sim.median, names = c("Mean","Median"), col=c('lightgray','blue'),
        xlab = "Estimates", ylab = "Density", main = "Mean vs. Median")
abline(h = 20, lwd = 2, col = "red")
legend(1.3, 20.4, legend=c("Mean", "Median"),
       col=c('lightgray','blue'), lty=1, cex=1, lwd = 2)        


##############################################################################
# Data generated from a normal distribution N(20,9)
##############################################################################
sim.mean<- sim.median <- vector()

for(i in 1:M){
  set.seed(i)
  data0 <- rnorm(ns, 20, 3)
  sim.mean[i] <- mean(data0)
  sim.median[i] <- median(data0)
}

# Histogram of the Means
hist(sim.mean, probability = T, xlab = "Mean", ylab = "Density", main = "Simulated Mean", breaks = 50, 
     cex.axis = 1.5, cex.lab = 1.5)
box()
abline(v = 20, lwd = 2, col = "red")

# Histogram of the Medians
hist(sim.median, probability = T, xlab = "Median", ylab = "Density", main = "Simulated Median", breaks = 50, 
     cex.axis = 1.5, cex.lab = 1.5)
box()
abline(v = 20, lwd = 2, col = "red")


# Smooth density comparison
plot(density(sim.mean), col="black", ylim = c(0,1.5), lwd = 2, 
     xlab = "Estimates", ylab = "Density", main = "Mean vs. Median",
     cex.axis = 1.5, cex.lab = 1.5)
points(density(sim.median), col="blue", type = "l", lwd = 2)
abline(v = 20, lwd = 2, col = "red")
legend(20.2, 4, legend=c("Mean", "Median"),
       col=c("black", "blue"), lty=1, cex=1, lwd = 2)

# Boxplot comparison
boxplot(sim.mean, sim.median, names = c("Mean","Median"), col=c('lightgray','blue'),
        xlab = "Estimates", ylab = "Density", main = "Mean vs. Median")
abline(h = 20, lwd = 2, col = "red")
legend(1.3, 21, legend=c("Mean", "Median"),
       col=c('lightgray','blue'), lty=1, cex=1, lwd = 2)    


#####################################################################################################
# Data simulated from a contaminated normal data
# 95% simulated from a N(20,1) and 5% simulated from a t distribution with 2 df and location 20
#####################################################################################################

sim.mean <- sim.median <- vector()

for(i in 1:M){
  set.seed(i)
  data0 <- c(rnorm(ns*0.95, 20, 1), 20+rt(ns*0.05, df = 2) )
  sim.mean[i] <- mean(data0)
  sim.median[i] <- median(data0)
}

# Histogram of the Means
hist(sim.mean, probability = T, xlab = "Mean", ylab = "Density", main = "Simulated Mean", breaks = 50, 
     cex.axis = 1.5, cex.lab = 1.5)
box()
abline(v = 20, lwd = 2, col = "red")

# Proportion of estimates smaller than 19.75 and greater than 20.25
mean(sim.mean > 20.25) + mean(sim.mean < 19.75)


# Histogram of the Medians
hist(sim.median, probability = T, xlab = "Median", ylab = "Density", main = "Simulated Median", breaks = 50, 
     cex.axis = 1.5, cex.lab = 1.5)
box()
abline(v = 20, lwd = 2, col = "red")


# Smooth density comparison
plot(density(sim.mean), col="black", ylim = c(0,4), lwd = 2, 
     xlab = "Estimates", ylab = "Density", main = "Mean vs. Median",
     cex.axis = 1.5, cex.lab = 1.5)
points(density(sim.median), col="blue", type = "l", lwd = 2)
abline(v = 20, lwd = 2, col = "red")
legend(20.2, 4, legend=c("Mean", "Median"),
       col=c("black", "blue"), lty=1, cex=1, lwd = 2)

# Boxplot comparison
boxplot(sim.mean, sim.median, names = c("Mean","Median"), col=c('lightgray','blue'),
        xlab = "Estimates", ylab = "Density", main = "Mean vs. Median")
abline(h = 20, lwd = 2, col = "red")
legend(1.3, 24, legend=c("Mean", "Median"),
       col=c('lightgray','blue'), lty=1, cex=1, lwd = 2) 

###################################################################################################
# Data simulated from a contaminated normal data
# 90% simulated from a N(20,1) and 10% simulated from a t distribution with 2 df
###################################################################################################


sim.mean <- sim.median <- vector()

for(i in 1:M){
  set.seed(i)
  data0 <- c(rnorm(ns*0.90, 20, 1), 20+rt(ns*0.10, df = 2) )
  sim.mean[i] <- mean(data0)
  sim.median[i] <- median(data0)
}

# Histogram of the Means
hist(sim.mean, probability = T, xlab = "Mean", ylab = "Density", main = "Simulated Mean", breaks = 50, 
     cex.axis = 1.5, cex.lab = 1.5)
box()
abline(v = 20, lwd = 2, col = "red")


# Histogram of the Medians
hist(sim.median, probability = T, xlab = "Median", ylab = "Density", main = "Simulated Median", breaks = 50, 
     cex.axis = 1.5, cex.lab = 1.5)
box()
abline(v = 20, lwd = 2, col = "red")


# Smooth density comparison
plot(density(sim.mean), col="black", ylim = c(0,4), lwd = 2, 
     xlab = "Estimates", ylab = "Density", main = "Mean vs. Median",
     cex.axis = 1.5, cex.lab = 1.5)
points(density(sim.median), col="blue", type = "l", lwd = 2)
abline(v = 20, lwd = 2, col = "red")
legend(20.2, 4, legend=c("Mean", "Median"),
       col=c("black", "blue"), lty=1, cex=1, lwd = 2)

# Boxplot comparison
boxplot(sim.mean, sim.median, names = c("Mean","Median"), col=c('lightgray','blue'),
        xlab = "Estimates", ylab = "Density", main = "Mean vs. Median")
abline(h = 20, lwd = 2, col = "red")
legend(1.3, 25, legend=c("Mean", "Median"),
       col=c('lightgray','blue'), lty=1, cex=1, lwd = 2)
