##########################################################################
# Permutation test based on the difference of means
##########################################################################
rm(list = ls())

# Simulated samples (Same distribution)
set.seed(123)
X <- rnorm(n=100, mean = 0, sd = 1)
Y <- rnorm(n=100, mean = 0, sd = 1)
Z <- c(X,Y)
n <- length(X)
m <- length(Y)
N <- length(Z)

# Number of permutations
K = 10000

# Test statistic
TS <- function(A,B) abs(mean(A) - mean(B))

# Test statistic for the observed sample
TO <- TS(X,Y)

# Vector of test statistics for each permutation
TT <- vector()

# Permutation test
for(i in 1:K){
 set.seed(i)
 Z.pi <- sample(Z, N, replace = FALSE)
 TT[i] <- TS(Z.pi[1:n], Z.pi[(n+1):(n+m)])
}

# Visualising the permuted test statistics
hist(TT)
abline(v = TO, lwd = 2, col = "red")
box()

# approximate p-value
mean(TT>TO)

##########################################################################
# Permutation test based on the difference of means
##########################################################################
rm(list = ls())

# Simulated samples (Different mean)
set.seed(123)
X <- rnorm(n=100, mean = 0, sd = 1)
Y <- rnorm(n=100, mean = 1, sd = 1)
Z <- c(X,Y)
n <- length(X)
m <- length(Y)
N <- length(Z)

# Number of permutations
K = 10000

# Test statistic
TS <- function(A,B) abs(mean(A) - mean(B))

# Test statistic for the observed sample
TO <- TS(X,Y)

# Vector of test statistics for each permutation
TT <- vector()

# Permutation test
for(i in 1:K){
  set.seed(i)
  Z.pi <- sample(Z, N, replace = FALSE)
  TT[i] <- TS(Z.pi[1:n], Z.pi[(n+1):(n+m)])
}

# Visualising the permuted test statistics
hist(TT)
abline(v = TO, lwd = 2, col = "red")
box()

# approximate p-value
mean(TT>TO)




##########################################################################
# Permutation test based on the difference of means
##########################################################################
rm(list = ls())


# Simulated samples (Different SD)
set.seed(123)
X <- rnorm(n=100, mean = 0, sd = 1)
Y <- rnorm(n=100, mean = 0, sd = 2)
Z <- c(X,Y)
n <- length(X)
m <- length(Y)
N <- length(Z)

# Number of permutations
K = 10000

# Test statistic
TS <- function(A,B) abs(mean(A) - mean(B))

# Test statistic for the observed sample
TO <- TS(X,Y)

# Vector of test statistics for each permutation
TT <- vector()

# Permutation test
for(i in 1:K){
  set.seed(i)
  Z.pi <- sample(Z, N, replace = FALSE)
  TT[i] <- TS(Z.pi[1:n], Z.pi[(n+1):(n+m)])
}

# Visualising the permuted test statistics
hist(TT)
abline(v = TO, lwd = 2, col = "red")
box()

# approximate p-value
mean(TT>TO)


#----------------------------------------------------------------------------------------------




##########################################################################
#Test based on the maximum distance between empirical distributions
##########################################################################
rm(list = ls())
# Simulated samples (Same distribution)
set.seed(123)
X <- rnorm(n=100, mean = 0, sd = 1)
Y <- rnorm(n=100, mean = 0, sd = 1)
Z <- c(X,Y)
n <- length(X)
m <- length(Y)
N <- length(Z)

# Number of permutations
K = 10000

# Test statistic
TS <- function(A,B) ks.test(A,B)$statistic

# Test statistic for the observed sample
TO <- TS(X,Y)

# Vector of test statistics for each permutation
TT <- vector()

# Permutation test
for(i in 1:K){
  set.seed(i)
  Z.pi <- sample(Z, N, replace = FALSE)
  TT[i] <- TS(Z.pi[1:n], Z.pi[(n+1):(n+m)])
}

# Visualising the permuted test statistics
hist(TT)
abline(v = TO, lwd = 2, col = "red")
box()

# approximate p-value
mean(TT>TO)

# Direct test using the Kolmogorov distribution
ks.test(X,Y)



##########################################################################
#Test based on the maximum distance between empirical distributions
##########################################################################
rm(list = ls())
# Simulated samples (Different mean)
set.seed(123)
X <- rnorm(n=100, mean = 0, sd = 1)
Y <- rnorm(n=100, mean = 1, sd = 1)
Z <- c(X,Y)
n <- length(X)
m <- length(Y)
N <- length(Z)

# Number of permutations
K = 10000

# Test statistic
TS <- function(A,B) ks.test(A,B)$statistic

# Test statistic for the observed sample
TO <- TS(X,Y)

# Vector of test statistics for each permutation
TT <- vector()

# Permutation test
for(i in 1:K){
  set.seed(i)
  Z.pi <- sample(Z, N, replace = FALSE)
  TT[i] <- TS(Z.pi[1:n], Z.pi[(n+1):(n+m)])
}

# Visualising the permuted test statistics
hist(TT)
abline(v = TO, lwd = 2, col = "red")
box()

# approximate p-value
mean(TT>TO)

# Direct test using the Kolmogorov distribution
ks.test(X,Y)




##########################################################################
#Test based on the maximum distance between empirical distributions
##########################################################################
rm(list = ls())
# Simulated samples (Different SD)
set.seed(123)
X <- rnorm(n=100, mean = 0, sd = 1)
Y <- rnorm(n=100, mean = 0, sd = 2)
Z <- c(X,Y)
n <- length(X)
m <- length(Y)
N <- length(Z)

# Number of permutations
K = 10000

# Test statistic
TS <- function(A,B) ks.test(A,B)$statistic

# Test statistic for the observed sample
TO <- TS(X,Y)

# Vector of test statistics for each permutation
TT <- vector()

# Permutation test
for(i in 1:K){
  set.seed(i)
  Z.pi <- sample(Z, N, replace = FALSE)
  TT[i] <- TS(Z.pi[1:n], Z.pi[(n+1):(n+m)])
}

# Visualising the permuted test statistics
hist(TT)
abline(v = TO, lwd = 2, col = "red")
box()

# approximate p-value
mean(TT>TO)

# Direct test using the Kolmogorov distribution
ks.test(X,Y)
