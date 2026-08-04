##########################################################################
# Permutation test based on the difference of means
##########################################################################
rm(list = ls())
p.val <- vector()
for (j in 1:1000){
  print(j)
# Simulated samples (Same distribution)
set.seed(j)
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

# approximate p-value
p.val[j] <- mean(TT>TO)
}

hist(p.val, breaks=30)
