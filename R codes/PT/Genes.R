#############################################################
# Permutation test for H0: Median Type I = Median Type II
#############################################################
rm(list=ls())
T1 <- c(230, -1350, -1580, -400, -760) # Type I 
T2 <- c(970, 110, -50, -190, -200)    # Type II 
Z <- c(T1,T2)
n <- length(T1)
m <- length(T2)
N <- length(Z)

# Number of permutations
K = 10000

# Test statistic
TS <- function(A,B) abs(median(A) - median(B))

# Test statistic for the observed sample
TO <- TS(T1,T2)

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
