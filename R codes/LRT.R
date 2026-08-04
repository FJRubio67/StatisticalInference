# Size
alpha <- 0.05
# mu0
mu0 <- 0
# number of simulations
NS <- 10000 
# sample size
n <- 30 
# Quantile 1-alpha of the t distribution
z.a <- qt(1-alpha/2, df = n-1)

ind <- vector()

for(i in 1:NS){
  data <- rnorm(n, mu0, 1)
  Tn <- (mean(data) - mu0)/(sqrt(var(data)/n))
  ind[i] <- ifelse(abs(Tn)>z.a,1,0)
}

# Size
mean(ind)
