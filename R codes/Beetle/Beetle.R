# Delete Memory
rm(list=ls())

# Required package
library(knitr)

# Beetle data
dat =  cbind(c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113,1.8369, 1.861, 1.8839), 
             c(59, 60, 62, 56, 63, 59, 62, 60), 
             c(6, 13, 18, 28, 52, 53, 61, 60))

# Naming the columns
colnames(dat) <- c("Dose", "N","x")

# Displaying the data
kable(dat, digits = 4)


ns = length(dat[,1]) # number of responses
ni <- dat[,2] # numbers of beetles
yi <- dat[,3] # numbers of dead beetles after exposure
xi <- dat[,1] # Dose

###############################################################################################################
# Maximum Likelihood Estimation using three methods
###############################################################################################################

#--------------------------
# Using  Newton's method
#--------------------------
theta = c(0,0) # Initial value

N = 100 # Number of iterations 

# Running the iterations
for(i in 1:N){
  pi <- exp(theta[1] + theta[2]*xi)/( 1 + exp(theta[1] + theta[2]*xi) )
  S <- c( sum(yi - ni*pi), sum(xi*(yi - ni*pi)))
  I <- rbind( c(sum(ni*pi*(1-pi)), sum(ni*xi*pi*(1-pi)) ), c(sum(ni*xi*pi*(1-pi)), sum(ni*xi^2*pi*(1-pi)) ) )
  theta <- theta + solve(I)%*%S
}

# MLE after 100 iterations
theta

# LD50
-theta[1]/theta[2]

#--------------------------
# Direct implementation
#--------------------------

# log-likelihood function
lpl = function(par){
  var <- vector()
  for(i in 1:ns) var[i] = dat[i,3]*log(plogis(par[1]+par[2]*dat[i,1])) + (dat[i,2]-dat[i,3])*log(1-plogis(par[1]+par[2]*dat[i,1]))
  return(-sum(var))
}

# Optimisation step using optim
OPT <- optim(c(-60,30),lpl,control=list(maxit=20000,abstol=1e-18,reltol=1e-18))
# MLE
OPT$par

#--------------------------
# Using glm
#--------------------------
glm(cbind(yi,ni-yi) ~ xi, family=binomial(logit), data=data.frame(dat))


###############################################################################################################
# Visualisation of the fitted dose-response model
###############################################################################################################

# Proportions
propi <- yi/ni

# Fitted logistic model
fit.logis <- Vectorize(function(d)  plogis(theta[1] + theta[2]*d))

curve(fit.logis,1.6,2, xlab = "Dose", ylab = "Proportion", main = "Fitted Model", lwd = 2, cex.axis = 1.5, cex.lab = 1.5)
points(xi,propi,pch=10,col="red",lwd=2)
