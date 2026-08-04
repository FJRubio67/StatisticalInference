###################################################################################################################
# Asymptotic Normal Confidence Intervals
###################################################################################################################

#Delete Memory
rm(list=ls())

# Required package
library(numDeriv)

###########################################################################################
# Function to calculate the normal confidence intervals
# The parameters indicated with "index" are transformed to the real line using log()
###########################################################################################
# FUN   : minus log-likelihood function to be used to calculate the confidence intervals
# MLE   : maximum likelihood estimator of the parameters of interest
# level : confidence level
# index : position of the positive parameters under the original parameterisation

Conf.Int <- function(FUN,MLE,level=0.95,index=NULL){
  sd.int <- abs(qnorm(0.5*(1-level)))
  tempf <- function(par){
    par[index] = exp(par[index])
    return(FUN( par ))
  }
  r.MLE <- MLE
  r.MLE[index] <- log(MLE[index])
  HESS <- hessian(tempf,x=r.MLE, method.args=list(eps=1e-8, d=0.000001))
  Fisher.Info <- solve(HESS)
  Sigma <- sqrt(diag(Fisher.Info))
  U<- r.MLE + sd.int*Sigma
  L<- r.MLE - sd.int*Sigma
  C.I <- cbind(L,U,r.MLE, Sigma)
  names.row <- paste0("par", seq_along(1:length(MLE)))
  names.row[index] <- paste0("log.par", seq_along(index))
  rownames(C.I)<- names.row
  colnames(C.I)<- c("Lower","Upper","Transf MLE", "Std. Error")
  return(C.I)
}


#-------------------------------------
# Example 1: Logistic Regression
# Here, all the parameters are real
#-------------------------------------


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

# log-likelihood function
lpl = function(par){
  var <- vector()
  for(i in 1:ns) var[i] =  dat[i,3]*log(plogis(par[1]+par[2]*dat[i,1])) + (dat[i,2]-dat[i,3])*log(1-plogis(par[1]+par[2]*dat[i,1]))
  return(-sum(var))
}

# Optimisation step using optim
OPT <- optim(c(-60,30),lpl,control=list(maxit=20000,abstol=1e-18,reltol=1e-18))
# MLE
MLE <- OPT$par

# Confidence interval and standard errors
CI <- Conf.Int(lpl,MLE,level=0.95)
print(kable(CI,digits=4))

# Standard errors obtained with the expressions calculated in the lecture
pi <- exp(MLE[1] + MLE[2]*xi)/( 1 + exp(MLE[1] + MLE[2]*xi) )
I <- rbind( c(sum(ni*pi*(1-pi)), sum(ni*xi*pi*(1-pi)) ), c(sum(ni*xi*pi*(1-pi)), sum(ni*xi^2*pi*(1-pi)) ) )
Fisher.Info <- solve(I)
Sigma <- sqrt(diag(Fisher.Info))
Sigma

# Double checking this calculations with glm()
glmlogis <- glm(cbind(yi,ni-yi) ~ xi, family=binomial(logit), data=data.frame(dat))
sqrt(diag(vcov(glmlogis))) # Standard errors