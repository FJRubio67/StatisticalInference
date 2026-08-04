 bid <- c(1,5,10,20,30,40,50,75,100,150,200)
 n <- c(31,29,27,25,23,21,19,17,15,15,15)
 y <- c(0,3,6,7,9,13,17,12,11,14,13)
 m1 <- glm(y/n~log(bid),weights=n,family=binomial)
 m1

 confint(m1)

confint(m1,level=.99)

 # Compute profile log-likelihood function for beta_2
 # logLik is a built-in R function to compute log-likelihood of model
 k <- 200
 b2 <- seq(.7,2,length=k)
 w <- rep(0,k)
 for(i in 1:k){
   mm <- glm(y/n~1,offset=b2[i]*log(bid),weights=n,family=binomial(logit))
   w[i] <- logLik(mm)
   }
 plot(b2,w,type="l",ylab="Profile log-likelihood",cex.lab=1.3,cex.axis=1.3)
 abline(h=logLik(m1)-qchisq(.95,1)/2,lty=2)
 
  f <- function(b2,n,x,y,maxloglik){
    mm <- glm(y/n~1,offset=b2*x,weights=n,family=binomial)
    logLik(mm) - maxloglik + qchisq(.95,1)/2
    }
 
 uniroot(f,c(.8,1.1),n=n,x=log(bid),y=y,maxloglik=logLik(m1))
 
 uniroot(f,c(1.5,1.8),n=n,x=log(bid),y=y,maxloglik=logLik(m1))