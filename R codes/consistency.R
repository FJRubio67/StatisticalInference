m=100
mean_samples=function(n=10){
     X=matrix(rnorm(n*m),nrow=m,ncol=n)
     return(rowMeans(X))
   }
 B=matrix(NA,100,100)
 for(i in 1:100){
     B[,i]=mean_samples(i*10)
   }
 colnames(B)=as.character(seq(10,1000,by=10))
 boxplot(B,cex.axis=2)
 abline(h=0,col="red",lwd=2)

# u=seq(0,21,by=.2)
# v=sqrt(u*10)
# lines(u,1.96/v,col="red")
# lines(u,-1.96/v,col="red")