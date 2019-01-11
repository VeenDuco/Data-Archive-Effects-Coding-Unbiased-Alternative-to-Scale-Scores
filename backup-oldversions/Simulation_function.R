#n = sample size, alpha1 = mean factor score, psi11 = factor variance
#lambda = vector of factor loadings, theta = vector of residual variances (mean 0).
#plot = "TRUE" produces plot
library(lavaan)
sim_1factor4observed <- function(n=100,alpha1=50,psi11=100,lambda = c(1,1,1,1), theta = c(100,100,100,100),plot="FALSE"){
  
  f1 <- rnorm(n,alpha1,sqrt(psi11))
  #factor1
  #alpha1 == 50, psy11 == 100 (sd=10)
  
  lambda11 <- lambda[1]
  lambda12 <- lambda[2]
  lambda13 <- lambda[3]
  lambda14 <- lambda[4]
  #factor loadings
  theta[which(theta==0)] <- .001
  theta11 <- rnorm(n,0,sqrt(theta[1]))
  theta22 <- rnorm(n,0,sqrt(theta[2]))
  theta33 <- rnorm(n,0,sqrt(theta[3]))
  theta44 <- rnorm(n,0,sqrt(theta[4]))
  #residual variances
  
  x1 <- f1*lambda11 + theta11
  x2 <- f1*lambda12 + theta22
  x3 <- f1*lambda13 + theta33
  x4 <- f1*lambda14 + theta44
  #create observerd variables
  
  data_sim1 <- cbind(x1,x2,x3,x4)
  #"observed" data
  
  model.syntax <- '
  f1 =~ c1*x1 + c2*x2 + c3*x3 + c4*x4
  #factor 1 defined
  c1 == 4 - c2 - c3 -c4
  #todd little parametrization
  
  f1 ~~ v1*f1
  v1>0.001
  #psy11
  
  x1 ~~ rv1*x1
  x2 ~~ rv2*x2
  x3 ~~ rv3*x3
  x4 ~~ rv4*x4
  # residusal variances
  
  f1 ~ 1
  #factor mean - eta1
  
  x1 ~ i1*1
  x2 ~ i2*1
  x3 ~ i3*1
  x4 ~ i4*1
  i1 == 0 - i2 - i3 - i4
  # intercepts average at 0 
  
  '
  ## model syntax
  out_EC <- lavaan(model = model.syntax, data = data_sim1)
  #estimate model
  EC <- predict(out_EC)
  #get predicted data based on model
  
  AS <- rowMeans(data_sim1)
  #get mean scores based on factors
  
  if(plot=="TRUE"){
    plot(EC,f1,col="blue",xlim=c(min(c(EC,AS)),max(c(EC,AS))),ylim=c(min(f1),max(f1)),xlab= "Estimates",ylab="True factor scores",bty= "n")
    lines(seq(min(c(EC,AS,f1),max(c(EC,AS,f1)))),seq(min(c(EC,AS,f1),max(c(EC,AS,f1)))),type="l")
    points(AS,f1,col="red")
    legend("topleft",c("Effects Coding (EC)", "Average Score (AS)"),col=c("blue","red"),pch=1,bty= "n")  
  }
  bias_EC <- mean(f1-EC)
  bias_AS <- mean(f1-AS)
  bias_var_EC <- out_EC@ParTable$est[which(out_EC@ParTable$label=="v1")] - var(f1)
  bias_var_AS <- var(AS) - var(f1)
  correlation_AS_EC <- cor(EC,AS)
  
  output <- matrix(c(bias_EC,bias_AS,bias_var_EC,bias_var_AS,correlation_AS_EC),nrow=5,
                   dimnames=list(c("EC bias factor scores","AS bias factor scores","EC bias factor variance","AS bias factor variance","Correlation factor and AS"),
                                 c("")))
  
  return(output)
  
}

