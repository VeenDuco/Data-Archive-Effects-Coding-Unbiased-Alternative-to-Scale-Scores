#n = sample size, eta = mean factor scores, psi = c(unique factor1 variance, unique factor2 variance, factor covariance)
## note that total variace for factor 1 is unique variance factor 1 + factor covariance 
## note that total variace for factor 2 is unique variance factor 2 + factor covariance 
#lambda = vector of factor loadings in order(l11,l21,l31,l42,l52,l62), theta = vector of residual variances (mean 0).
#note that if no covariance is modelled, half of the time the covariance will be negative between f1 and f2
# this means that we do not may have a positive-definite covariance matrix and we cannot estimate the EC model. 
# any residual variance in the model will cause a positive definite cov matrix. 
# no cov and no resid variance will lead to non positive definite cov matrix. hence line:   theta[which(theta==0)] <- .001
# smaller value than .001 leads to some non positive definite cov matrixes. 
library(lavaan)
sim_2factor3observed <- function(n=100,eta=c(10,10),psi=c(5,5,0),lambda = c(1,1,1,1,1,1), theta = c(10,10,10,10,10,10)){
  
  covf1f2 <- rnorm(n,0,psi[3])
  f1 <- rnorm(n,eta[1],sqrt(psi[1])) + covf1f2
  f2 <- rnorm(n,eta[2],sqrt(psi[2])) + covf1f2
  #factor1
  #alpha1 == 50, psy11 == 100 (sd=10)
  
  lambda11 <- lambda[1]
  lambda21 <- lambda[2]
  lambda31 <- lambda[3]
  lambda42 <- lambda[4]
  lambda52 <- lambda[5]
  lambda62 <- lambda[6]
  #factor loadings
  theta[which(theta==0)] <- .001
  theta11 <- rnorm(n,0,sqrt(theta[1]))
  theta22 <- rnorm(n,0,sqrt(theta[2]))
  theta33 <- rnorm(n,0,sqrt(theta[3]))
  theta44 <- rnorm(n,0,sqrt(theta[4]))
  theta55 <- rnorm(n,0,sqrt(theta[5]))
  theta66 <- rnorm(n,0,sqrt(theta[6]))
  #residual variances
  
  x1 <- f1*lambda11 + theta11
  x2 <- f1*lambda21 + theta22
  x3 <- f1*lambda31 + theta33
  x4 <- f2*lambda42 + theta44
  x5 <- f2*lambda52 + theta55
  x6 <- f2*lambda62 + theta66
  #create observerd variables
  
  data_sim1 <- cbind(x1,x2,x3,x4,x5,x6)
  #"observed" data
  
  model.syntax <- '
   f1 =~ c1*x1 + c2*x2 + c3*x3 
#factor 1 defined
  c1 == 3 - c2 - c3
  #todd little parametrization
  
  f2 =~ c4*x4 + c5*x5 + c6*x6 
  #factor 2 defined
  c4 == 3 - c5 - c6
  #todd little parametrization
  
  f1 ~~ v1*f1
  v1>0.001
  #psy11
  f2 ~~ v2*f2
  v2>0.001
  #psy22
  f1 ~~ v3*f2
  v3>.001
  
  x1 ~~ rv1*x1
  x2 ~~ rv2*x2
  x3 ~~ rv3*x3
  x4 ~~ rv4*x4
  x5 ~~ rv5*x5
  x6 ~~ rv6*x6
  rv1>.001
  rv2>.001
  rv3>.001
  rv4>.001
  rv5>.001
  rv6>.001
  # residusal variances
  
  f1 ~ 1
  f2 ~ 1
  #factor mean - eta1
  
  x1 ~ i1*1
  x2 ~ i2*1
  x3 ~ i3*1
  x4 ~ i4*1
  x5 ~ i5*1
  x6 ~ i6*1
  i1 == 0 - i2 - i3
  i4 == 0 - i5 - i6
  # intercepts average at 0 
  
  '
  ## model syntax
  out_EC <- lavaan(model = model.syntax, data = data_sim1)
  #estimate model
  EC <- predict(out_EC)
  #get predicted data based on model
  
  ASf1 <- rowMeans(data_sim1[,1:3])
  ASf2 <- rowMeans(data_sim1[,4:6])
  #get mean scores based on factors
  
 
  bias_ECf1 <- mean(f1-EC[,1])
  bias_ECf2 <- mean(f2-EC[,2])  
  bias_ASf1 <- mean(f1-ASf1)
  bias_ASf2 <- mean(f2-ASf2)  
  bias_var_ECf1 <- out_EC@ParTable$est[which(out_EC@ParTable$label=="v1")] - var(f1)
  bias_var_ECf2 <- out_EC@ParTable$est[which(out_EC@ParTable$label=="v2")] - var(f2)
  bias_var_ASf1 <- var(ASf1) - var(f1)
  bias_var_ASf2 <- var(ASf2) - var(f2)
  bias_covar_EC <- out_EC@ParTable$est[which(out_EC@ParTable$label=="v3")] - cov(f1,f2)
  bias_covar_AS <- cov(ASf1,ASf2) - cov(f1,f2)
  actual_f1     <- mean(f1)
  actual_f2     <- mean(f2)
  actual_var_f1 <- var(f1)
  actual_var_f2 <- var(f2)
  actual_covar  <- cov(f1,f2)
  
  output <- matrix(c(bias_ECf1,bias_ECf2,bias_ASf1,bias_ASf2,bias_var_ECf1,bias_var_ECf2,
                     bias_var_ASf1,bias_var_ASf2,bias_covar_EC,bias_covar_AS,
                     actual_f1, actual_f2, actual_var_f1, actual_var_f2, actual_covar),nrow=15,
                   dimnames=list(c("EC bias factor scores 1","EC bias factor scores 2",
                                   "AS bias factor scores 1","AS bias factor scores 2",
                                   "EC bias factor variance 1","EC bias factor variance 2",
                                   "AS bias factor variance 1","AS bias factor variance 2",
                                   "Bias cov EC","Bias cov AS",
                                   "actual f1", "actual f2", "actual var f1",
                                   "actual var f2", "actual covar f1f2"),
                                 c("")))
  
  return(output)
  
}
sim_2factor3observed(theta=rep(0,6))
