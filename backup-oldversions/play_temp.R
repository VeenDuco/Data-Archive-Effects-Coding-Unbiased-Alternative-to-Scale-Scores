#install.packages("lavaan")
#install.packages("GPArotation")
library(lavaan)
library(psych) 
library(GPArotation)
# initial settings for the CFA
set.seed(1)
n=10000
eta=c(9,10) # factor means
psi=c(5,5,1) # unique factor variances(1&2) and additional covariance(3)
lambda = c(.5,1,1.5,1,1,1) # factor loadings, first 3 for f1, 4-6 f2
theta = c(10,10,10,10,10,10) # residual variances for each manifest variable

# creating (co)variances
covf1f2 <- rnorm(n,0,psi[3])
f1 <- rnorm(n,eta[1],sqrt(psi[1])) + covf1f2
f2 <- rnorm(n,eta[2],sqrt(psi[2])) + covf1f2

#seeting factor loadings
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

x1 <- f1*lambda11 + theta11 #+ rnorm(n,3,3) #delete first hastag to add aditional unobserved information
x2 <- f1*lambda21 + theta22 #+ rnorm(n,-3,.5)  #delete first hastag to add aditional unobserved information
x3 <- f1*lambda31 + theta33 
x4 <- f2*lambda42 + theta44
x5 <- f2*lambda52 + theta55
x6 <- f2*lambda62 + theta66
#create manifest variables

data_sim1 <- cbind(x1,x2,x3,x4,x5,x6) # bind manifest variables in one data set
#"observed" data

# model syntax
model.syntax <- '
f1 =~ lambda11*x1 + lambda21*x2 + lambda31*x3 
#factor 1 defined
lambda11 == 3 - lambda21 - lambda31
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
  out_EC_free <- lavaan(model = model.syntax, data = data_sim1)
# estimate model
  summary(out_EC_free) # get summary information for the model. 
  

  #fa(data_sim1,nfactors = 2)
  fac3 <- fa(data_sim1,nfacotrs = 3,rotate = "oblimin",fm="minres")
    print(fac3$loadings)
  pca2 <- principal(data_sim1,nfactors=2)
  print(pca2$loadings)
  pca3 <- principal(data_sim1,nfactors=3)
  print(pca3$loadings)
  ## adding or not adding covariance
  
  
  
  # model syntax
  model.syntax2 <- '
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
  out_EC_free2 <- lavaan(model = model.syntax2, data = data_sim1)
  # estimate model
  summary(out_EC_free2) # get summary information for the model. 
  
  
  
  cbind(partable(out_EC_free)[,c(2:4,14,15)],
        rbind(partable(out_EC_free2)[1:8,c(2:4,14,15)],NA,partable(out_EC_free2)[9:26,c(2:4,14,15)],
              NA,partable(out_EC_free2)[27:34,c(2:4,14,15)]))
  
  
  modindices(out_EC_free)
  #estimate model
   EC <- predict(out_EC)
  #get predicted data based on model
  
  ASf1 <- rowMeans(data_sim1[,1:3])
  ASf2 <- rowMeans(data_sim1[,4:6])
  #get mean scores based on factors
  