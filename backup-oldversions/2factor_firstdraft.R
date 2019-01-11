library(lavaan)
n <- 1000
lambda <- 1
theta <- 0
psi <- c(5,10)
eta <- c(5,10)

shared.error <- rnorm(n,0,0)
# if you want covariance
f1 <- rnorm(n,psi[1],eta[1]) + shared.error
f2 <- rnorm(n,psi[2],eta[2]) + shared.error

x1 <- f1 * 1 + rnorm(n,0,theta)
x2 <- f1 * 1 + rnorm(n,0,theta)
x3 <- f1 * 1 + rnorm(n,0,theta)
x4 <- f2 * 1 + rnorm(n,0,theta)
x5 <- f2 * 1 + rnorm(n,0,theta)
x6 <- f2 * 1 + rnorm(n,0,theta)

data <- cbind(x1,x2,x3,x4,x5,x6)
cov(f1,f2)
cov(data)



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


  x1 ~~ rv1*x1
  x2 ~~ rv2*x2
  x3 ~~ rv3*x3
  x4 ~~ rv4*x4
  x5 ~~ rv5*x5
  x6 ~~ rv6*x6
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



out <- lavaan(model = model.syntax,data=data)
summary(out)
fitted(out)

ASf1 <- rowMeans(data[,1:3])
mean(ASf1)
var(ASf1)
ASf2 <- rowMeans(data[,4:6])
mean(ASf2)
var(ASf2)



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
f1 ~~ v3*f2


x1 ~~ rv1*x1
x2 ~~ rv2*x2
x3 ~~ rv3*x3
x4 ~~ rv4*x4
x5 ~~ rv5*x5
x6 ~~ rv6*x6
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



out2 <- lavaan(model = model.syntax2,data=data)
summary(out2)

















model.syntaxAS <- '
  f1 =~ c1*x1 + c2*x2 + c3*x3 
c1==1
c2==1
c3==1


f2 =~ c4*x4 + c5*x5 + c6*x6 
#factor 2 defined
c4 == 1
c5 == 1
c6 ==1


f1 ~~ v1*f1
v1>0.001
#psy11
f2 ~~ v2*f2
v2>0.001
#psy22
f1 ~~ f2

x1 ~~ rv1*x1
x2 ~~ rv2*x2
x3 ~~ rv3*x3
x4 ~~ rv4*x4
x5 ~~ rv5*x5
x6 ~~ rv6*x6
# residusal variances
rv1==0
rv2==0
rv3==0
rv4==0
rv5==0
rv6==0


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
outas <- lavaan(model = model.syntaxAS,data=data)
summary(outas)
ASf1 <- rowMeans(data[,1:3])
mean(ASf1)
var(ASf1)
ASf2 <- rowMeans(data[,4:6])
mean(ASf2)
var(ASf2)
cov(ASf1,ASf2)

