library(lavaan)
library(plotly)
library(RColorBrewer)
set.seed(28022017)
nsim <- 500
results <- matrix(NA,nrow=21,ncol=nsim)
for(i in 1:nsim){
lambda <- c(1,1,1,1,1,1)
theta <- c(5,5,5,5,5,5)
n <- 5000
eta <- c(10,10)
psi <- c(5,5,1)

covf1f2 <- rnorm(n,0,psi[3])
f1 <- rnorm(n,eta[1],sqrt(psi[1])) + covf1f2
f2 <- rnorm(n,eta[2],sqrt(psi[2])) + covf1f2
#factor1 and 2

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


y <- 3 + 1.7 * f1 + 2.7 * f2 + rnorm(n)

results[,i] <- rbind(
      summary(lm(y~1+EC[,1]+EC[,2]))$coefficients[1,1],
      summary(lm(y~1+EC[,1]+EC[,2]))$coefficients[1,2],
      summary(lm(y~1+EC[,1]+EC[,2]))$coefficients[2,1],
      summary(lm(y~1+EC[,1]+EC[,2]))$coefficients[2,2],
      summary(lm(y~1+EC[,1]+EC[,2]))$coefficients[3,1],
      summary(lm(y~1+EC[,1]+EC[,2]))$coefficients[3,2],
      summary(lm(y~1+EC[,1]+EC[,2]))$r.squared,
      summary(lm(y~1+ASf1+ASf2))$coefficients[1,1],
      summary(lm(y~1+ASf1+ASf2))$coefficients[1,2],
      summary(lm(y~1+ASf1+ASf2))$coefficients[2,1],
      summary(lm(y~1+ASf1+ASf2))$coefficients[2,2],
      summary(lm(y~1+ASf1+ASf2))$coefficients[3,1],
      summary(lm(y~1+ASf1+ASf2))$coefficients[3,2],      
      summary(lm(y~1+ASf1+ASf2))$r.squared,
      summary(lm(y~1+f1+f2))$coefficients[1,1],
      summary(lm(y~1+f1+f2))$coefficients[1,2],
      summary(lm(y~1+f1+f2))$coefficients[2,1],
      summary(lm(y~1+f1+f2))$coefficients[2,2],
      summary(lm(y~1+f1+f2))$coefficients[3,1],
      summary(lm(y~1+f1+f2))$coefficients[3,2],
      summary(lm(y~1+f1+f2))$r.squared
     )

}

rownames(results) <- c("EC.b0 est","EC.b0 se","EC.b1 est","EC.b1 se","EC.b2 est","EC.b2 se","EC R^2",
                       "AS.b0 est","AS.b0 se","AS.b1 est","AS.b1 se","AS.b2 est","AS.b2 se","AS R^2",
                       "f1.b0 est","f1.b0 se","f1.b1 est","f1.b1 se","f1.b2 est","f1.b2 se","f1 R^2")
#result <- as.data.frame(t(results))
#head(result)
#library(vioplot)
#vioplot(result[,1],result[,6],names=c("EC b0 est","AS b0 est"))
#abline(h=7,col="gray",lty=2)
#vioplot(result[,3],result[,8],names=c("EC b1 est","AS b1 est"))
#abline(h=7.7,col="gray",lty=2)
round(rowMeans(results),2)




range <- seq(-10,35,by=1)
xas <- range
yas <- range
zas <- matrix(NA,nrow=length(range),ncol=length(range))
for(i in 1:length(range)){
  for(ii in 1:length(range)){
    zas[i,ii] <- 3+1.7*xas[i] + 2.7*yas[ii]    
  }
}

zasEC <- matrix(NA,nrow=length(range),ncol=length(range))
for(i in 1:length(range)){
  for(ii in 1:length(range)){
    zasEC[i,ii] <- round(rowMeans(results),2)[1]+round(rowMeans(results),2)[3]*xas[i] + round(rowMeans(results),2)[5]*yas[ii]    
  }
}

zasAS <- matrix(NA,nrow=length(range),ncol=length(range))
for(i in 1:length(range)){
  for(ii in 1:length(range)){
    zasAS[i,ii] <- round(rowMeans(results),2)[8]+round(rowMeans(results),2)[10]*xas[i] + round(rowMeans(results),2)[12]*yas[ii]    
  }
}




p <- plot_ly(showscale = F) %>%
  add_surface(z = ~zas,opacity=.98,colorscale = list(c(0,1),c("rgb(107,184,214)","rgb(0,90,124)"))) %>%
  add_surface(z = ~zasAS, opacity = .8,colorscale = list(c(0,1),c("rgb(255,112,184)","rgb(128,0,64)"))) %>%
  layout(scene = list(xaxis = list(title="Factor 1"),yaxis=list(title="Factor 2"),
                      zaxis=list(title="Outcome")))

p



