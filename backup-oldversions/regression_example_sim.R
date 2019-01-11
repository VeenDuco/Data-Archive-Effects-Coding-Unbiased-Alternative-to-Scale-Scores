library(lavaan)
set.seed(28022017)
nsim <- 500
results <- matrix(NA,nrow=15,ncol=nsim)
for(i in 1:nsim){
lambda <- c(1,1,1,1)
theta <- c(5,5,5,5)
n <- 5000
alpha1 <- 10
psi11 <- 5
f1 <- rnorm(n,alpha1,sqrt(psi11))


lambda11 <- lambda[1]
lambda12 <- lambda[2]
lambda13 <- lambda[3]
lambda14 <- lambda[4]
#factor loadings

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

f1 ~ m1 * 1
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

average <- rowMeans(data_sim1)
pred.ec <- predict(out_EC)


y <- 7 + 7.7 * f1 + rnorm(n)

results[,i] <- rbind(summary(lm(y~1+pred.ec))$coefficients[1,1],
      summary(lm(y~1+pred.ec))$coefficients[1,2],
      summary(lm(y~1+pred.ec))$coefficients[2,1],
      summary(lm(y~1+pred.ec))$coefficients[2,2],
      summary(lm(y~1+pred.ec))$r.squared,
      summary(lm(y~1+average))$coefficients[1,1],
      summary(lm(y~1+average))$coefficients[1,2],
      summary(lm(y~1+average))$coefficients[2,1],
      summary(lm(y~1+average))$coefficients[2,2],
      summary(lm(y~1+average))$r.squared,
      summary(lm(y~1+f1))$coefficients[1,1],
      summary(lm(y~1+f1))$coefficients[1,2],
      summary(lm(y~1+f1))$coefficients[2,1],
      summary(lm(y~1+f1))$coefficients[2,2],
      summary(lm(y~1+f1))$r.squared
     )

}

rownames(results) <- c("EC.b0 est","EC.b0 se","EC.b1 est","EC.b1 se","EC R^2",
                       "AS.b0 est","AS.b0 se","AS.b1 est","AS.b1 se","AS R^2",
                       "f1.b0 est","f1.b0 se","f1.b1 est","f1.b1 se","f1 R^2")
#result <- as.data.frame(t(results))
#head(result)
#library(vioplot)
#vioplot(result[,1],result[,6],names=c("EC b0 est","AS b0 est"))
#abline(h=7,col="gray",lty=2)
#vioplot(result[,3],result[,8],names=c("EC b1 est","AS b1 est"))
#abline(h=7.7,col="gray",lty=2)
round(rowMeans(results),2)

add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

pdf("regression.pdf",width=11,height=6)
plot(NA,NA,xlim = c(0,25),ylim = c(0,150),bty="n",ylab="Outcome regression",xlab="Factor score")
abline(a=7,b=7.7,col="darkgray",lwd=5,lty=5)
abline(a=round(rowMeans(results),2)[1],b=,round(rowMeans(results),2)[3],col="red",lty=2,lwd=3)
abline(a=round(rowMeans(results),2)[6],b=,round(rowMeans(results),2)[8],col="blue",lty=3,lwd=3)
segments(x0=10,y0=0,x1=10,y1=84,lty=3,col="gray",lwd=2)
segments(x0=0,y0=84,x1=10,y1=84,lty=3,col="gray",lwd=2)
add_legend("topright", legend=c("True model","Effects Coding","Average Scores"), lty=c(5,2,3), 
           col=c("gray","red", "blue"),
           horiz=TRUE, bty='n', cex=1.5,lwd=3)
dev.off()
