source("Simulation_function_2factor.R")
nsim <- 500 ## amount of simulations for each (Sub) condition
set.seed(22032018) #set seed for reproducibility
tot_results2 <- lapply(seq(0,10,by=0.2), function(x) matrix(NA, nrow=10, ncol=nsim))
#get a list to store the simulation results for nmin in for the varying condions for residual variances
for(ii in 1:length(tot_results2)){
  #for each list (residual variances)
  for(i in 1:nsim){
    #simulate nsim times the condition given residual variances theta.
    tot_results2[[ii]][,i] <- sim_2factor3observed(eta=c(10,10),psi=c(5,5,1),
                                                   lambda=c(1,1,1,1,1,1),theta=rep(seq(0,10,by=0.2)[ii],6))
  }}

warnings_tot_results2 <- warnings()

pdf("sim1_2factors.pdf",width=11,height=8.5)
par(mfrow=c(2,2),cex.lab=1.3, cex.axis=1.1)
add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

### bias variance factor f1
plot(NA,NA,xlim=c(0,10),ylim=c(-3,7),
     xlab="Residual variances",ylab="Bias factor variance Factor 1",bty="n")
for(ii in 1:length(tot_results2)){
  lines(x=c(seq(0,10,by=0.2)[ii]-.01,seq(0,10,by=0.2)[ii]-.01),
        y=c(min(tot_results2[[ii]][5,]),
            max(tot_results2[[ii]][5,])),type="l",col="red")
}
## plot residual variance versus bias factor variance f1 EC

for(ii in 1:length(tot_results2)){
  lines(x=c(seq(0,10,by=0.2)[ii]+.01,seq(0,10,by=0.2)[ii]+.01),
        y=c(min(tot_results2[[ii]][7,]),
            max(tot_results2[[ii]][7,])),type="l",col="blue")
}
## plot residual variance versus bias factor variance f1 AS
abline(h=0,lty=2,col="gray") ## add 0 line

### bias variance factor f2
plot(NA,NA,xlim=c(0,10),ylim=c(-3,7),
     xlab="Residual variances",ylab="Bias factor variance Factor 2",bty="n")
for(ii in 1:length(tot_results2)){
  lines(x=c(seq(0,10,by=0.2)[ii]-.01,seq(0,10,by=0.2)[ii]-.01),
        y=c(min(tot_results2[[ii]][6,]),
            max(tot_results2[[ii]][6,])),type="l",col="red")
}
## plot residual variance versus bias factor variance f2 EC

for(ii in 1:length(tot_results2)){
  lines(x=c(seq(0,10,by=0.2)[ii]+.01,seq(0,10,by=0.2)[ii]+.01),
        y=c(min(tot_results2[[ii]][8,]),
            max(tot_results2[[ii]][8,])),type="l",col="blue")
}
## plot residual variance versus bias factor variance f2 AS
abline(h=0,lty=2,col="gray") ## add 0 line


## bias mean f1 
plot(NA,NA,xlim=c(0,10),ylim=c(-1,1),
     xlab="Residual variances",ylab="Bias factor mean Factor 1",bty="n")
for(ii in 1:length(tot_results2)){
  lines(x=c(seq(0,10,by=0.2)[ii]-.01,seq(0,10,by=0.2)[ii]-.01),
        y=c(min(tot_results2[[ii]][1,]),
            max(tot_results2[[ii]][1,])),type="l",col="red")
}
#plot the bias range for each simulation condition for EC for f1
for(ii in 1:length(tot_results2)){
  lines(x=c(seq(0,10,by=0.2)[ii]+.01,seq(0,10,by=0.2)[ii]+.01),
        y=c(min(tot_results2[[ii]][3,]),
            max(tot_results2[[ii]][3,])),type="l",col="blue")
}
#plot the bias range for each simulation condition for AS for f1
abline(h=0,lty=2,col="gray") ## add 0 line



## bias mean f2 
plot(NA,NA,xlim=c(0,10),ylim=c(-1,1),
     xlab="Residual variances",ylab="Bias factor mean Factor 2",bty="n")
for(ii in 1:length(tot_results2)){
  lines(x=c(seq(0,10,by=0.2)[ii]-.01,seq(0,10,by=0.2)[ii]-.01),
        y=c(min(tot_results2[[ii]][2,]),
            max(tot_results2[[ii]][2,])),type="l",col="red")
}
#plot the bias range for each simulation condition for EC for f2
for(ii in 1:length(tot_results2)){
  lines(x=c(seq(0,10,by=0.2)[ii]+.01,seq(0,10,by=0.2)[ii]+.01),
        y=c(min(tot_results2[[ii]][4,]),
            max(tot_results2[[ii]][4,])),type="l",col="blue")
}
#plot the bias range for each simulation condition for AS for f2
abline(h=0,lty=2,col="gray") ## add 0 line


add_legend("topright", legend=c("Effects Coding","Average Scores"), lty=c(1,1), 
           col=c("red", "blue"),
           horiz=TRUE, bty='n', cex=1.5,lwd=2)
## add legend
dev.off()


par(mfrow=c(1,1))
## bias covariance
plot(NA,NA,xlim=c(0,10),ylim=c(-4,4),
     xlab="Residual variances",ylab="Bias factor covariance",bty="n")
for(ii in 1:length(tot_results2)){
  lines(x=c(seq(0,10,by=0.2)[ii]-.01,seq(0,10,by=0.2)[ii]-.01),
        y=c(min(tot_results2[[ii]][9]),
            max(tot_results2[[ii]][9,])),type="l",col="red")
}
#plot the bias range for each simulation condition for EC for factor cov
for(ii in 1:length(tot_results2)){
  lines(x=c(seq(0,10,by=0.2)[ii]+.01,seq(0,10,by=0.2)[ii]+.01),
        y=c(min(tot_results2[[ii]][10,]),
            max(tot_results2[[ii]][10,])),type="l",col="blue")
}
#plot the bias range for each simulation condition for AS for factor cov
abline(h=0,lty=2,col="gray") ## add 0 line
#abline(h=-1,lty=3,col="gray") ## add 0 line





#### zoomed in plots on N(0,1) N(0,2) N(0,4) and N(0,8)
pdf("sim1_2factor_zoomf1.pdf",width=11,height=8.5)
par(mfrow=c(2,2),cex.lab=1.3, cex.axis=1.1, cex.sub=1.3)

plot(NA,NA,xlim=c(-.4,.4), ylim=c(-4.5,4.5),
     xlab= "Bias factor mean Factor 1",ylab="Bias factor variance Factor 1",bty="n",sub="Residual variances N(0,1)")
abline(h=0,col="gray",lty=2)
abline(v=0,col="gray",lty=2)
#generate plot to set Bias factor scores against bias factor variance
points(tot_results2[[6]][1,],tot_results2[[6]][5,],col="red",pch=3)
points(tot_results2[[6]][3,],tot_results2[[6]][7,],col="blue",pch=4)


plot(NA,NA,xlim=c(-.4,.4), ylim=c(-4.5,4.5),
     xlab= "Bias factor mean Factor 1",ylab="Bias factor variance Factor 1",bty="n",sub="Residual variances N(0,2)")
#generate plot to set Bias factor scores against bias factor variance
#generate plot to set Bias against SS residuls
abline(h=0,col="gray",lty=2)
abline(v=0,col="gray",lty=2)
points(tot_results2[[11]][1,],tot_results2[[11]][5,],col="red",pch=3)
points(tot_results2[[11]][3,],tot_results2[[11]][7,],col="blue",pch=4)

plot(NA,NA,xlim=c(-.4,.4), ylim=c(-4.5,4.5),
     xlab= "Bias factor mean Factor 1",ylab="Bias factor variance Factor 1",bty="n",sub="Residual variances N(0,4)")
#generate plot to set Bias factor scores against bias factor variance
#generate plot to set Bias against SS residuls
abline(h=0,col="gray",lty=2)
abline(v=0,col="gray",lty=2)
points(tot_results2[[26]][1,],tot_results2[[26]][5,],col="red",pch=3)
points(tot_results2[[26]][3,],tot_results2[[26]][7,],col="blue",pch=4)


plot(NA,NA,xlim=c(-.4,.4), ylim=c(-4.5,4.5),
     xlab= "Bias factor mean Factor 1",ylab="Bias factor variance Factor 1",bty="n",sub="Residual variances N(0,8)")
#generate plot to set Bias factor scores against bias factor variance
#generate plot to set Bias against SS residuls
abline(h=0,col="gray",lty=2)
abline(v=0,col="gray",lty=2)
points(tot_results2[[46]][1,],tot_results2[[46]][5,],col="red",pch=3)
points(tot_results2[[46]][3,],tot_results2[[46]][7,],col="blue",pch=4)


add_legend("topright", legend=c("Effects Coding","Average Scores"), pch=c(3,4), 
           col=c("red", "blue"),
           horiz=TRUE, bty='n', cex=2)

dev.off()







pdf("sim1_2factor_zoomf2.pdf",width=11,height=8.5)
par(mfrow=c(2,2))

plot(NA,NA,xlim=c(-.4,.4), ylim=c(-4.5,4.5),
     xlab= "Bias factor mean Factor 2",ylab="Bias factor variance Factor 2",bty="n",sub="Residual variances N(0,1)")
abline(h=0,col="gray",lty=2)
abline(v=0,col="gray",lty=2)
#generate plot to set Bias factor scores against bias factor variance
points(tot_results2[[6]][2,],tot_results2[[6]][6,],col="red",pch=3)
points(tot_results2[[6]][4,],tot_results2[[6]][8,],col="blue",pch=4)

plot(NA,NA,xlim=c(-.4,.4), ylim=c(-4.5,4.5),
     xlab= "Bias factor mean Factor 2",ylab="Bias factor variance Factor 2",bty="n",sub="Residual variances N(0,2)")
#generate plot to set Bias factor scores against bias factor variance
#generate plot to set Bias against SS residuls
abline(h=0,col="gray",lty=2)
abline(v=0,col="gray",lty=2)
points(tot_results2[[11]][2,],tot_results2[[11]][6,],col="red",pch=3)
points(tot_results2[[11]][4,],tot_results2[[11]][8,],col="blue",pch=4)

plot(NA,NA,xlim=c(-.4,.4), ylim=c(-4.5,4.5),
     xlab= "Bias factor mean Factor 2",ylab="Bias factor variance Factor 2",bty="n",sub="Residual variances N(0,4)")
#generate plot to set Bias factor scores against bias factor variance
#generate plot to set Bias against SS residuls
abline(h=0,col="gray",lty=2)
abline(v=0,col="gray",lty=2)
points(tot_results2[[26]][2,],tot_results2[[26]][6,],col="red",pch=3)
points(tot_results2[[26]][4,],tot_results2[[26]][8,],col="blue",pch=4)

plot(NA,NA,xlim=c(-.4,.4), ylim=c(-4.5,4.5),
     xlab= "Bias factor mean Factor 2",ylab="Bias factor variance Factor 2",bty="n",sub="Residual variances N(0,8)")
#generate plot to set Bias factor scores against bias factor variance
#generate plot to set Bias against SS residuls
abline(h=0,col="gray",lty=2)
abline(v=0,col="gray",lty=2)
points(tot_results2[[46]][2,],tot_results2[[46]][6,],col="red",pch=3)
points(tot_results2[[46]][4,],tot_results2[[46]][8,],col="blue",pch=4)


add_legend("topright", legend=c("Effects Coding","Average Scores"), pch=c(3,4), 
           col=c("red", "blue"),
           horiz=TRUE, bty='n', cex=2)

dev.off()


save.image("C:/Users/5507553/surfdrive/PhD werk/Projecten/simulation factor and sum scores/Data Archive/Simulations_EC_AS/Simulation1_2factor.RData")