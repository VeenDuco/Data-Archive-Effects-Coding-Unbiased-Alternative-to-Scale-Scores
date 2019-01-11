source("Simulation_function.R")
nsim <- 500 ## amount of simulations for each (Sub) condition
set.seed(28022018) #set seed for reproducibility
tot_results2 <- lapply(seq(0,10,by=0.2), function(x) matrix(NA, nrow=5, ncol=nsim))
#get a list to store the simulation results for nmin in for the varying condions for residual variances
for(ii in 1:length(tot_results2)){
  #for each list (residual variances)
  for(i in 1:nsim){
    #simulate nsim times the condition given residual variances theta.
    tot_results2[[ii]][,i] <- sim_1factor4observed(alpha1=10,psi11=5,lambda=c(1,1,1,1),theta=rep(seq(0,10,by=0.2)[ii],4))
  }}

warnings_tot_results2 <- warnings()

pdf("sim1.pdf",width=11,height=8.5)
par(mfrow=c(2,1))
add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}


plot(NA,NA,xlim=c(0,10),ylim=c(-3,7),
     xlab="Residual variances",ylab="Bias factor variance",bty="n")
for(ii in 1:length(tot_results2)){
  lines(x=c(seq(0,10,by=0.2)[ii]-.01,seq(0,10,by=0.2)[ii]-.01),
        y=c(min(tot_results2[[ii]][3,]),
            max(tot_results2[[ii]][3,])),type="l",col="red")
}
#plot the SS resid range for each simulation condition for effects coding
#boxplot(lapply(tot_results2,"[",3,),xaxt="n")
#lines(abline(h=0))

for(ii in 1:length(tot_results2)){
  lines(x=c(seq(0,10,by=0.2)[ii]+.01,seq(0,10,by=0.2)[ii]+.01),
        y=c(min(tot_results2[[ii]][4,]),
            max(tot_results2[[ii]][4,])),type="l",col="blue")
}
abline(h=0,lty=2,col="gray")
#for(ii in 1:length(tot_results2)){
#  points(x=rep(c(seq(1,10,by=0.2)[ii]+.01),nsim),
#        y=c((tot_results2[[ii]][4,])),col="blue")
#}
#boxplot(lapply(tot_results2,"[",4,),xaxt="n")

#plot the SS resid range for each simulation condition for average scores

plot(NA,NA,xlim=c(0,10),ylim=c(-1,1),
     xlab="Residual variances",ylab="Bias factor mean",bty="n")
for(ii in 1:length(tot_results2)){
  lines(x=c(seq(0,10,by=0.2)[ii]-.01,seq(0,10,by=0.2)[ii]-.01),
        y=c(min(tot_results2[[ii]][1,]),
            max(tot_results2[[ii]][1,])),type="l",col="red")
}
#plot the bias range for each simulation condition for effects coding
for(ii in 1:length(tot_results2)){
  lines(x=c(seq(0,10,by=0.2)[ii]+.01,seq(0,10,by=0.2)[ii]+.01),
        y=c(min(tot_results2[[ii]][2,]),
            max(tot_results2[[ii]][2,])),type="l",col="blue")
}
#plot the bias range for each simulation condition for average scores
abline(h=0,lty=2,col="gray")
add_legend("topright", legend=c("Effects Coding","Average Scores"), lty=c(1,1), 
           col=c("red", "blue"),
           horiz=TRUE, bty='n', cex=1.5,lwd=2)

dev.off()


#### zoomed in plots on N(0,1) N(0,2) N(0,4) and N(0,8)
pdf("sim1_zoom.pdf",width=11,height=8.5)
par(mfrow=c(2,2))
add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

plot(NA,NA,xlim=c(-.4,.4), ylim=c(-4.5,4.5),
     xlab= "Bias factor mean",ylab="Bias factor variance",bty="n",sub="Residual variances N(0,1)")
abline(h=0,col="gray",lty=2)
abline(v=0,col="gray",lty=2)
#generate plot to set Bias factor scores against bias factor variance
points(tot_results2[[6]][1,],tot_results2[[6]][3,],col="red",pch=3)
points(tot_results2[[6]][2,],tot_results2[[6]][4,],col="blue",pch=4)




plot(NA,NA,xlim=c(-.4,.4), ylim=c(-4.5,4.5),
     xlab= "Bias factor mean",ylab="Bias factor variance",bty="n",sub="Residual variances N(0,2)")
#generate plot to set Bias factor scores against bias factor variance
#generate plot to set Bias against SS residuls
abline(h=0,col="gray",lty=2)
abline(v=0,col="gray",lty=2)
points(tot_results2[[11]][1,],tot_results2[[11]][3,],col="red",pch=3)
points(tot_results2[[11]][2,],tot_results2[[11]][4,],col="blue",pch=4)

plot(NA,NA,xlim=c(-.4,.4), ylim=c(-4.5,4.5),
     xlab= "Bias factor mean",ylab="Bias factor variance",bty="n",sub="Residual variances N(0,4)")
#generate plot to set Bias factor scores against bias factor variance
#generate plot to set Bias against SS residuls
abline(h=0,col="gray",lty=2)
abline(v=0,col="gray",lty=2)
points(tot_results2[[26]][1,],tot_results2[[26]][3,],col="red",pch=3)
points(tot_results2[[26]][2,],tot_results2[[26]][4,],col="blue",pch=4)

plot(NA,NA,xlim=c(-.4,.4), ylim=c(-4.5,4.5),
     xlab= "Bias factor mean",ylab="Bias factor variance",bty="n",sub="Residual variances N(0,8)")
#generate plot to set Bias factor scores against bias factor variance
#generate plot to set Bias against SS residuls
abline(h=0,col="gray",lty=2)
abline(v=0,col="gray",lty=2)
points(tot_results2[[46]][1,],tot_results2[[46]][3,],col="red",pch=3)
points(tot_results2[[46]][2,],tot_results2[[46]][4,],col="blue",pch=4)

add_legend("topright", legend=c("Effects Coding","Average Scores"), pch=c(3,4), 
           col=c("red", "blue"),
           horiz=TRUE, bty='n', cex=2)

dev.off()
save.image("C:/Users/5507553/surfdrive/PhD werk/Projecten/simulation factor and sum scores/Data Archive/Simulations_EC_AS/Simulation1.RData")