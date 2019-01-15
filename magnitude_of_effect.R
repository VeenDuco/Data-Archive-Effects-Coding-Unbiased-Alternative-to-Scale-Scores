# get packages

# load data
load("RData/Simulation1_2factor.RData")

# define functions
add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}



test.matrix <- tot_results2[[51]]
rownames(test.matrix) <- c("EC bias factor scores 1","EC bias factor scores 2",
                           "AS bias factor scores 1","AS bias factor scores 2",
                           "EC bias factor variance 1","EC bias factor variance 2",
                           "AS bias factor variance 1","AS bias factor variance 2",
                           "Bias cov EC","Bias cov AS",
                           "actual f1", "actual f2", "actual var f1",
                           "actual var f2", "actual covar f1f2")


test.matrix[5, 1] 

# bias EC var f1 & f2
mean(test.matrix[5, ])
mean(test.matrix[6, ])
# bias AS var f1 & f2
mean(test.matrix[7, ])
mean(test.matrix[8, ])
# bias EC & AS covar f1 & f2
mean(test.matrix[9, ])
mean(test.matrix[9, ])


(test.matrix[5, ] / test.matrix[13, ]) * 100
(test.matrix[7, ] / test.matrix[13, ]) * 100

# cohen's D like measure est. bias AS - EC / sd(EC)
mean((test.matrix[7, ] - test.matrix[5, ])) / sd(test.matrix[5, ])
# or True - EC / sd(True) == bias[EC] / sd(True)
mean(test.matrix[5, ]) / sd(test.matrix[13, ])
mean(test.matrix[7, ]) / sd(test.matrix[13, ])


# Bias is simulation 1
pdf("Figures/magnitude_of_bias.pdf",width=11,height=8.5)
par(mfrow=c(2,2),cex.lab=1.3, cex.axis=1.1)

### bias variance factor f1
plot(NA,NA,xlim=c(0,10),ylim=c(-100,150),
     xlab="Residual variances",ylab="Bias factor variance Factor 1 (%)",
     sub = expression(paste(italic("note"),
                            ": Bias defined as (Absolute bias / True variance) * 100"),
                      sep = ""), bty="n")
for(ii in 1:length(tot_results2)){
  lines(x=c(seq(0,10,by=0.2)[ii]-.01,seq(0,10,by=0.2)[ii]-.01),
        y=c(min( (tot_results2[[ii]][5,] / tot_results2[[ii]][13, ]) * 100 ),
            max((tot_results2[[ii]][5,] / tot_results2[[ii]][13, ]) * 100 )),
        type="l",col="red")
}
## plot residual variance versus bias factor variance f1 EC

for(ii in 1:length(tot_results2)){
  lines(x=c(seq(0,10,by=0.2)[ii]+.01,seq(0,10,by=0.2)[ii]+.01),
        y=c(min( (tot_results2[[ii]][7,] / tot_results2[[ii]][13, ]) * 100 ),
            max((tot_results2[[ii]][7,] / tot_results2[[ii]][13, ]) * 100 )),
        type="l",col="blue")
}
## plot residual variance versus bias factor variance f1 AS
abline(h=0,lty=2,col="gray") ## add 0 line

### bias variance factor f2
plot(NA,NA,xlim=c(0,10),ylim=c(-100,150),
     xlab="Residual variances",ylab="Bias factor variance Factor 2 (%)",
     sub = expression(paste(italic("note"),
                            ": Bias defined as (Absolute bias / True variance) * 100"),
                      sep = ""), bty="n")
for(ii in 1:length(tot_results2)){
  lines(x=c(seq(0,10,by=0.2)[ii]-.01,seq(0,10,by=0.2)[ii]-.01),
        y=c(min( (tot_results2[[ii]][6,] / tot_results2[[ii]][14, ]) * 100 ),
            max((tot_results2[[ii]][6,] / tot_results2[[ii]][14, ]) * 100 )),
        type="l",col="red")
}
## plot residual variance versus bias factor variance f1 EC

for(ii in 1:length(tot_results2)){
  lines(x=c(seq(0,10,by=0.2)[ii]+.01,seq(0,10,by=0.2)[ii]+.01),
        y=c(min( (tot_results2[[ii]][8,] / tot_results2[[ii]][14, ]) * 100 ),
            max((tot_results2[[ii]][8,] / tot_results2[[ii]][14, ]) * 100 )),
        type="l",col="blue")
}
## plot residual variance versus bias factor variance f1 AS
abline(h=0,lty=2,col="gray") ## add 0 line


# add_legend("topright", legend=c("Effects Coding","Average Scores"), lty=c(1,1), 
#            col=c("red", "blue"),
#            horiz=TRUE, bty='n', cex=1.5,lwd=2)
## add legend
# dev.off()


plot(NA,NA,xlim=c(0,10),ylim=c(-3,10),
     xlab="Residual variances",ylab="Effect size bias variance Factor 1",
     sub = expression(paste(italic("note"),
                            ": Effect size Bias defined as mean bias / sd(true variance factor 1)"),
                      sep = ""), bty="n")
for(ii in 1:length(tot_results2)){
  lines(x=c(seq(0,10,by=0.2)[ii]-.01),
        y=c(mean(tot_results2[[ii]][5, ]) / sd(tot_results2[[ii]][13, ])),
        type="b",col="red", pch = 3)
}
for(ii in 1:length(tot_results2)){
  lines(x=c(seq(0,10,by=0.2)[ii]+.01),
        y=c(mean(tot_results2[[ii]][7, ]) / sd(tot_results2[[ii]][13, ])),
        type="b",col="blue", pch = 4)
}
abline(h=0,lty=2,col="gray") ## add 0 line

plot(NA,NA,xlim=c(0,10),ylim=c(-3,10),
     xlab="Residual variances",ylab="Effect size bias variance Factor 2",
     sub = expression(paste(italic("note"),
                            ": Effect size Bias defined as mean bias / sd(true variance factor 2)"),
                      sep = ""), bty="n")
for(ii in 1:length(tot_results2)){
  lines(x=c(seq(0,10,by=0.2)[ii]-.01),
        y=c(mean(tot_results2[[ii]][6, ]) / sd(tot_results2[[ii]][14, ])),
        type="b",col="red", pch = 3)
}
for(ii in 1:length(tot_results2)){
  lines(x=c(seq(0,10,by=0.2)[ii]+.01),
        y=c(mean(tot_results2[[ii]][8, ]) / sd(tot_results2[[ii]][14, ])),
        type="b",col="blue", pch = 4)
}
abline(h=0,lty=2,col="gray") ## add 0 line

add_legend("topright", legend=c("Effects Coding","Scale Scoring"), lty=c(1,1), 
           col=c("red", "blue"), pch = c(3,4),
           horiz=TRUE, bty='n', cex=1.5,lwd=2)

dev.off()



# zoomed in plot - relative (mean) bias

#### zoomed in plots on N(0,1) N(0,2) N(0,4) and N(0,8)
pdf("Figures/sim1_2factor_zoomf1_magnitude.pdf",width=11,height=8.5)
par(mfrow=c(2,2),cex.lab=1.3, cex.axis=1.1, cex.sub=1.3,
    mar = c(7, 4, 7, 2))

plot(NA,NA,xlim=c(-10,10), ylim=c(-150,150),
     xlab= "Bias factor mean Factor 1 (%)",ylab="Bias factor variance Factor 1 (%)",
     bty="n",main="Residual variances N(0,1)")#,

abline(h=0,col="gray",lty=2)
abline(v=0,col="gray",lty=2)
#generate plot to set Bias factor scores against bias factor variance
points((tot_results2[[6]][1,] / tot_results2[[6]][11, ]) * 100 ,
       (tot_results2[[6]][5,] / tot_results2[[6]][13, ]) * 100 ,col="red",pch=3)
points((tot_results2[[6]][3,] / tot_results2[[6]][11, ]) * 100 ,
       (tot_results2[[6]][7,] / tot_results2[[6]][13, ]) * 100 ,pch=4, col = "blue")

plot(NA,NA,xlim=c(-10,10), ylim=c(-150,150),
     xlab= "Bias factor mean Factor 1 (%)",ylab="Bias factor variance Factor 1 (%)",
     bty="n",main="Residual variances N(0,2)")#,

abline(h=0,col="gray",lty=2)
abline(v=0,col="gray",lty=2)
#generate plot to set Bias factor scores against bias factor variance
points((tot_results2[[11]][1,] / tot_results2[[11]][11, ]) * 100 ,
       (tot_results2[[11]][5,] / tot_results2[[11]][13, ]) * 100 ,col="red",pch=3)
points((tot_results2[[11]][3,] / tot_results2[[11]][11, ]) * 100 ,
       (tot_results2[[11]][7,] / tot_results2[[11]][13, ]) * 100 ,pch=4, col = "blue")


plot(NA,NA,xlim=c(-10,10), ylim=c(-150,150),
     xlab= "Bias factor mean Factor 1 (%)",ylab="Bias factor variance Factor 1 (%)",
     bty="n",main="Residual variances N(0,4)")#,

abline(h=0,col="gray",lty=2)
abline(v=0,col="gray",lty=2)
#generate plot to set Bias factor scores against bias factor variance
points((tot_results2[[26]][1,] / tot_results2[[26]][11, ]) * 100 ,
       (tot_results2[[26]][5,] / tot_results2[[26]][13, ]) * 100 ,col="red",pch=3)
points((tot_results2[[26]][3,] / tot_results2[[26]][11, ]) * 100 ,
       (tot_results2[[26]][7,] / tot_results2[[26]][13, ]) * 100 ,pch=4, col = "blue")


plot(NA,NA,xlim=c(-10,10), ylim=c(-150,150),
     xlab= "Bias factor mean Factor 1 (%)",ylab="Bias factor variance Factor 1 (%)",
     bty="n",main="Residual variances N(0,8)")#,
    # sub = expression(paste(italic("note"),
                          # ": Effect size Bias defined as mean bias / sd(true variance factor 1)"),
                      # sep = ""))
abline(h=0,col="gray",lty=2)
abline(v=0,col="gray",lty=2)
#generate plot to set Bias factor scores against bias factor variance
points((tot_results2[[46]][1,] / tot_results2[[46]][11, ]) * 100 ,
       (tot_results2[[46]][5,] / tot_results2[[46]][13, ]) * 100 ,col="red",pch=3)
points((tot_results2[[46]][3,] / tot_results2[[46]][11, ]) * 100 ,
       (tot_results2[[46]][7,] / tot_results2[[46]][13, ]) * 100 ,pch=4, col = "blue")



add_legend("topright", legend=c("Effects Coding","Scale Scoring"), pch=c(3,4),
           col=c("red", "blue"),
           horiz=TRUE, bty='n', cex=1.5)


add_legend("bottomleft", 
           legend = expression(paste(italic("note"),
                                  ": Bias defined as (Absolute bias / True variance) * 100"),
                            sep = ""),
           horiz=TRUE, bty='n', cex=1)

dev.off()


#### zoomed in plots on N(0,1) N(0,2) N(0,4) and N(0,8)
pdf("Figures/sim1_2factor_zoomf2_magnitude.pdf",width=11,height=8.5)
par(mfrow=c(2,2),cex.lab=1.3, cex.axis=1.1, cex.sub=1.3,
    mar = c(7, 4, 7, 2))

plot(NA,NA,xlim=c(-10,10), ylim=c(-150,150),
     xlab= "Bias factor mean Factor 2 (%)",ylab="Bias factor variance Factor 2 (%)",
     bty="n",main="Residual variances N(0,1)")#,

abline(h=0,col="gray",lty=2)
abline(v=0,col="gray",lty=2)
#generate plot to set Bias factor scores against bias factor variance
points((tot_results2[[6]][2,] / tot_results2[[6]][12, ]) * 100 ,
       (tot_results2[[6]][6,] / tot_results2[[6]][14, ]) * 100 ,col="red",pch=3)
points((tot_results2[[6]][4,] / tot_results2[[6]][12, ]) * 100 ,
       (tot_results2[[6]][8,] / tot_results2[[6]][14, ]) * 100 ,pch=4, col = "blue")

plot(NA,NA,xlim=c(-10,10), ylim=c(-150,150),
     xlab= "Bias factor mean Factor 2 (%)",ylab="Bias factor variance Factor 2 (%)",
     bty="n",main="Residual variances N(0,2)")#,

abline(h=0,col="gray",lty=2)
abline(v=0,col="gray",lty=2)
#generate plot to set Bias factor scores against bias factor variance
points((tot_results2[[11]][2,] / tot_results2[[11]][12, ]) * 100 ,
       (tot_results2[[11]][6,] / tot_results2[[11]][14, ]) * 100 ,col="red",pch=3)
points((tot_results2[[11]][4,] / tot_results2[[11]][12, ]) * 100 ,
       (tot_results2[[11]][8,] / tot_results2[[11]][14, ]) * 100 ,pch=4, col = "blue")


plot(NA,NA,xlim=c(-10,10), ylim=c(-150,150),
     xlab= "Bias factor mean Factor 2 (%)",ylab="Bias factor variance Factor 2 (%)",
     bty="n",main="Residual variances N(0,4)")#,

abline(h=0,col="gray",lty=2)
abline(v=0,col="gray",lty=2)
#generate plot to set Bias factor scores against bias factor variance
points((tot_results2[[26]][2,] / tot_results2[[26]][12, ]) * 100 ,
       (tot_results2[[26]][6,] / tot_results2[[26]][14, ]) * 100 ,col="red",pch=3)
points((tot_results2[[26]][4,] / tot_results2[[26]][12, ]) * 100 ,
       (tot_results2[[26]][8,] / tot_results2[[26]][14, ]) * 100 ,pch=4, col = "blue")


plot(NA,NA,xlim=c(-10,10), ylim=c(-150,150),
     xlab= "Bias factor mean Factor 2 (%)",ylab="Bias factor variance Factor 2 (%)",
     bty="n",main="Residual variances N(0,8)")#,
# sub = expression(paste(italic("note"),
# ": Effect size Bias defined as mean bias / sd(true variance Factor 2)"),
# sep = ""))
abline(h=0,col="gray",lty=2)
abline(v=0,col="gray",lty=2)
#generate plot to set Bias factor scores against bias factor variance
points((tot_results2[[46]][2,] / tot_results2[[46]][12, ]) * 100 ,
       (tot_results2[[46]][6,] / tot_results2[[46]][14, ]) * 100 ,col="red",pch=3)
points((tot_results2[[46]][4,] / tot_results2[[46]][12, ]) * 100 ,
       (tot_results2[[46]][8,] / tot_results2[[46]][14, ]) * 100 ,pch=4, col = "blue")



add_legend("topright", legend=c("Effects Coding","Scale Scoring"), pch=c(3,4),
           col=c("red", "blue"),
           horiz=TRUE, bty='n', cex=1.5)


add_legend("bottomleft", 
           legend = expression(paste(italic("note"),
                                     ": Bias defined as (Absolute bias / True variance) * 100"),
                               sep = ""),
           horiz=TRUE, bty='n', cex=1)

dev.off()
