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
#pdf("Figures/magnitude_of_bias.pdf",width=11,height=8.5)
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
        type="b",col="red")
}
for(ii in 1:length(tot_results2)){
  lines(x=c(seq(0,10,by=0.2)[ii]+.01),
        y=c(mean(tot_results2[[ii]][7, ]) / sd(tot_results2[[ii]][13, ])),
        type="b",col="blue")
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
        type="b",col="red")
}
for(ii in 1:length(tot_results2)){
  lines(x=c(seq(0,10,by=0.2)[ii]+.01),
        y=c(mean(tot_results2[[ii]][8, ]) / sd(tot_results2[[ii]][14, ])),
        type="b",col="blue")
}
abline(h=0,lty=2,col="gray") ## add 0 line

add_legend("topright", legend=c("Effects Coding","Average Scores"), lty=c(1,1), 
           col=c("red", "blue"),
           horiz=TRUE, bty='n', cex=1.5,lwd=2)