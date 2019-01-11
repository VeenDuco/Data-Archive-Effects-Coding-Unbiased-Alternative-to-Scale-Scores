library(lavaan)
set.seed(13031990)
nsim <- 500

lgc.sim <- function(nsim){
  
  results.true <- matrix(NA,nrow=20,ncol=nsim)
  results.AS.est <- matrix(NA,nrow=20,ncol=nsim)
  results.AS.se <- matrix(NA,nrow=20,ncol=nsim)
  results.EC.est <- matrix(NA,nrow=20,ncol=nsim)
  results.EC.se <- matrix(NA,nrow=20,ncol=nsim)
  results.f1.est <- matrix(NA,nrow=20,ncol=nsim)
  results.f1.se <- matrix(NA,nrow=20,ncol=nsim)
  results.1stage.est <- matrix(NA,nrow=20,ncol=nsim)
  results.1stage.se <- matrix(NA,nrow=20,ncol=nsim)
  
  for(i in 1:nsim){
    n <- 10000
    
    I <- rnorm(n,7,sqrt(3))
    S <- rnorm(n,1,sqrt(.25))
    
    e_lgc_x1 <- rnorm(n,0,sqrt(10))
    e_lgc_x2 <- rnorm(n,0,sqrt(10))
    e_lgc_x3 <- rnorm(n,0,sqrt(10))
    e_lgc_x4 <- rnorm(n,0,sqrt(10))
    
    lgc_x1 <- 1 * I + 1 * S + e_lgc_x1
    lgc_x2 <- 1 * I + 3 * S + e_lgc_x2
    lgc_x3 <- 1 * I + 6 * S + e_lgc_x3
    lgc_x4 <- 1 * I + 12* S + e_lgc_x4
    
    B0_f1 <- rnorm(n,10,sqrt(10))
    f1 <-  B0_f1 + 7*I + 7.7*S 
    
    
    e_cfa_x1 <- rnorm(n,0,sqrt(25))
    e_cfa_x2 <- rnorm(n,0,sqrt(25))
    e_cfa_x3 <- rnorm(n,0,sqrt(25))
    e_cfa_x4 <- rnorm(n,0,sqrt(25))
    
    
    cfa_x1 <- f1*1 + e_cfa_x1
    cfa_x2 <- f1*1 + e_cfa_x2
    cfa_x3 <- f1*1 + e_cfa_x3
    cfa_x4 <- f1*1 + e_cfa_x4
    
    data_cfa <- cbind(cfa_x1,cfa_x2,cfa_x3,cfa_x4)
    
    
    
    #### CFA part predicting:
    
model.cfa.ec <- '
# factor 1 and 2 defined
f1 =~ l11 * cfa.x5 + l21 * cfa.x6 + l31 * cfa.x7 
f2 =~ l42 * cfa.x8 + l52 * cfa.x9 + l62 * cfa.x10 
l11 == 3 - l21 - l31
l42 == 3 - l52 - l62

# factor (co) variances
f1 ~~ p33 * f1
f2 ~~ p44 * f2
f1 ~~ p43 * f2
p33 > 0.001
p44 > 0.001
p43 > 0.001

# residual variances
cfa.x5  ~~ t55   * cfa_x5
cfa.x6  ~~ t66   * cfa.x6
cfa.x7  ~~ t77   * cfa.x7
cfa.x8  ~~ t88   * cfa.x8
cfa.x9  ~~ t99   * cfa.x9
cfa.x10 ~~ t1010 * cfa.x10
t55   > 0.001
t66   > 0.001
t77   > 0.001
t88   > 0.001
t99   > 0.001
t1010 > 0.001

#factor means
f1 ~ e3 * 1
f1 ~ e4 * 1

cfa_x1 ~ i1*1
cfa_x2 ~ i2*1
cfa_x3 ~ i3*1
cfa_x4 ~ i4*1
i1 == 0 - i2 - i3 - i4
# intercepts average at 0 

'
    ## model syntax
    out_EC <- lavaan(model = model.syntax, data = data_cfa)
    #estimate model
    EC <- predict(out_EC)
    #get predicted data based on model
    
    AS <- rowMeans(data_cfa)
    #get mean scores based on factors
    
    #plot(EC,f1,col="blue")
    #points(AS,f1,col="Red")
    
    
    ################ LGCM part
    
    data_lgc <- cbind(lgc_x1,lgc_x2,lgc_x3,lgc_x4,AS,EC,f1)
    colnames(data_lgc) <- c("lgc_x1","lgc_x2","lgc_x3","lgc_x4","AS","EC","f1")
    
    
    model.lgc_f1 <- '
Int =~ 1*lgc_x1 + 1*lgc_x2 + 1* lgc_x3 + 1*lgc_x4
Slope =~ 1*lgc_x1 + 3*lgc_x2 + 6* lgc_x3 + 12 * lgc_x4
Int ~ 1
Slope ~1
Int ~~ Int
Slope ~~ Slope
lgc_x1 ~~ lgc_x1
lgc_x2 ~~ lgc_x2
lgc_x3 ~~ lgc_x3
lgc_x4 ~~ lgc_x4
f1 ~ 1+Int + Slope
f1 ~~ f1

'
    lgc_f1 <- lavaan(model = model.lgc_f1,data=data_lgc)
    #summary(lgc_f1)
    
    
    model.lgc_AS <- '
Int =~ 1*lgc_x1 + 1*lgc_x2 + 1* lgc_x3 + 1*lgc_x4
Slope =~ 1*lgc_x1 + 3*lgc_x2 + 6* lgc_x3 + 12 * lgc_x4
Int ~ 1
Slope ~1
Int ~~ Int
Slope ~~ Slope
lgc_x1 ~~ lgc_x1
lgc_x2 ~~ lgc_x2
lgc_x3 ~~ lgc_x3
lgc_x4 ~~ lgc_x4
AS ~ 1+Int + Slope
AS ~~ AS

'
    lgc_AS <- lavaan(model = model.lgc_AS,data=data_lgc)
    #summary(lgc_AS)
    
    
    model.lgc_EC <- '
Int =~ 1*lgc_x1 + 1*lgc_x2 + 1* lgc_x3 + 1*lgc_x4
Slope =~ 1*lgc_x1 + 3*lgc_x2 + 6* lgc_x3 + 12 * lgc_x4
Int ~ 1
Slope ~1
Int ~~ Int
Slope ~~ Slope
lgc_x1 ~~ lgc_x1
lgc_x2 ~~ lgc_x2
lgc_x3 ~~ lgc_x3
lgc_x4 ~~ lgc_x4
EC ~ 1+Int + Slope
EC ~~ EC

'
    lgc_EC <- lavaan(model = model.lgc_EC,data=data_lgc)
    #summary(lgc_EC)

    
    ## one stage:
    model.syntax.1stage <- '
    f1 =~ c1*cfa_x1 + c2*cfa_x2 + c3*cfa_x3 + c4*cfa_x4
    #factor 1 defined
    c1 == 4 - c2 - c3 -c4
    #todd little parametrization
    
    f1 ~~ v1*f1
    #psy11
    
    cfa_x1 ~~ rv1*cfa_x1
    cfa_x2 ~~ rv2*cfa_x2
    cfa_x3 ~~ rv3*cfa_x3
    cfa_x4 ~~ rv4*cfa_x4
    rv1>0.001
    rv2>0.001
    rv3>0.001
    rv4>0.001
    # residusal variances
    
    cfa_x1 ~ i1*1
    cfa_x2 ~ i2*1
    cfa_x3 ~ i3*1
    cfa_x4 ~ i4*1
    i1 == 0 - i2 - i3 - i4
    # intercepts average at 0 
    
    
    Int =~ 1*lgc_x1 + 1*lgc_x2 + 1* lgc_x3 + 1*lgc_x4
    Slope =~ 1*lgc_x1 + 3*lgc_x2 + 6* lgc_x3 + 12 * lgc_x4
    Int ~ 1
    Slope ~1
    Int ~~ Int
    Slope ~~ Slope
    lgc_x1 ~~ lgc_x1
    lgc_x2 ~~ lgc_x2
    lgc_x3 ~~ lgc_x3
    lgc_x4 ~~ lgc_x4
    
    f1 ~ 1+ Int + Slope
    
    '
    lgc_1stage <- lavaan(model = model.syntax.1stage,data=cbind(data_lgc,data_cfa))    
    
        
    results.true[,i] <- c(mean(I),var(I),mean(S),var(S),
                          var(e_lgc_x1),var(e_lgc_x2),var(e_lgc_x3),var(e_lgc_x4),
                          mean(B0_f1),7,7.7,var(B0_f1),
                          var(e_cfa_x1),var(e_cfa_x2),var(e_cfa_x3),var(e_cfa_x4),1,1,1,1)
    
    
    
    results.f1.est[,i] <- c(parTable(lgc_f1)[c(9,11,10,12,13,14,15,16,17,18,19,20),14],NA,NA,NA,NA,NA,NA,NA,NA)
    results.f1.se[,i] <- c(parTable(lgc_f1)[c(9,11,10,12,13,14,15,16,17,18,19,20),15],NA,NA,NA,NA,NA,NA,NA,NA)
    
    results.AS.est[,i] <- c(as.numeric(parTable(lgc_AS)[c(9,11,10,12,13,14,15,16,17,18,19,20),14]),NA,NA,NA,NA,NA,NA,NA,NA)
    results.AS.se[,i] <- c(as.numeric(parTable(lgc_AS)[c(9,11,10,12,13,14,15,16,17,18,19,20),15]),NA,NA,NA,NA,NA,NA,NA,NA)
    
    results.EC.est[,i] <- c(as.numeric(parTable(lgc_EC)[c(9,11,10,12,13,14,15,16,17,18,19,20),14]),NA,NA,NA,NA,NA,NA,NA,NA)
    results.EC.se[,i] <- c(as.numeric(parTable(lgc_EC)[c(9,11,10,12,13,14,15,16,17,18,19,20),15]),NA,NA,NA,NA,NA,NA,NA,NA)

    results.1stage.est[,i] <- as.numeric(partable(lgc_1stage)[c(22,24,23,25,26,27,28,29,30,31,32,5,6,7,8,9,1,2,3,4),14])
    results.1stage.se[,i] <- as.numeric(partable(lgc_1stage)[c(22,24,23,25,26,27,28,29,30,31,32,5,6,7,8,9,1,2,3,4),15])
    
  }
  
  out <- cbind(rowMeans(results.true),rowMeans(results.f1.est),rowMeans(results.f1.se),
               rowMeans(results.AS.est),rowMeans(results.AS.se),
               rowMeans(results.EC.est),rowMeans(results.EC.se),
               rowMeans(results.1stage.est),rowMeans(results.1stage.se))
  rownames(out) <- c("Eta1","Psi11",
                     "Eta2","Psi22",
                     "Theta11","Theta22",
                     "Theta33","Theta44",
                     "Eta3","B1","B2","Psi33",
                     "Theta55","Theta66",
                     "Theta77","Theta88",
                     "Lambda1","Lambda2",
                     "Lambda3","Lambda4")
  colnames(out) <- c("True values","f1 est","f1 se","AS est","AS se","EC est","ES se","1stage_EC est","1stage_EC se")
  return(round(out,digits=2))
  
}

sim.lgc <- lgc.sim(nsim)
#system.time(lgc.sim(nsim)) 1 sec per 1 sim.
sim.lgc
## note parameter est. for var intercept and regression parameter slope for EC -> croon problem. 


