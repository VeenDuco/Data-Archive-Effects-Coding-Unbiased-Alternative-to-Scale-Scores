# Author comment
# No specific comments at this time
# Author: Duco Veen (d.veen@uu.nl/ducoveen@gmail.com)

# File Description Comment
# This file contains the thrid simulation study for the manuscript:
# "Effects Coding as Unbiased Alternative to Average Scores"
# In it a simulation is ran to compare a Latent growth curve model with distal outcomes
# for which the distal outcomes are two factors. We compare estimation using
# true factor scores and estimated factors scores using average scores or 
# effects coding parameterized Confirmatory Factor Analyses

# Library's required for analyses
library(lavaan)


# Function defenitions
# Function to simulate data
SimulatedData <- function (nsim, n, eta, psi, beta, lambda, theta) {
  # This function simulated data for a latent growth curve model (LGCM) with 2 distal outcomes.
  # The distal outcomes are two factor scores. The LGCM has 4 manifest variables, the two
  # factor scores have three manifest variables each. 
  #
  # Args:
  #   nsim: Number of data sets that need to be simulated
  #      n: Number of cases that are to be simulated 
  #    eta: Vector of 4 latent means 1) intercept LGCM 2) slope LGCM 3) factor 1 4) factor 2
  #    psi: Vector of 5 latent (co)variances. 1) intercept LGCM 2) slope LGCM 
  #         3) unique part factor 1 4) unique part factor 2 5) added covariance factors
  #   beta: Vector of 4 regression coefficients. 1) factor 1 on intercept 2) factor 1 on slope 
  #         3) factor 2 on intercept 4) factor 2 on slope. 
  # lambda: Vector of 6 factor loadings. 1-3) loadings for factor 1 4-6) loadings for factor 2
  #  theta: Vector of 10 residual variances. 1-4) for LGCM part 5-10) for CFA part
  #
  # Returns: 
  #   The array of manifest observations for the simulated data and true factor scores.
  
  # Error handling
  if (length(eta) != 4 | length(psi) != 5 | length(beta) != 4 | length(lambda) != 6 | length(theta)!=10) {
    stop("Incorrect dimensions for one of the arguments, check explanations of the function to see correct lengths.")
  }
  
  # Array to store simulated data in
  simulated.data <- array(NA, dim = c(n, 27, nsim))
  
  for (i in 1:nsim) {
    n <- n # sample size
    
    intercept <- rnorm(n, eta[1], sqrt(psi[1])) # intercept
    slope     <- rnorm(n, eta[2], sqrt(psi[2])) # slope
    
    error.lgcm.x1 <- rnorm(n, 0, sqrt(theta[1])) #error terms LGCM
    error.lgcm.x2 <- rnorm(n, 0, sqrt(theta[2]))
    error.lgcm.x3 <- rnorm(n, 0, sqrt(theta[3]))
    error.lgcm.x4 <- rnorm(n, 0, sqrt(theta[4]))
    
    # create manifest variables LGCM part
    lgcm.x1 <- 1 * intercept + 1  * slope + error.lgcm.x1 
    lgcm.x2 <- 1 * intercept + 3  * slope + error.lgcm.x2
    lgcm.x3 <- 1 * intercept + 6  * slope + error.lgcm.x3
    lgcm.x4 <- 1 * intercept + 12 * slope + error.lgcm.x4
    
    # factor scores unique (co) variance components
    unique.variance.f1 <- rnorm(n, eta[3], sqrt(psi[3]))
    unique.variance.f2 <- rnorm(n, eta[4], sqrt(psi[4]))
    covariance.f1.f2   <- rnorm(n, 0, sqrt(psi[5]))
    
    # create true factor scores based
    factor.1 <- beta[1] * intercept + beta[2] * slope + unique.variance.f1 + covariance.f1.f2
    factor.2 <- beta[3] * intercept + beta[4] * slope + unique.variance.f2 + covariance.f1.f2
    
    
    # errors manifest variables CFA part
    error.cfa.x5  <- rnorm(n, 0, sqrt(theta[5]))
    error.cfa.x6  <- rnorm(n, 0, sqrt(theta[6]))
    error.cfa.x7  <- rnorm(n, 0, sqrt(theta[7]))
    error.cfa.x8  <- rnorm(n, 0, sqrt(theta[8]))
    error.cfa.x9  <- rnorm(n, 0, sqrt(theta[9]))
    error.cfa.x10 <- rnorm(n, 0, sqrt(theta[10]))
    
    # create manifest variables CFA part
    cfa.x5  <- factor.1 * 1 + error.cfa.x5
    cfa.x6  <- factor.1 * 1 + error.cfa.x6
    cfa.x7  <- factor.1 * 1 + error.cfa.x7
    cfa.x8  <- factor.2 * 1 + error.cfa.x8
    cfa.x9  <- factor.2 * 1 + error.cfa.x9
    cfa.x10 <- factor.2 * 1 + error.cfa.x10
    
    # store all simulated manifest variables and true factor scors in one matrix    
    data <- cbind(lgcm.x1, lgcm.x2, lgcm.x3, lgcm.x4, 
                  cfa.x5, cfa.x6, cfa.x7, cfa.x8, cfa.x9, cfa.x10,
                  factor.1, factor.2,
                  intercept, slope, unique.variance.f1, unique.variance.f2, covariance.f1.f2,
                  error.lgcm.x1, error.lgcm.x2, error.lgcm.x3, error.lgcm.x4,
                  error.cfa.x5, error.cfa.x6, error.cfa.x7, error.cfa.x8, error.cfa.x9, error.cfa.x10)
    
    # store the matrix for simulation i in the array of simulated data
    simulated.data[, , i] <- data
  }
  colnames(simulated.data) <- c("lgcm.x1", "lgcm.x2", "lgcm.x3", "lgcm.x4", 
                                "cfa.x5", "cfa.x6", "cfa.x7", "cfa.x8", "cfa.x9", "cfa.x10",
                                "factor.1", "factor.2", "true.intercept","true.slope","true.psi33","true.psi44",
                                "true.psi43", "true.theta11", "true.theta22", "true.theta33", "true.theta44", 
                                "true.theta55", "true.theta66", "true.theta77", "true.theta88",
                                "true.theta99", "true.theta1010")
  return(simulated.data)
}

# Fuction that estimates model parameters for the different conditions

EstimationParameters <- function (true.parameters) {
  # This function estimates parameters for a latent growth curve model (LGCM) with 2 distal outcomes.
  # The distal outcomes are two factor scores. The LGCM has 4 manifest variables, the two
  # factor scores have three manifest variables each. 
  # The different methods to estimate the parameters are 1) esimating the model using true factor scores
  # as a baseline comparison 2) estimating the model using Average Scores (AS) estimates for the factor scores
  # 3) estimating the model using Effects Coding (EC) estimates for the factor scores, all estimated in 1 model
  # 4) estimating the model using Effects Coding (EC) estimates for the factor scores, with saved factor scores
  # Condition 4) is to show that this is not correct and will result in a bias that would require a croon correction.
  #
  # Args:
  # true.parameters: A list of true parameter values that are to be used to simulate the data. The parameters are:
  #                  -   nsim: Number of data sets that need to be simulated
  #                  -      n: Number of cases that are to be simulated 
  #                  -    eta: Vector of 4 latent means 1) intercept LGCM 2) slope LGCM 3) factor 1 4) factor 2
  #                  -    psi: Vector of 5 latent (co)variances. 1) intercept LGCM 2) slope LGCM 
  #                            3) unique part factor 1 4) unique part factor 2 5) added covariance factors
  #                  -   beta: Vector of 4 regression coefficients. 1) factor 1 on intercept 2) factor 1 on slope 
  #                            3) factor 2 on intercept 4) factor 2 on slope. 
  #                  - lambda: Vector of 6 factor loadings. 1-3) loadings for factor 1 4-6) loadings for factor 2
  #                  -  theta: Vector of 10 residual variances. 1-4) for LGCM part 5-10) for CFA part
  #
  # Returns: 
  #   The array of the results of the estimation using the different estimation methods.
  
  # Error handling
  if (length(true.parameters$eta) != 4 | length(true.parameters$psi) != 5 | length(true.parameters$beta) != 4 | 
      length(true.parameters$lambda) != 6 | length(true.parameters$theta)!=10) {
    stop("Incorrect dimensions for one of the arguments in the true parameters list, 
         check explanations of the function to see correct lengths.")
  }
  

  # Array to store results in
  results.output <- array(NA, dim = c(29, 9, true.parameters$nsim))
  
  # Estimating models
  for (i in 1:true.parameters$nsim) {
    
    # Get data set for this itteration
    data.i <- SimulatedData(nsim = 1, n = true.parameters$n, eta = true.parameters$eta,
                            psi = true.parameters$psi, beta = true.parameters$beta, lambda =  true.parameters$lambda,
                            theta = true.parameters$theta)[, , 1]
    
        # create matrices to store results of the analyses in. 
    results.true          <- matrix(c(mean(data.i[, "true.intercept"]), mean(data.i[, "true.slope"]),
                                      mean(data.i[, "true.psi33"]),mean(data.i[, "true.psi44"]),
                                      var(data.i[, "true.intercept"]), var(data.i[, "true.slope"]),
                                      var(data.i[, "true.psi33"] + data.i[, "true.psi43"]),
                                      var(data.i[, "true.psi44"] + data.i[, "true.psi43"]),
                                      var(data.i[, "true.psi43"]), true.parameters$beta[1],
                                      true.parameters$beta[2], true.parameters$beta[3], true.parameters$beta[4],
                                      true.parameters$lambda[1], true.parameters$lambda[2], true.parameters$lambda[3],
                                      true.parameters$lambda[4], true.parameters$lambda[5], true.parameters$lambda[6],
                                      var(data.i[, "true.theta11"]), var(data.i[, "true.theta22"]),
                                      var(data.i[, "true.theta33"]), var(data.i[, "true.theta44"]),
                                      var(data.i[, "true.theta55"]), var(data.i[, "true.theta66"]),
                                      var(data.i[, "true.theta77"]), var(data.i[, "true.theta88"]),
                                      var(data.i[, "true.theta99"]), var(data.i[, "true.theta1010"])),
                                    nrow=29, ncol=1)
    results.f1.est        <- matrix(NA, nrow=29, ncol=1)
    results.f1.se         <- matrix(NA, nrow=29, ncol=1)
    results.AS.est        <- matrix(NA, nrow=29, ncol=1)
    results.AS.se         <- matrix(NA, nrow=29, ncol=1)
    results.EC.1model.est <- matrix(NA, nrow=29, ncol=1)
    results.EC.1model.se  <- matrix(NA, nrow=29, ncol=1)
    results.EC.2step.est  <- matrix(NA, nrow=29, ncol=1)
    results.EC.2step.se   <- matrix(NA, nrow=29, ncol=1)

    # First obtaining AS estimates of factor scores
    factor1.AS <- rowMeans(data.i[, 5:7])
    factor2.AS <- rowMeans(data.i[, 8:10])
    
    # Now obtain EC estimates of factor scores for 2step EC model
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
    cfa.x5  ~~ t55   * cfa.x5
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
    
    # factor means
    f1 ~ e3 * 1
    f2 ~ e4 * 1
    
    # intercepts
    cfa.x5  ~ nu5  *1
    cfa.x6  ~ nu6  *1
    cfa.x7  ~ nu7  *1
    cfa.x8  ~ nu8  *1
    cfa.x9  ~ nu9  *1
    cfa.x10 ~ nu10 *1
    nu5 == 0 - nu6 - nu7
    nu8 == 0 - nu9 - nu10
    '
    out.cfa.EC <- lavaan(model = model.cfa.ec, data = data.i)
    # estimate model and predicted factor scores using EC
    # add stop function if we estimate unconverged models
    if(out.cfa.EC@Fit@converged == FALSE){
      stop("Unconverged model in 1st step 2 step EC estimation.")
    }  
    factor1.EC <- predict(out.cfa.EC)[,1]
    factor2.EC <- predict(out.cfa.EC)[,2]
    
    # now add the estimated factor scores to the data matrix to be used in later analyses
    data.i <- cbind(data.i, factor1.AS, factor2.AS, factor1.EC, factor2.EC)
    
    # estimating model with true factor scores as distal outcomes
    model.lgcm.f1 <- '
    int       =~ 1 * lgcm.x1 + 1 * lgcm.x2 + 1 * lgcm.x3 + 1  * lgcm.x4
    slope     =~ 1 * lgcm.x1 + 3 * lgcm.x2 + 6 * lgcm.x3 + 12 * lgcm.x4
    int       ~  e1 * 1
    slope     ~  e2 * 1
    int       ~~ p11 * int
    slope     ~~ p22 * slope
    lgcm.x1   ~~ t11 * lgcm.x1
    lgcm.x2   ~~ t22 * lgcm.x2
    lgcm.x3   ~~ t33 * lgcm.x3
    lgcm.x4   ~~ t44 * lgcm.x4
    factor.1  ~  e3 * 1 + b1 * int + b2 * slope
    factor.2  ~  e4 * 1 + b3 * int + b4 * slope
    factor.1  ~~ p33 * factor.1
    factor.2  ~~ p44 * factor.2
    factor.1  ~~ p43 * factor.2
'
    lgcm.f1 <- lavaan(model = model.lgcm.f1,data=data.i)
    # add stop function if we estimate unconverged models
    if(lgcm.f1@Fit@converged == FALSE){
      stop("Unconverged model in true factor score model estimation.")
    }  
    
    # store results of the estimation in appropriate matrices.
    results.f1.est[, 1] <- c(parTable(lgcm.f1)[c(9, 10, 17, 20, 11, 12, 23, 24, 25, 18, 19, 21, 22), 14], rep(NA, 6), 
                             parTable(lgcm.f1)[c(13, 14, 15, 16), 14], rep(NA,6))
    results.f1.se[, 1]  <- c(parTable(lgcm.f1)[c(9, 10, 17, 20, 11, 12, 23, 24, 25, 18, 19, 21, 22), 15], rep(NA, 6), 
                             parTable(lgcm.f1)[c(13, 14, 15, 16), 15], rep(NA,6))
 
    # estimating model with AS estimated factor scores as distal outcomes
    model.lgcm.AS <- '
    int         =~ 1 * lgcm.x1 + 1 * lgcm.x2 + 1 * lgcm.x3 + 1  * lgcm.x4
    slope       =~ 1 * lgcm.x1 + 3 * lgcm.x2 + 6 * lgcm.x3 + 12 * lgcm.x4
    int         ~  e1 * 1
    slope       ~  e2 * 1
    int         ~~ p11 * int
    slope       ~~ p22 * slope
    lgcm.x1     ~~ t11 * lgcm.x1
    lgcm.x2     ~~ t22 * lgcm.x2
    lgcm.x3     ~~ t33 * lgcm.x3
    lgcm.x4     ~~ t44 * lgcm.x4
    factor1.AS  ~  e3 * 1 + b1 * int + b2 * slope
    factor2.AS  ~  e4 * 1 + b3 * int + b4 * slope
    factor1.AS  ~~ p33 * factor1.AS
    factor2.AS  ~~ p44 * factor2.AS
    factor1.AS  ~~ p43 * factor2.AS
'
    lgcm.AS <- lavaan(model = model.lgcm.AS,data=data.i)
    # add stop function if we estimate unconverged models
    if(lgcm.AS@Fit@converged == FALSE){
      stop("Unconverged model in AS estimation.")
    }  
    
    
    # store results of the estimation in appropriate matrices.
    results.AS.est[, 1] <- c(parTable(lgcm.AS)[c(9, 10, 17, 20, 11, 12, 23, 24, 25, 18, 19, 21, 22), 14], rep(NA, 6), 
                             parTable(lgcm.AS)[c(13, 14, 15, 16), 14], rep(NA,6))
    results.AS.se[, 1]  <- c(parTable(lgcm.AS)[c(9, 10, 17, 20, 11, 12, 23, 24, 25, 18, 19, 21, 22), 15], rep(NA, 6), 
                             parTable(lgcm.AS)[c(13, 14, 15, 16), 15], rep(NA,6))    
    
    
    # estimating model with EC estimated factor scores as distal outcomes
    model.lgcm.EC <- '
    int         =~ 1 * lgcm.x1 + 1 * lgcm.x2 + 1 * lgcm.x3 + 1  * lgcm.x4
    slope       =~ 1 * lgcm.x1 + 3 * lgcm.x2 + 6 * lgcm.x3 + 12 * lgcm.x4
    int         ~  e1 * 1
    slope       ~  e2 * 1
    int         ~~ p11 * int
    slope       ~~ p22 * slope
    lgcm.x1     ~~ t11 * lgcm.x1
    lgcm.x2     ~~ t22 * lgcm.x2
    lgcm.x3     ~~ t33 * lgcm.x3
    lgcm.x4     ~~ t44 * lgcm.x4
    factor1.EC  ~  e3 * 1 + b1 * int + b2 * slope
    factor2.EC  ~  e4 * 1 + b3 * int + b4 * slope
    factor1.EC  ~~ p33 * factor1.EC
    factor2.EC  ~~ p44 * factor2.EC
    factor1.EC  ~~ p43 * factor2.EC
'
    lgcm.EC <- lavaan(model = model.lgcm.EC,data=data.i)
    # add stop function if we estimate unconverged models
    if(lgcm.EC@Fit@converged == FALSE){
      stop("Unconverged model in 2 stage EC estimation.")
    }  
      
    # store results of the estimation in appropriate matrices.
    results.EC.2step.est[, 1] <- c(parTable(lgcm.EC)[c(9, 10, 17, 20, 11, 12, 23, 24, 25, 18, 19, 21, 22), 14], rep(NA, 6), 
                                   parTable(lgcm.EC)[c(13, 14, 15, 16), 14], rep(NA,6))
    results.EC.2step.se[, 1]  <- c(parTable(lgcm.EC)[c(9, 10, 17, 20, 11, 12, 23, 24, 25, 18, 19, 21, 22), 15], rep(NA, 6), 
                                   parTable(lgcm.EC)[c(13, 14, 15, 16), 15], rep(NA,6))  
    
    # estimating model with EC estimated factor scores as distal outcomes 1 stage model
    model.lgcm.1stage.EC <- '
    # cfa part
    # factor 1 and 2 defined
    f1 =~ l11 * cfa.x5 + l21 * cfa.x6 + l31 * cfa.x7 
    f2 =~ l42 * cfa.x8 + l52 * cfa.x9 + l62 * cfa.x10 
    l11 == 3 - l21 - l31
    l42 == 3 - l52 - l62
    
    # residual variances
    cfa.x5  ~~ t55   * cfa.x5
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
    
    # intercepts
    cfa.x5  ~ nu5  *1
    cfa.x6  ~ nu6  *1
    cfa.x7  ~ nu7  *1
    cfa.x8  ~ nu8  *1
    cfa.x9  ~ nu9  *1
    cfa.x10 ~ nu10 *1
    nu5 == 0 - nu6 - nu7
    nu8 == 0 - nu9 - nu10

    # LGCM part
    int         =~ 1 * lgcm.x1 + 1 * lgcm.x2 + 1 * lgcm.x3 + 1  * lgcm.x4
    slope       =~ 1 * lgcm.x1 + 3 * lgcm.x2 + 6 * lgcm.x3 + 12 * lgcm.x4
    int         ~  e1 * 1
    slope       ~  e2 * 1
    int         ~~ p11 * int
    slope       ~~ p22 * slope
    lgcm.x1     ~~ t11 * lgcm.x1
    lgcm.x2     ~~ t22 * lgcm.x2
    lgcm.x3     ~~ t33 * lgcm.x3
    lgcm.x4     ~~ t44 * lgcm.x4
    f1  ~  e3 * 1 + b1 * int + b2 * slope
    f2  ~  e4 * 1 + b3 * int + b4 * slope
    f1  ~~ p33 * f1
    f2  ~~ p44 * f2
    f1  ~~ p43 * f2
    p33 > 0.001
    p44 > 0.001
    p43 > 0.001
'
    
    lgcm.1stage.EC <- lavaan(model = model.lgcm.1stage.EC,data=data.i)
    # add stop function if we estimate unconverged models
    if(lgcm.1stage.EC@Fit@converged == FALSE){
      stop("Unconverged model in 1 stage EC estimation.")
    }
    # store results of the estimation in appropriate matrices.
    results.EC.1model.est <- parTable(lgcm.1stage.EC)[c(27, 28, 35, 38, 29, 30, 41, 42, 43,
                                                        36, 37, 39, 40, 1:6, 31:34, 7:12), 14]
    results.EC.1model.se <- parTable(lgcm.1stage.EC)[c(27, 28, 35, 38, 29, 30, 41, 42, 43,
                                                        36, 37, 39, 40, 1:6, 31:34, 7:12), 15]
    
    # store results in the appropriate place in the array.  
    results.output[, , i] <- cbind(results.true, results.f1.est, results.f1.se,
                                   results.AS.est, results.AS.se,
                                   results.EC.1model.est, results.EC.1model.se,
                                   results.EC.2step.est, results.EC.2step.se)
      
  }
  
  colnames(results.output) <- c("True / Simulated Values", "Est. using true factor scores", "SE using true factor scores",
                                "Est. using AS estimates", "SE using AS estimates", 
                                "Est. using EC estimates (1 model)", "SE using EC estimates (1 model)",
                                "Est. using EC estimates (2 step)", "SE using EC estimates (2 step)")
  
  rownames(results.output) <- c("eta1", "eta2", "eta3", "eta4", "psi11", "psi22", "psi33", "psi44", "psi43",
                                "beta1", "beta2", "beta3", "beta4", "lambda11", "lambda21", "lambda31",
                                "lambda42", "lambda52", "lambda62", "theta11", "theta22", "theta33", "theta44",
                                "theta55", "theta66", "theta77", "theta88", "theta99", "theta1010")
  return(results.output)
  
}


# Executing statements
# list of true parameters to simulate the data with. 
true.parameters <- list(nsim = 500, n = 10000, eta = c(5, 3, 10, 10), psi = c(2, 1, 10, 10, 2), 
                        beta = c(2, 3, 3, 1), lambda = rep(1,6), theta = c(rep(10, 4), rep(25, 6)) )
set.seed(26032018)
sim <- EstimationParameters(true.parameters = true.parameters)
#data <- SimulatedData(nsim = 10, n = 10000, eta = c(5, 3, 10, 10), psi = c(2, 1, 10, 10, 1), 
#              beta = c(2, 3, 3, 1), lambda = rep(1,6), theta = c(rep(10, 4), rep(25, 6)) )

results.table <- matrix(NA, ncol = 9, nrow=29)
for (i in 1:29){
results.table[i, ] <- round(rowMeans(sim[i , ,]),2)
}

colnames(results.table) <- c("True / Simulated Values", "Est. using true factor scores", "SE using true factor scores",
                              "Est. using AS estimates", "SE using AS estimates", 
                              "Est. using EC estimates (1 model)", "SE using EC estimates (1 model)",
                              "Est. using EC estimates (2 step)", "SE using EC estimates (2 step)")

rownames(results.table) <- c("eta1", "eta2", "eta3", "eta4", "psi11", "psi22", "psi33", "psi44", "psi43",
                              "beta1", "beta2", "beta3", "beta4", "lambda11", "lambda21", "lambda31",
                              "lambda42", "lambda52", "lambda62", "theta11", "theta22", "theta33", "theta44",
                              "theta55", "theta66", "theta77", "theta88", "theta99", "theta1010")

cbind(results.table[3, 1:7])
save.image("LGCM_example_2factor.rdata")