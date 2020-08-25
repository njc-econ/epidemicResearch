library(data.table)

N <- 18000000L
R <- 10000L
I0 <- 4000L

beta <- 0.11
delta <- 0.07

parameterList <- list(N = N, I0 = I0,R0=R, betaMat = matrix(beta), delta = delta)

simulationSIR <- function(parameterList, maxPeriods = 365){
  # first step initially select the infected in each population
  
  # with recovered as input the effective susceptible population is N-R
  # can treat the population as having that size
  
  
  
  t <- 0
  I <- parameterList$I0
  R <- parameterList$R0
  N <- parameterList$N
  
  betaMat <- parameterList$betaMat
  delta <- parameterList$delta
  
  Icounts <- c(I,integer(maxPeriods-1L))
  Rcounts <- c(R,integer(maxPeriods-1L))
  deltaI <- c(NA_integer_,integer(maxPeriods-1L))  
  # then the looping step, at each time period collect and store counts
  
  env <- list2env(list(t=t,I=I,R=R,N=N,Icounts=Icounts,Rcounts=Rcounts,deltaI=deltaI))
  
  #simResults <- data.table(t, matrix(Icounts,nrow=1), matrix(Rcounts,nrow=1), countInfected, countRecovered)
  
  
  while (env$t < maxPeriods & env$I > 0){
    with(env, {
      
      
      # number of susceptibles at the start of the period
      S <- N - R - I
      # calculate the transition probabilities for susceptibles in each population
      infectionProbabilities <- betaMat %*% (I / N)
      # randomly assign transitions
      
      newInfections <- rbinom(1L,S,infectionProbabilities)
      newRecovered <- rbinom(1L,I,delta)
      
      I <- I + newInfections - newRecovered
      R <- R + newRecovered
      
      
      t <- t+1
      Icounts[t+1] <- I
      Rcounts[t+1] <- R
      deltaI[t+1] <- newInfections
    })
    
  }
  
  simResults <- data.table(noInfected = env$Icounts[1:(env$t +1)], noRecovered = env$Rcounts[1:(env$t +1)], newCases = env$deltaI[1:(env$t +1)])
  
  return(simResults)
}

parameterList <- list(N = 18000000L, I0 = I0,R0=R, betaMat = matrix(beta), delta = delta)
system.time({largestBundesland <- simulationSIR(parameterList = parameterList)})
