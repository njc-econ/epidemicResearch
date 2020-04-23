##### Stochastic SIR two populations #####

# size of populations
N1 <- 40000
N2 <- 50000

I1_0 <- 200
I2_0 <- 100

# contact rates within and between populations
beta11 <- 0.11
beta22 <- 0.07
beta12 <- 0.03
beta21 <- 0.05

# recovery rates in different populations
# e.g. immune compromised remain sick and contagious for longer
# can also interpret delta as a measure of the quality of identification and isolation
# i.e. if testing is widespread cases are identified and "removed" from the population quicker 
delta1 <- 0.08
delta2 <- 0.06

N <- c(N1=N1,N2=N2)
delta <- c(delta1, delta2)
I0 <- c(I1_0,I2_0)

betaMat <- matrix(c(beta11, beta12, beta21, beta22), ncol=2, byrow = TRUE)

parameterList <- list(N = N, I0 = I0, betaMat = betaMat, delta = delta)

# function takes as input a list of parameters that define the epidemic
# returns a table with simulated data for no of infected and recovered
# for each of the populations referred to in the parameters

simulationSIR_manyPops <- function(parameterList, maxPeriods = 1500){
  # first step initially select the infected in each population
  t <- 0
  I <- lapply(parameterList$N, integer)
  R <- I
  
  for (i in 1:length(I)){
    I[[i]][sample(c(1:parameterList$N[i]), parameterList$I0[i])] <- 1L
  }
  
  countInfected <- sum(parameterList$I0)
  # then the looping step, at each time period collect and store counts
  
  env <- list2env(list(t=t,I=I,R=R,countInfected=countInfected))
  
  # collect counts
  Icounts <- sapply(env$I, sum)
  Rcounts <- sapply(env$R, sum)
  countInfected <- sum(Icounts)
  countRecovered <- sum(Rcounts)
  
  simResults <- data.table(t, matrix(Icounts,nrow=1), matrix(Rcounts,nrow=1), countInfected, countRecovered)
  
  while (env$t < maxPeriods & countInfected > 0){
    with(env, {
      # identify susceptibles at the start of the period
      S <- mapply(function(x,y){1L - x - y}, I, R, SIMPLIFY = FALSE)
      # calculate the transition probabilities for susceptibles in each population
      infectionProbabilities <- betaMat %*% (Icounts / N)
      # randomly assign transitions
      # first recovery - find the locations of all the infected
      infected <- lapply(I, function(x){which(x == 1)})
      # from the infected choose which will recover
      newrecovered <- mapply(function(p,q){
        if (length(q)>0) p[q] else integer()
      },infected,
      mapply(function(x,y){
        sapply(x,function(z){sample(c(TRUE,FALSE),size=1,prob = c(y,1-y))})
      }, infected, delta, SIMPLIFY = FALSE),
      SIMPLIFY = FALSE)
      
      # now infection - find the locations of all the susceptibles
      susceptibles <- lapply(S, function(x){which(x == 1)})
      # from the susceptibles choose which will be infected 
      newinfected <- mapply(
        function(p,q){
          if (length(q)>0) p[q] else integer()
        },
        susceptibles,
        mapply(function(x,y){
          sapply(x, function(z){sample(c(TRUE,FALSE),size=1,prob = c(y,1-y))})
        }, susceptibles, infectionProbabilities, SIMPLIFY = FALSE),
        SIMPLIFY = FALSE
      )
      
      # apply the status changes
      # to the infected
      # infection
      I <- mapply(function(x,y){x[y] <- 1; x},
                  I,
                  newinfected,
                  SIMPLIFY = FALSE)
      # removal
      I <- mapply(function(x,y){x[y] <- 0; x},
                  I,
                  newrecovered,
                  SIMPLIFY = FALSE)
      # apply recovery changes
      R <- mapply(function(x,y){x[y] <- 1; x},
                  R,
                  newrecovered,
                  SIMPLIFY = FALSE)
      t <- t+1
      
    })
    # collect counts
    Icounts <- sapply(env$I, sum)
    Rcounts <- sapply(env$R, sum)
    countInfected <- sum(Icounts)
    countRecovered <- sum(Rcounts)
    
    simResults <- rbindlist(list(simResults, data.table(t=env$t, matrix(Icounts,nrow=1), matrix(Rcounts,nrow=1), countInfected, countRecovered)))
    
  }
  
  
  colnames(simResults)[c(2:(1+length(N)))] <- paste0("Infected_Pop",c(1:length(N))) 
  colnames(simResults)[c((2+length(N)):(1+2*length(N)))] <- paste0("Recovered_Pop",c(1:length(N))) 
  simResults[,totalCases := countInfected + countRecovered]
  return(simResults)
}

library(data.table)
simulationResults <- simulationSIR_manyPops(parameterList)

library(ggplot2)

simulationResultsLong <- melt(simulationResults, id="t")
ggplot(simulationResultsLong, aes(x=t, y=value, color=variable)) +
  geom_line()
