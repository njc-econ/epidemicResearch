##### Stochastic SIR two populations #####

# size of populations
N1 <- 40000
N2 <- 50000

I1_0 <- 200
I2_0 <- 100

# contact rate
beta11 <- 0.5
beta22 <- 0.4
beta12 <- 0.2
beta21 <- 0.15

# recovery rate
delta <- 0.10

N <- c(N1=N1,N2=N2)
I0 <- c(I1_0,I2_0)

betaMat <- matrix(c(beta11, beta12, beta21, beta22), ncol=2, byrow = TRUE)

parameterList <- list(N=N, I0=I0, betaMat=betaMat, delta=delta)

simulation <- function(parameterList){
  # first step initially select the infected in each population
  t<- 0
  I <- lapply(parameterList$N, integer)
  R <- I
  for (i in 1:length(I)){
    I[[i]][sample(c(1:parameterList$N[i]), parameterList$I0[i])] <- 1L
  }
  
  countInfected <- sum(parameterList$I0)
  # then the looping step, at each time period collect and store counts
  
  env <- list2env(list(t=t,I=I,R=I,countInfected=countInfected))
  
  
  while (env$t < 1001 & env$countInfected > 0){
    # calculate the transition probabilities for susceptibles in each population
    
    # randomly assign transitions
    
    # collect counts
    Icounts <- sapply(I, sum)
    Rcounts <- sapply(R, sum)
    countInfected <- sum(Icounts)
  }
  
  
  
  
  
}