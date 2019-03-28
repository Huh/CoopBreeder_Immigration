    ##  Harvest and random effect for pack on density of wolves in central Idaho
    ##  Sarah Bassing
    ##  March 2019
################################################################################

################################################################################
  #  Load packages & data  
  require(R2jags)
  require(mcmcplots)
  require(dplyr)
  
  load.module("glm")
  
  setwd("G:/My Drive/1_Repositories/CoopBreeder_Immigration")
  load("./Input/ID_Gdensity.RData") 

################################################################################
  #  Setup data for model
  IDdensity <- ID_Gdensity %>%
    group_by(Region, Year, Harvest, nSites) %>%
    summarise(
      nind = as.numeric(sum(n)),
      density = as.numeric(mean(Density))
    ) %>%
    ungroup() %>%
    as.data.frame()
  nrow <- nrow(IDdensity)
  
  win.data <- list(
    "y" = IDdensity$density,
    "YR" = as.numeric(as.factor(IDdensity$Year)), 
    "AREA" = as.numeric(as.factor(IDdensity$Region)) - 1,
    "HARV" = IDdensity$Harvest - 1, 
    "SRVY" = as.numeric(IDdensity$nSites),
    "nyear" = length(IDdensity$Year),
    "nrow" = nrow
  )

################################################################################   
  #####  JAGS model  ####
  
  sink("./Immigration Models/ID_Density_Harv_rndYr.txt")
  cat("
  model {
      
      #  Linear predictor
      #  Alpha[yr] is a random effect for year
      for(yr in 1:nyear) {
        for(i in 1:nrow) {
          den[yr,i] <- alpha[yr] + B1 * HARV[i]
        }
      }

      #  Likelihood
      for(i in 1:nrow) {
        y[i] ~ dnorm(den[YR[i],i], tau)
      }
      

      #  Specify priors
      #  Fixed effect: Harvest (Y/N)
      B1 ~ dnorm(0, 0.35)

      #  Precision on normal distribution
      tau ~ dgamma(0.001, 0.001)
      sigma <- sqrt(1/tau)

      #  Random effect for year
      for(yr in 1:nyear) {
        alpha[yr] ~ dnorm(0, tau.alpha)
      }
      
      #  Hyperparameters for random effect of year
      sd.alpha ~ dunif(0, 200)
      tau.alpha <- 1/(sd.alpha)^2


      #  Derived parameters
      #  Annual density when harvest did and did not occur
      for(h in 1:2) {
        for(yr in 1:nyear) {
          harv.den[h, yr] <- alpha[yr] + B1 * (h - 1)
        }
      }

      #  Mean density when harvest did and did not occur
      for(h in 1:2) {
        mean.den[h] <- mean(harv.den[h,])
      }


      }", fill = TRUE)
  sink()

################################################################################
  #  Define initial values and parameters to monitor
  
  inits <- function(){
    list(
      B1 = runif(1, -1, 1)
    )
  }
  params <- c("B1", "alpha", "tau", "sigma", "sd.alpha", "tau.alpha", "harv.den", "mean.den") 
  
  #  MCMC settings
  ni <- 50000
  nt <- 1
  nb <- 40000
  nc <- 3
  
  #  Call JAGS
  out.den.harv <- jags(win.data, inits, params, n.iter = ni, n.thin = nt, n.burnin = nb, 
              n.chains = nc, progress.bar = "text", jags.module = c("glm","dic"),
              "./Immigration Models/ID_Density_Harv_rndYr.txt")
  
  print(out.den.harv, dig = 2)
  
  mcmcplot(out.den.harv)