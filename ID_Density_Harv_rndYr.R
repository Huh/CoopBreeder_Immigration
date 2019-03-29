################################################################################

    ##  Harvest and random effect for pack on density of wolves in central Idaho
    ##  Sarah Bassing
    ##  March 2019

################################################################################

  #  Model tests for effect of harvest (Y/N) on density of wolves in central
  #  Idaho. This includes a random effects for year.

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
    "YR" = as.numeric(as.factor(IDdensity$Year  - 2007)), 
    "AREA" = as.numeric(as.factor(IDdensity$Region)) - 1,
    "HARV" = IDdensity$Harvest - 1, 
    "SRVY" = as.numeric(IDdensity$nSites),
    "nyear" = length(unique(IDdensity$Year)),
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
      #  Fixed effects 
      B1 ~ dnorm(0, 0.35)  # Harvest (Y/N)

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
      #  Annual density when harvest did and did not occur in each study area


      ###  THIS ISN'T QUITE RIGHT B/C THERE SHOULDN'T BE A HARVEST/NO HARVEST FOR EACH YEAR
      ###  HOW DO I ADJUST FOR UNBALANCED YEARS OF HARVEST?


      for(h in 1:2) {
        for(yr in 1:nyear) {
          harv.den[h, yr] <- alpha[yr] + B1 * (h - 1)
        }
      }
      
      #  Mean density when harvest did and did not occur
      for(h in 1:2) {
        mean.den[h] <- mean(harv.den[h,])
      }

      # #  Individual estiamtes for each year and study area
      #   for(i in 1:nrow) {
      #     harv.den[i] <- alpha[YR[i]] + B1 * HARV[i]
      #   }
      # now what... hard code which harv.den estimates should be averaged together
      # to get at mean density with and without harvest?  Ewwww


      }", fill = TRUE)
  sink()

################################################################################
  #  Define initial values and parameters to monitor
  
  inits <- function(){
    list(
      B1 = runif(1, -1, 1)
    )
  }
  params <- c("B1", "alpha", "tau", "sigma", "sd.alpha", "tau.alpha", "mean.den", "harv.den")  #
  
  #  MCMC settings
  ni <- 500
  nt <- 1
  nb <- 400
  nc <- 3
  
  #  Call JAGS
  out.den.harv <- jags(win.data, inits, params, n.iter = ni, n.thin = nt, n.burnin = nb, 
              n.chains = nc, progress.bar = "text", jags.module = c("glm","dic"),
              "./Immigration Models/ID_Density_Harv_rndYr.txt")
  
  print(out.den.harv, dig = 2)
  
  # mcmcplot(out.den.harv)
  
  #   WHY DOES THIS SAY THERE'S A POSITIVE EFFECT OF HARVEST ON DENSITY? 
  #   I'M SO CONFUSSED!  AND WHY IS IT UNDERESTIMATING DENSITY PRE-HARVEST?