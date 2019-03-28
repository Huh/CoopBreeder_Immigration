    ##  Harvest and random effect for pack on proportion of long-dist. immigrants
    ##  Sarah Bassing
    ##  March 2019
################################################################################
  #  Model tests for effect of harvest (Y/N) on the proportion of long-distance 
  #  immigrants per pack. This includes a random effects for pack. Data are 
  #  pooled across study areas to increase sample size (also no difference 
  #  between study areas in the proportion of long-distance immigrants per pack).
################################################################################
  #  Load packages & data  
  require(R2jags)
  require(mcmcplots)
  require(dplyr)
  
  load.module("glm")
  
  setwd("G:/My Drive/1 University of Montana/Immigration_Hypothesis_Testing/Models")
  load("./Input/ID_imms.RData")

################################################################################  
  #  Set up data for model  
  packd <- ID_imms %>%
    group_by(Year, Region, Putative, Harvest, nSites) %>%
    summarise(
      Nsampled = length(unique(wolf_id)),
      Nimm = sum(LDI == 1)
    ) %>%
    ungroup()
  nrows <- nrow(packd)
  
  win.data <- list(
    "y" = packd$Nimm,
    "YR" = as.numeric(as.factor(packd$Year)),  # - 2008
    "AREA" = packd$Region - 1,
    "HARV" = packd$Harvest, 
    "SRVY" = as.numeric(scale(packd$nSites)),
    "PCK" = as.numeric(as.factor(packd$Putative)),
    "packd" = as.numeric(packd$Nsampled),
    "npack" = length(unique(packd$Putative)), 
    "nrow" = nrows
  )

################################################################################   
  #####  JAGS model  ####
  
  sink("./Immigration Models/ID_LDI_Mod_testing.txt")
  cat("
  model {
    
    #  Likelihhod with random effect for pack
    for(i in 1:nrow) {
      y[i] ~ dbin(p[i], packd[i])
      for(j in 1:npack) {
        logit(p[i,j]) <- alpha[j] + B1 * HARV[i]   #alpha[PCK[i]]
        # now the indexing is all screwed up. need to figure this out.
      }
    }


    #  Priors for fixed effects
    B1 ~ dnorm(0, 0.35)

    #  Priors for random effect of pack
    for(j in 1:npack) {
      alpha[j] ~ dnorm(0, sd.tau)
    }
    # #  Priors for random effect of year
    # for(j in 1:YR) {
    #   alpha[j] ~ dnorm(0, tau.alpha)
    # }

    #  Hyperprior for random effect of pack
    sd.alpha ~ dunif(0, 200)
    sd.tau <- 1/(sd.alpha^2)
    #tau.alpha ~ dscaled.gamma(sd.tau, df)  # what the heck should the values of these parameters be?!
    #tau.alpha ~ dgamma(0.01, 0.01) # this works-ish but 95%CRI = 0 - 800 so...
    #tau.alpha ~ dunif(0, 200) # definitely not working on precision
    #sd.alpha <- sqrt(1/tau.alpha)

    #  Derived parameters
    #  Proportion of immigrants in each pack when harvest did & did not occur
    for(k in 1:2) {
      for(p in 1:npack) {   # also try YR
        ind.imm[k, p] <- 1/(1 + exp(-(alpha[p] + B1 *(k - 1))))
      }
    }

    #  Mean proportion of immigrants across packs when harvest did & did not occur
    for(h in 1:2) {
      mean.imm[h] <- mean(ind.imm[h,])
    }

      }
      ", fill = TRUE)
  sink()
################################################################################  
  
  #  Define initial values and parameters to monitor
  inits <- function(){
    list(
      B0 = runif(1, -1, 1),
      B1 = runif(1, -1, 1)
    )
  }
  params <- c("B1", "alpha", "sd.tau", "sd.alpha", "mean.imm") #"ind.imm", 
  
  #  MCMC settings
  ni <- 25000
  nt <- 1
  nb <- 20000
  nc <- 3
  
  #  Call JAGS
  out <- jags(win.data, inits, params, n.iter = ni, n.thin = nt, n.burnin = nb, 
              n.chains = nc, progress.bar = "text", jags.module = c("glm","dic"),
              "./Immigration Models/ID_LDI_Mod_testing.txt")
  
  print(out, dig = 2)
  
  #mcmcplot(out)
  
  
  
  
  
  
  
  
  
