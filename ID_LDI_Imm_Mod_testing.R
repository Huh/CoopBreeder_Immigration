    ##  Harvest and random effect for pack on proportion of long-dist. immigrants
    ##  Sarah Bassing
    ##  March 2019
################################################################################
  #  Model tests for effect of harvest (Y/N) on the proportion of long-distance 
  #  immigrants per pack. This includes a random effects for pack. Data are 
  #  pooled across study areas to increase sample size (also no difference 
  #  between study areas in the proportion of long-distance immigrants per pack).
  #  P.S. Box 6.2.1 in Hooten & Hobbs 2015 explains this random effect very well.
################################################################################
  #  Load packages & data  
  require(R2jags)
  require(mcmcplots)
  require(dplyr)
  
  load.module("glm")
  
  setwd("G:/My Drive/1_Repositories/CoopBreeder_Immigration")
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
  
  #  Reformat structure of harvest covariate... 
  # harv <- packd[,c(1,3,4)] %>%
  #   tidyr::spread("Year", "Harvest") %>%
  #   mutate(
  #     Putative = as.numeric(as.factor(Putative))
  #   )
  # yrs <- seq(1,9)
  # colnames(harv) = c("Putative", yrs)
  
  win.data <- list(
    "y" = packd$Nimm,
    "YR" = as.numeric(as.factor(packd$Year)),  # packd$Year - 2008
    "AREA" = packd$Region - 1,
    "HARV" = packd$Harvest,  #harv[,2:10] #model not happy with this version of formatting
    "SRVY" = as.numeric(scale(packd$nSites)),
    "PCK" = as.numeric(as.factor(packd$Putative)),
    "psize" = as.numeric(packd$Nsampled),
    "nyear" = length(unique(packd$Year)),
    "npack" = length(unique(packd$Putative)), 
    "nrow" = nrows
  )

################################################################################   
  #####  JAGS model  ####
  
  sink("./Immigration Models/ID_LDI_Mod_testing.txt")
  cat("
  model {
    
    #  Linear predictor with random effect for pack
    for(pk in 1:npack) {
      for(i in 1:nrow) {
        logit(lp[pk,i]) <- alpha[pk] + B1 * HARV[i]
      }
    }

    for(i in 1:nrow) {
      y[i] ~ dbin(lp[PCK[i],i], psize[i])
    }
    # Nested indexing on PCK saves me a for loop

    # Various attempts at the likelihood, etc. that aren't working out

    # #  Josh's version but dimensions of HARV don't make sense to me and it doens't work
    # for(pk in 1:npack) {
    #   for(yr in 1:nyear) {
    #     logit(lp[pk, yr]) <- alpha[pk] + B1 * HARV[pk, yr]
    #   }
    # }
    #
    # #  Josh's version but model doesn't like dimensions of lp
    # for(i in 1:nrow) {
    #   y[i] ~ dbin(lp[PCK[i], YR[i]])  #, psize[i]
    # }

    # #  Original version with likelihood and linear predictor combined in 1 loop:
    # for(i in 1:nrow) {
    #   y[i] ~ dbin(lp[i], psize[i])
    #   logit(lp[i]) <- alpha[PCK[i]] + B1 * HARV[i]
    # }



    ##  Priors
    #  Fixed effects
    B1 ~ dnorm(0, 0.35)  # Harvest (Y/N)

    #  Random effect of pack
    for(j in 1:npack) {
      alpha[j] ~ dnorm(0, sd.tau)
    }

    #  Hyperparameters for random effect of pack
    sd.alpha ~ dunif(0, 200)
    sd.tau <- 1/(sd.alpha^2)


    ##  Derived parameters
    #  Proportion of immigrants in each pack when harvest did & did not occur
    for(k in 1:2) {
      for(p in 1:npack) {   # also try YR
        ind.imm[k, p] <- 1/(1 + exp(-(alpha[p] + B1 *(k - 1))))
      }
    }

    #  Mean proportion of immigrants across packs when harvest did & did not occur
    for(k in 1:2) {
      mean.imm[k] <- mean(ind.imm[k,])
    }

      }
      ", fill = TRUE)
  sink()
################################################################################  
  
  #  Define initial values and parameters to monitor
  inits <- function(){
    list(
      B1 = runif(1, -1, 1)
    )
  }
  params <- c("B1", "alpha", "sd.tau", "sd.alpha", "mean.imm") #"ind.imm", 
  
  #  MCMC settings
  ni <- 50000
  nt <- 1
  nb <- 40000
  nc <- 3
  
  #  Call JAGS
  out <- jags(win.data, inits, params, n.iter = ni, n.thin = nt, n.burnin = nb, 
              n.chains = nc, progress.bar = "text", jags.module = c("glm","dic"),
              "./Immigration Models/ID_LDI_Mod_testing.txt")
  
  print(out, dig = 2)
  
  mcmcplot(out)
  
  
  
  
  
  
  
  
  
