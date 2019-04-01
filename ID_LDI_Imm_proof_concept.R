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

  #  Use RStudio projects so we don't have to deal with this anymore...
  setwd("C:/Temp")

################################################################################
  #  Set up data for model
  #  nobs is the number of observations, length 1 integer
  #  npack is the number of packs to iterate over, all unique packs should be
  #   included
  #  nyear is a length 1 integer of the number of years in which observations
  #   were made, keep in mind this will create an index from 1:nyear
  #  PCK a vector of integers describing pack membership, the pack described by
  #   the observation in Y
  #   nrow(HARV) == length(PCK)
  #  YR a vector of integers describing the year of observation, the year
  #    described by the observation in Y
  #   ncol(HARV) == length(YR)

  #  HARV will be a matrix/df with a row for each pack (npack) and a column for
  #   each year (nyear).  I tend to keep the data long and the spread at the
  #   last moment and it will fill in the missing values and make it square and
  #   suitable for jags
  #  psize will be vector of integers indicating the size of each pack in each
  #   year
  #  y will be a vector of integers, each value should be <= psize

  #  In my head the data would look something like...
  df <- tibble::tibble(
      Pack = rep(LETTERS[1:3], 3),
      Year = rep(c(2001, 2002, 2003), each = 3)
    ) %>%
    group_by(Pack, Year) %>%
    mutate(
      psize = rpois(1, 9),
      Harvest = rbinom(1, size = psize, prob = .3),
      y = rbinom(1, size = psize, prob = .1)
    ) %>%
    ungroup() %>%
    arrange(Pack, Year)

  #  Make long things wide, mainly covariates need to go wide because of
  #   indexing when creating the linear predictor.
  harv <- df %>%
    select(Pack, Year, Harvest) %>%
    tidyr::spread(Year, Harvest)


  #  Gather data for jags
  jags_data <- list(
    npack = n_distinct(df$Pack),
    nyear = n_distinct(df$Year),
    PCK = as.integer(as.factor(df$Pack)),
    YR = df$Year - min(df$Year) + 1,
    nobs = nrow(df),

    HARV = harv[,-1],
    psize = df$psize,
    y = df$y
  )


################################################################################
  #####  JAGS model  ####

  sink("./ID_LDI_Mod_testing.txt")
  cat("
  model {

    ##  Priors
    #  Fixed effects
    B1 ~ dnorm(0, 0.35)  # Harvest (Y/N)
    alpha ~ dnorm(0, 0.01)

    #  Hyperparameters for random effect of pack
    sd_pack ~ dunif(0, 200)
    tau_pack <- 1/(sd_pack ^ 2)

    #  Random effect of pack
    for(j in 1:npack) {
      pack_eff[j] ~ dnorm(0, tau_pack)
    }

    #  Linear predictor with random effect for pack
    for(pk in 1:npack) {
      for(yr in 1:nyear){
        logit(lp[pk,yr]) <- alpha + B1 * HARV[pk, yr] + pack_eff[pk]
      }
    }

    for(i in 1:nobs) {
      y[i] ~ dbin(lp[PCK[i],YR[i]], psize[i])
    }
    # Nested indexing on PCK saves me a for loop

    ##  Derived parameters
    #  Proportion of immigrants in each pack when harvest did & did not occur
    #  Creates all possible combinations, but we could zero out the ones that
    #   do not matter
    #  The loops are not needed right now because we dropped the random effect
    #   and so are just repeating the estimate for a generic wolf pack over and
    #   over
    # for(p in 1:npack) {
    #   for(yr in 1:nyear){
    #     for(k in 1:2) {
    #       pack.imm[k, p] <- 1/(1 + exp(-(alpha + B1 * (k - 1))))
    #     }
    #   }
    # }

    #  Mean proportion of immigrants across packs when harvest did & did not occur
    # for(k in 1:2) {
    #   mean.imm[k] <- mean(pack.imm[k,])
    # }

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
  params <- c("B1", "alpha", "sd.tau", "sd.alpha", "pack.imm", "mean.imm")

  #  MCMC settings
  ni <- 500
  nt <- 1
  nb <- 400
  nc <- 3

  #  Call JAGS
  out <- jags(
    jags_data,
    inits,
    params,
    n.iter = ni,
    n.thin = nt,
    n.burnin = nb,
    n.chains = nc,
    progress.bar = "text",
    jags.module = c("glm","dic"),
    "./ID_LDI_Mod_testing.txt"
  )

  print(out, dig = 2)

  #mcmcplot(out)









