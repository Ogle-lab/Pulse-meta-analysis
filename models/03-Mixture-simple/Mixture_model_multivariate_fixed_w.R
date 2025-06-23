# 5/5/2025: This is the mixture model, with the following key attributes:
#   1) Truncate t.peak
#   2) Treat study-level is root node
#   3) Multivariate models for traits.


model{
  
  for(i in 1:Nobs){ # number of observations
    # Likelihood for the log-response ratio (e.g., LRR) for response variables
    # of interest:
    Y[i] ~ dnorm(mu[i], tau[i])
    # Replicated data
    Y.rep[i] ~ dnorm(mu[i], tau[i])
    
    # Likelihood for "observation" (pooled) precision and associated variances;
    # Enables estimation of precision (variance) when values are missing:
    tau[i] ~ dgamma(a.t[vID[i]], b.t[vID[i]])
    sig[i] <- pow(tau[i], -0.5)
    
    # Mean model involves a mixture of potential functions to describe the 
    # pulse response. E.g., Function1 is based on a reparameterization of the 
    # Ricker function, expressed on the "original" LRR scale (i.e., not transformed).
    # Function2 is a simple linear function of time since pulse.
    # The mixture weight, w, is allowed to vary by pulse ID and variable ID.
    mu[i] <- w[pID[i],vID[i]]*Function1[i] + (1-w[pID[i],vID[i]])*Function2[i]
    
    # Function 1: Ricker model
    Function1[i] <- y.peak[pID[i],vID[i]]*TimePart[i]
    TimePart[i] <- exp(LogPart[i])
    # For multiplicative models, we can often avoid numerical overflow errors by 
    # modeling appropriate parts on the additive log scale, then exponentiating
    # to get the predicted (e.g, mean) value
    LogPart[i] <- 1-(t[i]/t.peak[pID[i],vID[i]]) + log(t[i]) - Lt.peak[pID[i],vID[i]]
    # Function 2: Linear model
    Function2[i] <- bb[pID[i],vID[i]] + mm[pID[i],vID[i]]*t[i]
    
    # Squared differences
    #Sqdiff[i] <- pow(Y[i] - Y.rep[i], 2) 
  }
  
  # Hierarchical priors for pulse-level parameters, centered on study-level 
  # parameters.
  for(p in 1:Npulse) {
    # For Function1 (Ricker Model):
    # Since the time at which the peak occurs should be after the pulse, t.peak > 0,
    # give hierarchical prior on log scale, centered on study-level parameters.
    # For each parameter, use multivariate normal distribution for the hierarhical
    # prior to account for correlations among the different response variables 
    # measured within the same pulse experiment.
    
    # Hierarchical multivariate normal prior for log-scasle t.peak, with parmeters
    # that vary by study ID and variable ID:
    ## KO: I don't think we need pID3 as it's just a vector going from 1, 2, .., 61, 
    ## where Npulse is 61
    Lt.peak[p,1:Nvar] ~ dmnorm(mu.Lt.peak[sID3[p],1:Nvar], omega.Lt.peak[1:Nvar,1:Nvar])
    
    # Back-transform Lt.peak to original t.peak scale, and truncate t.peak at
    # the maximum reported time-since-pulse values so that the peak is not estimated 
    # to occur after the last observations:
    for(v in 1:Nvar){
      ## KO: Note, read in values for maxT (not logmaxT)
      t.peak[p,v] <- min(exp(Lt.peak[p,v]),maxT[p])
    }
    
    # Hierarchical multivariate normal prior for y.peak; y.peal can be positive (increase in 
    # response after pulse) or negative (e.g., decrease in response after pulse), 
    # so model on original scale:
    y.peak[p,1:Nvar] ~ dmnorm(mu.y.peak[sID3[p],1:Nvar], omega.y.peak[1:Nvar,1:Nvar])
    
    # For Function2 (Linear model)
    # Hierarchical multivariate normal prior for intercept parameter:
    bb[p,1:Nvar] ~ dmnorm(mu.bb[sID3[p],1:Nvar], omega.bb[1:Nvar,1:Nvar])
    # Hierarchical multivariate normal for slope parameter:
    mm[p,1:Nvar] ~ dmnorm(mu.mm[sID3[p],1:Nvar], omega.mm[1:Nvar,1:Nvar])
  }
  

  # j loop (pulse)
  # for(j in 1:Nprewin){
  #   # Define the mixture weight; if the weight is not 0 or 1 (deterministic),
  #   # use beta distribution (stochastic) as a prior for the mixture weight.
  #   w[pID1[j],vid1[j]] <- ifelse(w1[j]!=1 && w1[j]!=0, wx[pID1[j],vid1[j]], w1[j])
  # }
  for(p in 1:Npulse){
    for(v in 1:Nvar){
      # Define the mixture weight; if the weight is not 0 or 1 (deterministic), 
      # use beta distribution (stochastic) as a prior for the mixture weight.
      ## KO: Note: w1 is a matrix, Npulse x Nvar; for any pulse-variable combos
      ## that don't exist, set w1 = 0; these will not used and
      ## we don't care about them.
      w[p,v] <- equals(w1[p,v],1)*1 + equals(w1[p,v],0)*0 + 
        (1-equals(w1[p,v],1)-equals(w1[p,v],0))*wx[p,v]
      #w[p,v] <- ifelse(w1[p,v]!=1 && w1[p,v]!=0, wx[p,v], w1[p,v])
      # Assign independent priors wrt to response variable to mixture weight:
      # Hierarchical model for mixture weight, based on a beta distribution.
      # Some pulse-variable combinations are irrelvant (no corresponding data),
      # So we can "ignore" wx and just monitor / report w (above)
      ## KO: Emma, check that I'm using the correct sID (sID3) -- it is of length
      # 61, which is the same as Npulse, so I'm assuming this is the study ID
      # for each of the unique pulses.
      wx[p,v] ~ dbeta(a.w[sID3[p],v], b.w[sID3[p],v])
    }
  }
  
  # Assign priors to study level parameters for each response variable:
  for(s in 1:Nstudy){
    for(v in 1:Nvar){
      # Independent, relatively non-informative priors for log-scale t.peak;
      # Some study-variable combinations are irrelevant (no data), so just
      # ignore parameters corresponding to those combinations (posteriors will
      # look like the priors). Truncate at the maximum reported time-since-pulse for
      # each study.
      ## need study.maxT
      mu.Lt.peak[s,v] ~ dnorm(0, 0.001)
      # Back-transform and truncate to get study-level t.peak:
      mu.t.peak[s,v] <- min(exp(mu.Lt.peak[s,v]),study.maxT[s])
      # Independent, relatively non-informative priors for remaining study-
      # variable level parameters:
      mu.y.peak[s,v] ~ dnorm(0, 0.001)
      mu.bb[s,v] ~ dnorm(0, 0.001)
      mu.mm[s,v] ~ dnorm(0, 0.001)
      # For mixture weight hyperparameters, truncate at 1 using U(1,100) prior 
      # to avoid errors with the pulse-level beta prior
      a.w[s,v] ~ dunif(1,100)
      b.w[s,v] ~ dunif(1,100)
      # Expected study-level mixture weight:
      mu.w[s,v] <- a.w[s,v]/(a.w[s,v]+b.w[s,v])
    }
  }
  
  for(v in 1:Nvar){
    # Priors for observation precision
    a.t[v] ~ dunif(1,100)
    b.t[v] ~ dunif(1,100)
    # Dsum (overall)
    #Dsum[v] <- sum(Sqdiff[,v])
  }
  
  # Priors for precision matrices (Ricker model):
  omega.Lt.peak[1:Nvar,1:Nvar] ~ dwish(R[,],Nvar)
  omega.y.peak[1:Nvar,1:Nvar] ~ dwish(R[,],Nvar)
  # compute covariance matrices:
  Sig.Lt.peak[1:Nvar,1:Nvar] <- inverse(omega.Lt.peak[1:Nvar,1:Nvar])
  Sig.y.peak[1:Nvar,1:Nvar] <- inverse(omega.y.peak[1:Nvar,1:Nvar])
  # compute correlation matrices:
  for(r in 1:Nvar){
    for(c in 1:Nvar){
      cor.Lt.peak[r,c] <- Sig.Lt.peak[r,c]/(sqrt(Sig.Lt.peak[c,c])*sqrt(Sig.Lt.peak[r,r]))
      cor.y.peak[r,c] <- Sig.y.peak[r,c]/(sqrt(Sig.y.peak[c,c])*sqrt(Sig.y.peak[r,r]))
    }
  }
  
  # Priors precision matrices:
  omega.bb[1:Nvar,1:Nvar] ~ dwish(R[,],Nvar)
  omega.mm[1:Nvar,1:Nvar] ~ dwish(R[,],Nvar)
  # compute covariance matrices
  Sig.bb[1:Nvar,1:Nvar] <- inverse(omega.bb[1:Nvar,1:Nvar])
  Sig.mm[1:Nvar,1:Nvar] <- inverse(omega.mm[1:Nvar,1:Nvar])
  
  # compute correlation matrices:
  for(r in 1:Nvar){
    for(c in 1:Nvar){
      cor.bb[r,c] <- Sig.bb[r,c]/(sqrt(Sig.bb[c,c])*sqrt(Sig.bb[r,r]))
      cor.mm[r,c] <- Sig.mm[r,c]/(sqrt(Sig.mm[c,c])*sqrt(Sig.mm[r,r]))
    }
  }
}