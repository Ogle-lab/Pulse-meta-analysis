### Initial Ricker model for ET subset of response variables

# if(!"postjags" %in% installed.packages()) {
#   devtools::install_github("fellmk/PostJAGS/postjags")
# }

library(dplyr)
library(tidyr)
library(tibble)
library(rjags)
load.module('dic')
library(ggplot2)
library(mcmcplots)
#library(postjags)
library(jagsUI)
# Load self-made functions
source("./scripts/functions.R")

# Load data
load("models/model_input.Rdata") # out_list
load("models/w_input.Rdata") # w_list

# A function to run the model on all
# set overwrite=T to save a new version of the coda object and initials
# set lowdev=T to save the chain with the lowest deviance and have the
# other two chains vary slightly around it
run_mod <- function(dfin, varname, overwrite = F, lowdev = F, fixed_w = F, multi = F){
  
  # Uncomment the next few lines to test the function line-by-line
  # Index Key: 1:"ET", 2:"WUE", 3:"T", 4:"Gs", 5:"PWP", 6:"ecosystemR", 7:"abovegroundR", 8:"belowgroundR", 9:"NPP", 10:"GPP", 11:"Anet"
  out_group1 <- out_list[c("ET", "NPP", "GPP", "ecosystemR")]
  df_group1 <- bind_rows(out_group1)
  out_group2 <- out_list[c("T", "Gs", "PWP", "Anet")]
  df_group2 <- bind_rows(out_group2)
  varname <- "group1" #ET, GPP, Gs
  fixed_w=T
  multi=T
  dfin <- df_group1
  
  initfilename <- paste("./models/03-Mixture-simple/inits/inits_", varname,".RData", sep = "")
  rickerinitfilename <- paste("./models/01-Ricker-simple/inits/inits_", varname,".RData", sep = "")
  linearinitfilename <- paste("./models/02-Linear-simple/inits/inits_", varname,".RData", sep = "")
  jm_codafilename <- paste("./models/03-Mixture-simple/coda/jm_coda_", varname,".RData", sep = "")
  jm_repfilename <- paste("./models/03-Mixture-simple/coda/jm_rep_", varname,".RData", sep = "")
  mcmcfoldername <- paste("./models/03-Mixture-simple/convergence/", varname, sep = "")
  mcmcfilename <- paste("MCMC_", varname, sep = "")
  model_file <- "models/03-Mixture-sconverconimple/Mixture_model.R"
  
  
  if(fixed_w==T){
    win <- as.double(w_list[[varname]])
    
    initfilename_fixed <- paste("./models/03-Mixture-simple/inits/inits_fixed_w_", varname,".RData", sep = "")
    jm_codafilename <- paste("./models/03-Mixture-simple/coda/jm_coda_fixed_w_", varname,".RData", sep = "")
    jm_repfilename <- paste("./models/03-Mixture-simple/coda/jm_rep_fixed_w_", varname,".RData", sep = "")
    mcmcfoldername <- paste("./models/03-Mixture-simple/convergence_fixed_w/", varname, sep = "")
    mcmcfilename <- paste("MCMC_", varname, sep = "")
    model_file <- "models/03-Mixture-simple/Mixture_model_fixed_w.R"
    }
  
  if(fixed_w==T){
    if(multi==T){
      initfilename_multi_fixed <- paste("./models/03-Mixture-simple/inits/inits_multi_fixed_w_", varname,".RData", sep = "")
      jm_codafilename <- paste("./models/03-Mixture-simple/coda/jm_coda_multi_fixed_w_", varname,".RData", sep = "")
      jm_repfilename <- paste("./models/03-Mixture-simple/coda/jm_rep_multi_fixed_w_", varname,".RData", sep = "")
      mcmcfoldername <- paste("./models/03-Mixture-simple/convergence_multi_fixed_w/", varname, sep = "")
      mcmcfilename <- paste("MCMC_", varname, sep = "")
      model_file <- "models/03-Mixture-simple/Mixture_model_multivariate_fixed_w.R"
    }}
    
    
  
  # remove pulses outside of range, restrict to 14 days after pulse, remove pre-pulse days
  dfin <- dfin %>%
    filter(Days.relative.to.pulse >= 0) %>%
    filter(Days.relative.to.pulse <= 14)

  
  if(fixed_w==T){
  if(multi==T){
    #out_group1 <- out_list[c("ET", "NPP", "GPP", "ecosystemR")]
    #out_group2 <- out_list[c("T", "Gs", "PWP", "Anet")]
    dfin <- dfin %>%
      mutate(Var.ID = case_when(varType == "ET" ~ 1,
                                varType == "NPP" ~ 2,
                                varType == "GPP" ~ 3,
                                varType == "ecosystemR" ~ 4,
                                varType == "T" ~ 1,
                                varType == "Gs" ~ 2,
                                varType == "PWP" ~ 3,
                                varType == "Anet" ~ 4))
  }
  }

  
  
# Create study_pulse combination, create integer sID

pulse_table <- dfin %>%
  expand(nesting(Study.ID, Pulse.ID)) %>%
  mutate(sID = as.numeric(factor(Study.ID))) %>%
  arrange(Study.ID) %>%
  tibble::rownames_to_column() %>%
  rename(pID = rowname) %>%
  mutate(pID = as.numeric(pID)) %>%
  relocate(pID, .after = sID)

if(fixed_w ==T){
  if(multi==T){
  
  pulse_table_pre <- dfin %>%
    expand(nesting(Study.ID, Pulse.ID)) %>%
    mutate(sID = as.numeric(factor(Study.ID))) %>%
    arrange(Study.ID) %>%
    tibble::rownames_to_column() %>%
    rename(pID = rowname) %>%
    mutate(pID = as.numeric(pID)) %>%
    relocate(pID, .after = sID)
  
  d_vid <- dfin %>%
    group_by(Study.ID, Pulse.ID, Var.ID) %>%
    summarise(maxT = max(Days.relative.to.pulse))
  
  pulse_table <- pulse_table_pre %>%
    left_join(d_vid)
  
  # Summarize by unique pulse
  d_Tmax_pulse <- pulse_table %>%
    select(pID,Var.ID,maxT) %>%
    group_by(pID) %>% 
    summarize(max = max(maxT))
  
  # Summarize by unique study
  d_Tmax_study <- pulse_table %>%
    select(sID,Var.ID,maxT) %>%
    group_by(sID) %>% 
    summarize(max = max(maxT)) 
  }
}

# Join with full table
df <- dfin %>%
  left_join(pulse_table) %>%
  relocate(sID, pID) %>%
  arrange(pID)


# getting w's for each pulse response
if(fixed_w==T){
  if(multi==T){
  #win <- as.double(w_list[[varname]])
  if(varname=="group1"){
    pre_win1 <- unlist(w_list[c("ET")])
    pre_win2 <- unlist(w_list[c("NPP")])
    pre_win3 <- unlist(w_list[c("GPP")])
    pre_win4 <- unlist(w_list[c("ecosystemR")])
    
    df_prewin <- data.frame(w = c(pre_win1,pre_win2,pre_win3,pre_win4))
    df_prewin$w <- ifelse(is.na(df_prewin$w), .5, df_prewin$w) # make NA values .5. This is a key so the model knows to made those values stocastic
    
    df_V1 <- df %>%
      filter(varType=="ET")
    df_V1_pid <- unique(df_V1$pID)
    V1 <- rep(1, length(df_V1_pid))
    df_V2 <- df %>%
      filter(varType=="NPP")
    df_V2_pid <- unique(df_V2$pID)
    V2 <- rep(2, length(df_V2_pid))
    df_V3 <- df %>%
      filter(varType=="GPP")
    df_V3_pid <- unique(df_V3$pID)
    V3 <- rep(3, length(df_V3_pid))
    df_V4 <- df %>%
      filter(varType=="ecosystemR")
    df_V4_pid <- unique(df_V4$pID)
    V4 <- rep(4, length(df_V4_pid))
    
    df_prewin$pID <- c(df_V1_pid,df_V2_pid,df_V3_pid,df_V4_pid)
    df_prewin$Var.ID <- c(V1,V2,V3,V4)
  }
    
  if(varname=="group2"){
    pre_win1 <- unlist(w_list[c("T")])
    pre_win2 <- unlist(w_list[c("Gs")])
    pre_win3 <- unlist(w_list[c("PWP")])
    pre_win4 <- unlist(w_list[c("Anet")])
    
    df_prewin <- data.frame(w = c(pre_win1,pre_win2,pre_win3,pre_win4))
    df_prewin$w <- ifelse(is.na(df_prewin$w), .5, df_prewin$w) # make NA values .5. This is a key so the model knows to made those values stocastic
    
    df_V1 <- df %>%
      filter(varType=="T")
    df_V1_pid <- unique(df_V1$pID)
    V1 <- rep(1, length(df_V1_pid))
    df_V2 <- df %>%
      filter(varType=="Gs")
    df_V2_pid <- unique(df_V2$pID)
    V2 <- rep(2, length(df_V2_pid))
    df_V3 <- df %>%
      filter(varType=="PWP")
    df_V3_pid <- unique(df_V3$pID)
    V3 <- rep(3, length(df_V3_pid))
    df_V4 <- df %>%
      filter(varType=="Anet")
    df_V4_pid <- unique(df_V4$pID)
    V4 <- rep(4, length(df_V4_pid))
    
    df_prewin$pID <- c(df_V1_pid,df_V2_pid,df_V3_pid,df_V4_pid)
    df_prewin$Var.ID <- c(V1,V2,V3,V4)
    
  }
  
  pulse_table <- pulse_table %>%
    left_join(df_prewin)
  
  # For all unique var-study combos
  df_var_study <- pulse_table %>% 
    distinct(sID, Var.ID, .keep_all = TRUE)
  
  # For all unique pulse-study combos
  df_pulse_study <- pulse_table %>% 
    distinct(pID, .keep_all = TRUE)
  
  # Save out combinations that actually exist-- we'll need this to filter out the combos that don't exist post jags
  saveRDS(pulse_table, paste("models/03-Mixture-simple/df_realIDs_", varname, ".RDS"))
  saveRDS(df_var_study, paste("models/03-Mixture-simple/df_var_study_", varname, ".RDS"))
  saveRDS(df_pulse_study, paste("models/03-Mixture-simple/df_pulse_study_", varname, ".RDS"))
  
  # convert pulse_table to matrix so we can call in weight's as w[pulse,variable]
    w_matrix <- pulse_table %>%
      select(pID,Var.ID,w) %>%
      pivot_wider(
        names_from = Var.ID,
        values_from = w) %>%
      select("pID","1","2","3","4") %>%
      column_to_rownames("pID") %>%
      as.matrix()
    
  # make w NA's 0 just so the model will run, we will ignore these later
    for(i in 1:nrow(w_matrix)){
      for(j in 1:ncol(w_matrix)){
        if(is.na(w_matrix[i,j])){
          w_matrix[i,j] <- 0
        }
      }
    }
  
  }
}

# Find observation start and stop values for each pulse
# These will be used to calculate Dsum for each pID for model comparison
df_starts <- df %>%
  mutate(startID = rownames(.)) %>%
  select(startID, everything()) %>%
  group_by(rleid = with(rle(pID), rep(seq_along(lengths), lengths))) %>%
  slice(1)
df_starts$startID <- as.numeric(df_starts$startID)
starts <- df_starts$startID

stops <- starts - 1
stops <- stops[2:length(stops)]
stops <- c(stops, nrow(df))

start_stops <- data.frame(startID = starts, stopID = stops)

# get pooled variance
tau = 1/df$poolVar # convert pooled variance to precision
tau = ifelse(is.infinite(tau),NA,tau)

# Prepare data list
datlist <- list(Y = df$LRR,
                t = df$Days.relative.to.pulse,
                pID = df$pID,
                sID = pulse_table$sID,
                startID = start_stops$startID,
                stopID = start_stops$stopID,
                Nobs = nrow(df),
                Npulse = nrow(pulse_table),
                Nstudy = length(unique(pulse_table$sID)),
                log.maxT = log(max(df$Days.relative.to.pulse)),
                tau = tau
                )

if(fixed_w==T){
  # edit data list
  win <- ifelse(is.na(win), .5, win) # make NA values .5. This is a key so the model knows to made those values stocastic
  datlist$w1 <- win
  datlist$pulse_startID <- start_stops$startID
  datlist$pulse_stopID <- start_stops$stopID
}

if(fixed_w==T){
  if(multi==T){
  
  # correlation matrix for dwishart
  Rmat <- diag(x = 1, nrow = 4, ncol = 4)

  datlist <- list(Y = df$LRR,
                  t = df$Days.relative.to.pulse,
                  pID = df$pID,
                  vID = df$Var.ID,
                  startID = start_stops$startID,
                  stopID = start_stops$stopID,
                  Nobs = nrow(df),
                  Npulse = length(unique(df_pulse_study$pID)),
                  Nstudy = length(unique(pulse_table$sID)),
                  Nvar = 4,
                  maxT = d_Tmax_pulse$max,
                  study.maxT = d_Tmax_study$max,
                  tau = tau,
                  w1 = w_matrix,
                  pulse_startID = start_stops$startID,
                  pulse_stopID = start_stops$stopID,
                  # pID1 = pulse_table$pID,
                  # sID1 = pulse_table$sID,
                  # vid1 = pulse_table$Var.ID,
                  # sID2 = as.vector(pulse_table2$sID),
                  # vid2 = pulse_table2$Var.ID,
                  # pID3 = as.vector(pulse_table3$pID),
                  # vID3 = as.vector(pulse_table3$Var.ID),
                  sID3 = as.vector(df_pulse_study$sID),
                  #Nprewin = nrow(pulse_table),
                  #Ncombo = nrow(pulse_table2),
                  R = Rmat# correlation matrix
  )
  }
}

#saveRDS(datlist, "test_datlist.RDS")

# Initial values: manual specification to get model started (option 1)

inits <- function(){
  list(mu.Lt.peak = rep(log(mean(df$Days.relative.to.pulse)), datlist$Nstudy),
       mu.y.peak = rnorm(datlist$Nstudy,log(mean(df$Mean)),2),
       mu.bb = rnorm(datlist$Nstudy,0,10),
       mu.mm = rnorm(datlist$Nstudy,0,10),
       sig.Lt.peak = .5,
       sig.y.peak = 3,
       sig.bb = 3,
       sig.mm = 3)
}

initslist <- list(inits(), inits(), inits())

if(fixed_w ==T){
if(multi==T){
  
  mu.Lt.peak = matrix(rnorm(datlist$Nstudy,1,2), nrow = length(unique(df$sID)), ncol = length(unique(df$Var.ID)))
  mu.bb = matrix(rnorm(datlist$Nstudy,0,10), nrow = length(unique(df$sID)), ncol = length(unique(df$Var.ID)))
  mu.mm = matrix(rnorm(datlist$Nstudy,0,10), nrow = length(unique(df$sID)), ncol = length(unique(df$Var.ID)))
  mu.y.peak = matrix(rnorm(datlist$Nstudy,log(mean(df$Mean))), nrow = length(unique(df$sID)), ncol = length(unique(df$Var.ID)))
inits <- function(){
  list(mu.Lt.peak = mu.Lt.peak,
       mu.y.peak = mu.y.peak,
       mu.bb = mu.bb,
       mu.mm = mu.mm,
       a.w = matrix(runif(datlist$Nstudy,min = 1, max =100), nrow = length(unique(df$sID)), ncol = length(unique(df$Var.ID))),
       b.w = matrix(runif(datlist$Nstudy,min = 1, max =100), nrow = length(unique(df$sID)), ncol = length(unique(df$Var.ID))),
       a.t = runif(length(unique(df$Var.ID)),min = 1, max =100),
       b.t = runif(length(unique(df$Var.ID)),min = 1, max =100)#,
       # Letting the model pick the omega initials
       #omega.Lt.peak = matrix(runif(length(unique(df$Var.ID)), min = .1, max = .8), nrow = length(unique(df$Var.ID)), ncol = length(unique(df$Var.ID))),
       #omega.y.peak = matrix(runif(length(unique(df$Var.ID)), min = .1, max = .8), nrow = length(unique(df$Var.ID)), ncol = length(unique(df$Var.ID))),
       #omega.bb = matrix(runif(length(unique(df$Var.ID)), min = 2, max = 5), nrow = length(unique(df$Var.ID)), ncol = length(unique(df$Var.ID))),
       #omega.mm = matrix(runif(length(unique(df$Var.ID)), min = 2, max = 5), nrow = length(unique(df$Var.ID)), ncol = length(unique(df$Var.ID)))
       )

}
initslist <- list(inits(), inits(), inits())

#saveRDS(initslist, "test_inits.RDS")
}
}


# Initial values: load ricker and linear inits to get model started (option 2)
if(file.exists(rickerinitfilename)){
  
  # ricker
  load(rickerinitfilename)
  initslist <- saved_state[[2]]
  
  # linear
  load(linearinitfilename)
  linearlist <- saved_state[[2]]
  for(i in c(1:3)){
    initslist[[i]][["mu.bb"]] <- linearlist[[i]][["mu.bb"]]
    initslist[[i]][["mu.mm"]] <- linearlist[[i]][["mu.mm"]] 
    initslist[[i]][["sig.bb"]] <- linearlist[[i]][["sig.bb"]]
    initslist[[i]][["sig.mm"]] <- linearlist[[i]][["sig.mm"]]
  }

}

# Initial values: from saved state (option 3)
if(file.exists(initfilename)){
  load(initfilename)
  initslist <- saved_state[[2]]
}else if(!file.exists(initfilename)){
  initslist <- initslist
}


# Run and monitor parameters

params <- c("w",
            "bb", "mm", # intercept and slope for linear model
            "t.peak","y.peak", "Lt.peak", # pulse-level parameters
            "sig", "sig.Lt.peak", "sig.y.peak", "tau",
            "M.Lt.peak","M.y.peak",
            "mu.Lt.peak", "mu.y.peak", # population-level parameters
            "M.bb", "M.mm",
            "mu.bb", "mu.mm",
            "M.w", # overall-level w
            "mu.w", # study-level w
            "deviance", # model performance metrics
            "R2") # Model fit

if(fixed_w==T){ # if not all weights are stochastic, track both stochastic and non-stochastic weight versions
  params <- c("w", "wx",
              "bb", "mm", # intercept and slope for linear model
              "t.peak","y.peak", "Lt.peak", # pulse-level parameters
              "sig", "sig.Lt.peak", "sig.y.peak", "tau",
              "M.Lt.peak","M.y.peak",
              "mu.Lt.peak", "mu.y.peak", # population-level parameters
              "cor.Lt.peak", "cor.y.peak", "cor.bb", "cor.mm",
              "M.bb", "M.mm",
              "mu.bb", "mu.mm",
              "M.w", # overall-level w
              "mu.w", # study-level w
              "deviance", # model performance metrics
              "R2") # Model fit
}
if(fixed_w==T){ # if not all weights are stochastic, track both stochastic and non-stochastic weight versions
  if(multi==T){ # not all pulse-var-study combinations exist, remove nonexistent rows after running
    params <- c("w", "wx",
                "bb", "mm", # intercept and slope for linear model
                "omega.Lt.peak", "omega.y.peak", "omega.bb", "omega.mm",
                "a.w", "b.w", "a.t", "b.t",
                "t.peak","y.peak", "Lt.peak", # pulse-level parameters
                "sig", "sig.Lt.peak", "sig.y.peak", "tau",
                "M.Lt.peak","M.y.peak",
                "mu.Lt.peak", "mu.y.peak", # population-level parameters
                "cor.Lt.peak", "cor.y.peak", "cor.bb", "cor.mm",
                "M.bb", "M.mm",
                "mu.bb", "mu.mm",
                "M.w", # overall-level w
                "mu.w", # study-level w
                "deviance") # model performance metrics
  }
}

# Run model with jagsui package
jagsui <- jags(data = datlist,
               inits = initslist,
               model.file = model_file,
               parameters.to.save = params,
               n.chains = 3,
               n.adapt = 1000,
               n.thin = 10,
               n.iter = 100000,
               parallel = TRUE)

jm_coda <- jagsui$samples

# If converged, save out
if(!dir.exists("models/03-Mixture-simple/coda")) {
  dir.create("models/03-Mixture-simple/coda")
}

if(overwrite==T){
  save(jagsui, file = jm_codafilename) #for local
}


# Plot output
if(!dir.exists("models/03-Mixture-simple/convergence")) {
  dir.create("models/03-Mixture-simple/convergence")
}
if(!dir.exists("models/03-Mixture-simple/convergence_fixed_w")) {
  dir.create("models/03-Mixture-simple/convergence_fixed_w")
}
if(!dir.exists(mcmcfoldername)) {
  dir.create(mcmcfoldername)
}
mcmcplot(jm_coda, parms = params, 
         dir = mcmcfoldername,
         filename = mcmcfilename)

# Save state

if(lowdev == T){
# Save inits based on chains with lowest deviance
dev_col <- which(colnames(jm_coda[[1]]) == "deviance")
dev1<- mean(jm_coda[[1]][,dev_col])
dev2<- mean(jm_coda[[2]][,dev_col])
dev3<- mean(jm_coda[[3]][,dev_col])
dev_min <- min(dev1, dev2, dev3)
if(dev1 == dev_min){
  devin = 1
} else if(dev2 == dev_min){
  devin = 2
} else if(dev3 == dev_min){
  devin = 3
}
}

# inits to save
init_names = names(initslist[[1]])

if(multi==T){
  init_names = c("mu.Lt.peak","mu.y.peak","mu.bb","mu.mm","a.w","b.w","a.t","b.t","omega.Lt.peak","omega.y.peak", "omega.bb", "omega.mm")
}

# use get_remove_index function to find which variables to remove
remove_vars = get_remove_index(init_names, params, type="jagsUI")

newinits <- initfind(jm_coda, OpenBUGS = FALSE)
newinits[[1]]
saved_state <- removevars(initsin = newinits, 
                          variables = remove_vars)

if(lowdev == T){
# saved chain with lowest deviance, and make remaining chains vary around it
saved_state[[2]][[1]] = saved_state[[2]][[devin]] # Best (low dev) initials for chain 1
saved_state[[2]][[2]] = lapply(saved_state[[2]][[devin]],"*",2)
saved_state[[2]][[3]] = lapply(saved_state[[2]][[devin]],"/",2)
}

saved_state[[1]]
if(!dir.exists("models/03-Mixture-simple/inits")) {
  dir.create("models/03-Mixture-simple/inits")
}

if(overwrite==T){
  save(saved_state, file = initfilename) #for local

  if(fixed_w==T){
    if(multi==F){
      save(saved_state, file = initfilename_fixed) #for local
    }
  }

  if(fixed_w==T){
    if(multi==T){
      save(saved_state, file = initfilename_multi_fixed) #for local
    }
  }

}

# If converged, run and save replicated data
jm_rep <- update(jagsui, parameters.to.save = "Y.rep",
                 n.iter = 15000, n.thin = 5)

if(overwrite==T){
  save(jm_rep, file = jm_repfilename) #for local
}

}


######## Use function to run model ############################################################

# To run all at once
variables <- c("ET", "T", "Gs", "PWP",
               "ecosystemR","belowgroundR",
               "NPP", "GPP", "Anet")

############ Uncomment if running locally #########################
# First run
# for(i in 1:length(variables)){
#   df_var <- as.data.frame(out_list[i])
#   run_mod(df_var, variables[i], overwrite = T)
# }

# Second run, after running 06_determine_mixture_weights
# for(i in 1:length(variables)){
#   df_var <- as.data.frame(out_list[i])
#   run_mod(df_var, variables[i], overwrite = T, fixed_w = T)
# }


######### Uncomment if running parallel on an HPC ########################
# set fixed_w and multi to T or F depending on which mixture model we want to run


# Run this first
# Set run params
# args<-commandArgs(TRUE)
# print(args)
# print("resname:")
# (resvar <- as.numeric(args[1]))
# print("seed:")
# (SEED <- as.numeric(args[2]))
# 
# 
# # Set defined R seed
# set.seed(SEED, kind = NULL, normal.kind = NULL)
# # Generate "random" seed for jags
# JAGS.seed<-ceiling(runif(1,1,10000000))

# for(i in resvar){
#   print(variables[i])
#   df_var <- as.data.frame(out_list[i])
#   run_mod(df_var, variables[i], overwrite = T, fixed_w = F)
# }

# Run this after running 05_determine_mixture_weights.R

# Set run params
args<-commandArgs(TRUE)
print(args)
print("groupname:")
(groupvar <- as.numeric(args[1]))
print("seed:")
(SEED <- as.numeric(args[2]))

if(groupvar==1){
  out_group <- out_list[c("ET", "NPP", "GPP", "ecosystemR")]
  df_group <- bind_rows(out_group)
  groupvarname = "group1"
}
if(groupvar==2){
  out_group <- out_list[c("T", "Gs", "PWP", "Anet")]
  df_group <- bind_rows(out_group)
  groupvarname = "group2"
}

run_mod(df_group, groupvarname, overwrite = T, fixed_w = T, multi = T)

# Run for univariate fixed responses
# run_mod(df_var, "belowgroundR", overwrite = T, fixed_w = T, multi = F)

