### Plot output, put model output in convenient format for post-analysis

library(dplyr)
library(tidyr)
library(jagsUI)
library(ggforce)
library(ggh4x) # for facet_nested
library(cowplot)
# Load self-made functions
source("./scripts/functions.R")


### Format output and combine table with study and pulse-level data we care about

# Load cleaned data
load("models/model_input.Rdata") # out_list

 variables <- c("ET", "T", "Gs", "PWP", # all variables
                "ecosystemR","belowgroundR",
                "NPP", "GPP", "Anet")

# Format study and pulse-level data we want in the context of the model output
pulse_table <- list()
for( i in 1:length(variables)){
  
  dfin <- out_list[[i]]
  
  # remove pulses outside of range, restrict to 14 days after pulse, remove pre-pulse days
  dfin <- dfin %>%
    filter(Days.relative.to.pulse >= 0) %>%
    filter(Days.relative.to.pulse <= 14)
  
  
  # Create study_pulse combination, create integer sID, pull study and pulse-level variables we care about
  pulse_table[[i]] <- dfin %>%
    expand(nesting(Study.ID, Pulse.ID, varType, MAT.C.wc, MAT.C, MAP.mm.wc, MAP.mm, aridity, elev.m.wc, unitDuration, Vegetation.type, Sample.unit, Pulse.type, Pulse.amount.mm, preSWC, obs)) %>%
    mutate(sID = as.numeric(factor(Study.ID))) %>%
    arrange(Study.ID) %>%
    tibble::rownames_to_column() %>%
    rename(pID = rowname) %>%
    mutate(pID = as.numeric(pID)) %>%
    relocate(pID, .after = sID) %>%
    relocate(sID, pID)
  
}



# variables <- c("ET", "T", "Gs", "PWP", # all variables
#                "ecosystemR","belowgroundR",
#                "NPP", "GPP", "Anet")
variables <- c("belowgroundR")
multigroups <- c("group1", "group2") # group 1 is c("ET", "NPP", "GPP", "ecosystemR") anf group 2 is c("T", "Gs", "PWP", "Anet")
model_table_longer1 <- list()
for(i in 1:length(variables)){ # variable loop
  print(paste("i:",i,sep=""))
  var <- variables[i]
    
  # Load jagsUI object
  load(paste("models/03-Mixture-simple/coda/jm_coda_fixed_w_", var, ".RData", sep="")) # jagsui

    
  model_table_longer1[[i]] <- data.frame(ID1 = get_index(jagsui), ID2 = NA, varType = var, # identifiers
                                          param = gsub('[[:digit:]]+', '', names(do.call(c, jagsui$mean))), # parameter names
                                          mean = do.call(c, jagsui$mean), 
                                          pc2.5 = do.call(c, jagsui$q2.5), pc97.5 = do.call(c, jagsui$q97.5),
                                          overlap0 = do.call(c, jagsui$overlap0),
                                          gel = do.call(c, jagsui$Rhat))
}
### dataframe of all models and variables connected to site- and pulse-level data, 
### dataframe of all models and variables in more of a coda-style format, more convenient for graphing
df_output_longer1 <- bind_rows(model_table_longer1) # convert list into a dataframe

df_output_longer1 <- df_output_longer1 %>%
  filter(param %in% c("t.peak","y.peak","bb", "mm","w")) %>%
  rename(pID = ID1) %>%
  mutate(converged = ifelse(gel<1.2, "yes", "no"))

# for multivariate model variables
model_table_longer2 <- list()
for(i in 1:length(multigroups)){ # variable loop
  print(paste("i:",i,sep=""))
  var <- multigroups[i]
  
  # Load jagsUI object
  load(paste("models/03-Mixture-simple/coda/jm_coda_multi_fixed_w_", var, ".RData", sep="")) # jagsui
  
  model_table_longer2[[i]] <- dumsum(jagsobj = jagsui, label = var, type = "jagsUI")
  # model_table_longer2[[i]] <- data.frame(ID = get_index(jagsui), groupType = var, # identifiers
  #                                       param = gsub('[[:digit:]]+', '', names(do.call(c, jagsui$mean))), # parameter names
  #                                       mean = do.call(c, jagsui$mean), 
  #                                       pc2.5 = do.call(c, jagsui$q2.5), pc97.5 = do.call(c, jagsui$q97.5),
  #                                       overlap0 = do.call(c, jagsui$overlap0),
  #                                       gel = do.call(c, jagsui$Rhat))
}
### dataframe of all models and variables connected to site- and pulse-level data, 
### dataframe of all models and variables in more of a coda-style format, more convenient for graphing
df_output_longer2 <- bind_rows(model_table_longer2) # convert list into a dataframe

### Now, convert group and index names into varTypes
# keys:
#[p,v]
df_output_longer2 <- df_output_longer2 %>%
  filter(param %in% c("t.peak","y.peak","bb", "mm","w")) %>%
  mutate(pID = ID1, vID = ID2)

df_output_longer2$varType = NA
for(i in 1:nrow(df_output_longer2)){
  
  if(df_output_longer2$group[i]=="group1"){
    if(df_output_longer2$vID[i]==1){
      df_output_longer2$varType[i] = "ET"
    }
    if(df_output_longer2$vID[i]==2){
      df_output_longer2$varType[i] = "NPP"
    }
    if(df_output_longer2$vID[i]==3){
      df_output_longer2$varType[i] = "GPP"
    }
    if(df_output_longer2$vID[i]==4){
      df_output_longer2$varType[i] = "ecosystemR"
    }
  }
  
  if(df_output_longer2$group[i]=="group2"){
    if(df_output_longer2$vID[i]==1){
      df_output_longer2$varType[i] = "T"
    }
    if(df_output_longer2$vID[i]==2){
      df_output_longer2$varType[i] = "Gs"
    }
    if(df_output_longer2$vID[i]==3){
      df_output_longer2$varType[i] = "PWP"
    }
    if(df_output_longer2$vID[i]==4){
      df_output_longer2$varType[i] = "Anet"
    }
  }
}


# Delete fake combos

# Read in saved keys
pv_key1 <- readRDS("./models/03-Mixture-simple/df_realIDs_group1.RDS")
pv_key2 <- readRDS("./models/03-Mixture-simple/df_realIDs_group2.RDS")

# combine p and v IDs to use as key
df_output_longer2$pvID <- paste(df_output_longer2$pID, df_output_longer2$vID, sep = "")
pv_key1$pvID <- paste(pv_key1$pID, pv_key1$Var.ID, sep = "")
pv_key2$pvID <- paste(pv_key2$pID, pv_key2$Var.ID, sep = "")

df_output_longer3 <- df_output_longer2

for(i in 1:nrow(df_output_longer3)){
  
  if(df_output_longer3$group[i]=="group1"){
      
      if((df_output_longer3$pvID[i] %in% pv_key1$pvID)==F){
        df_output_longer3[i,] <- NA # Make full row NA if nonexistent
      }
    next
  }
  
  if(df_output_longer3$group[i]=="group2"){
    
      if((df_output_longer3$pvID[i] %in% pv_key2$pvID)==F){
        df_output_longer3[i,] <- NA # Make full row NA if nonexistent
      }
    next
  }
  
}

# Drop rows based on NA labels
df_output_longer3 <- df_output_longer3 %>% 
  drop_na(param)

# Combine all output together
df_output_longer4 <- full_join(df_output_longer1, df_output_longer3)



### df_output_longer pulse-level parameters connected to pulse-level data
pulse_table2 <- bind_rows(pulse_table)
df_output_longer5 <- df_output_longer4 %>%
  filter(param %in% c("t.peak","y.peak","bb", "mm","w")) %>%
  mutate(converged = ifelse(gel<1.2, "yes", "no"))

pulse_table2 <- right_join(pulse_table2, df_output_longer4, by=c("pID","varType"))

################## Look at semi-deterministic mixture model
################## Here we will also group to look at study/pulse-specific variables
################## reorganizing for "post-analysis"

pulse_table_final <- pulse_table2 %>%
  mutate(varGroup = ifelse(varType %in% c("ET", "T", "Gs", "PWP"), "water", "carbon")) %>%
  mutate(Sample.unit = ifelse(Study.ID==962, "individual", Sample.unit)) # Fill gaps: 962 is individual for Sample.unit

# Create table for - "Is there a pulse?"
df_all <- pulse_table_final %>%
  filter(param %in% c("w", "y.peak", "t.peak", "mm", "bb")) %>%
  dplyr::select(c(sID, pID, Study.ID, Pulse.ID, varType, varGroup, MAT.C.wc, MAT.C, MAP.mm.wc, MAP.mm, aridity, unitDuration, Sample.unit, Pulse.type, Pulse.amount.mm, preSWC, obs, param, mean, pc2.5, pc97.5, overlap0)) %>%
  pivot_wider(names_from = param, values_from = c(mean, pc2.5, pc97.5, overlap0))

# Make NAs if the parameters are just pulling from the prior
df_all$mean_y.peak <- ifelse(df_all$mean_w==0, NA, df_all$mean_y.peak)
df_all$mean_t.peak <- ifelse(df_all$mean_w==0, NA, df_all$mean_t.peak)
df_all$mean_mm <- ifelse(df_all$mean_w==1, NA, df_all$mean_mm)
df_all$mean_bb <- ifelse(df_all$mean_w==1, NA, df_all$mean_bb)
df_all$pc2.5_y.peak <- ifelse(df_all$mean_w==0, NA, df_all$pc2.5_y.peak)
df_all$pc2.5_t.peak <- ifelse(df_all$mean_w==0, NA, df_all$pc2.5_t.peak)
df_all$pc2.5_mm <- ifelse(df_all$mean_w==1, NA, df_all$pc2.5_mm)
df_all$pc2.5_bb <- ifelse(df_all$mean_w==1, NA, df_all$pc2.5_bb)
df_all$pc97.5_y.peak <- ifelse(df_all$mean_w==0, NA, df_all$pc97.5_y.peak)
df_all$pc97.5_t.peak <- ifelse(df_all$mean_w==0, NA, df_all$pc97.5_t.peak)
df_all$pc97.5_mm <- ifelse(df_all$mean_w==1, NA, df_all$pc97.5_mm)
df_all$pc97.5_bb <- ifelse(df_all$mean_w==1, NA, df_all$pc97.5_bb)

# Pulse response category rules
# a) w CI above 0.5 - ricker - classic pulse response (code 1)
# b) w CI overlapping 0.5 - intermediate pulse response (code 2)
# c) w CI below 0.5 - linear with sig slope - linear pulse response (code 3)
# d) w CI below 0.5 - linear with non sig slope - no pulse response (code 4)
catlist <- c()
for(i in 1:nrow(df_all)){
  if(df_all$pc2.5_w[i] > 0.5 & df_all$pc97.5_w[i] > 0.5){ # a
    catlist[i] <- 1
  } else if(df_all$pc2.5_w[i] < 0.5 & df_all$pc97.5_w[i] > 0.5){ # b
    catlist[i] <- 2
  } else if(df_all$pc2.5_w[i] < 0.5 & df_all$pc97.5_w[i] < 0.5){ # c and d
    #catlist[i] <- 3
    if(df_all$overlap0_mm[i] == F){ # sig slope
      catlist[i] <- 3
    } else if(df_all$overlap0_mm[i] == T){ # non sig slope
      catlist[i] <- 4
    }
    
  }
}

# create column for response category
df_all$response_cat <- catlist

# Make Ricker-related parameters NAs if there is no pulse (cat 4)
df_all$mean_y.peak <- ifelse(df_all$response_cat==4, NA, df_all$mean_y.peak)
df_all$mean_t.peak <- ifelse(df_all$response_cat==4, NA, df_all$mean_t.peak)
df_all$pc2.5_y.peak <- ifelse(df_all$response_cat==4, NA, df_all$pc2.5_y.peak)
df_all$pc2.5_t.peak <- ifelse(df_all$response_cat==4, NA, df_all$pc2.5_t.peak)
df_all$pc97.5_y.peak <- ifelse(df_all$response_cat==4, NA, df_all$pc97.5_y.peak)
df_all$pc97.5_t.peak <- ifelse(df_all$response_cat==4, NA, df_all$pc97.5_t.peak)

# Save data to output folder
save(pulse_table_final, file = "data_output/pulse_table_final.Rdata")
save(df_all, file = "data_output/df_all.Rdata")


