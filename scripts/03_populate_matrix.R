# Populate matrix of overlapping parameters
# Diagonal: # of studies with just single variable
# Above: # of studies with both variables
# Below: # of observations with both variables

library(readr)
library(dplyr)


# Import data
d_clean <- read_csv("./data_clean/Clean_record_info.csv") %>%
  mutate(study_pulse = paste0(Study.ID, "_", Pulse.ID),
         study_pulse_record = paste0(study_pulse, "_", 
                                     Time.relative.to.pulse, "_",
                                     Time.relative.to.pulse.unit))

str(d_clean)

# check study pulse #
test <- d_clean %>%
  group_by(study_pulse) %>%
  summarize(nvar = length(unique(varType))) %>%
  filter(nvar == 2) %>%
  nrow()
#385

# Important variables
# variables <- c("WUE", "ET", "T", "Gs", "PWP",
#                "ecosystemR", "abovegroundR", "belowgroundR",
#                "NPP", "GPP", "Anet") # old

variables <- c("ET", "T", "Gs", "PWP",
               "ecosystemR", "belowgroundR",
               "NPP", "GPP", "Anet")

# Calculate diagonal
diag <- d_clean  %>%
  mutate(varType = factor(varType, levels = variables)) %>%
  group_by(varType) %>%
  summarize(n_record = n(),
            n_pulse = length(unique(study_pulse)),
            n_study = length(unique(Study.ID))) %>%
  filter(varType %in% variables) 

count_mat1 <- matrix(NA, 
                    nrow = length(variables),
                    ncol = length(variables))
count_mat2 <- matrix(NA, 
                     nrow = length(variables),
                     ncol = length(variables))
count_mat3 <- matrix(NA, 
                     nrow = length(variables),
                     ncol = length(variables))
count_mat4 <- matrix(NA, 
                     nrow = length(variables),
                     ncol = length(variables))

for(i in 1:length(variables)) {
  for(j in 1:length(variables)) {
     if(i == j) { # diagonal
       count_mat1[i, j] <- diag$n_record[i]
       count_mat2[i, j] <- diag$n_pulse[i]
       count_mat3[i, j] <- diag$n_pulse[i]
       count_mat4[i, j] <- diag$n_study[i]
     } else if(i > j){ # lower left half
        o_study <- d_clean %>%
         filter(varType == variables[i] | varType == variables[j]) %>%
         group_by(Study.ID) %>%
         summarize(nvar = length(unique(varType))) %>%
         filter(nvar == 2) %>%
         nrow()
        
        o_pulse <- d_clean %>%
          filter(varType == variables[i] | varType == variables[j]) %>%
          group_by(study_pulse) %>%
          summarize(nvar = length(unique(varType))) %>%
          filter(nvar == 2) %>%
          nrow()
        
        count_mat1[i, j] <- o_study
        count_mat2[i, j] <- o_pulse
        count_mat3[i, j] <- o_study
        count_mat4[i, j] <- o_study
        
     } else if(j > i) { # upper right half
       o_pulse2 <- d_clean %>%
         filter(varType == variables[i] | varType == variables[j]) %>%
         group_by(study_pulse) %>%
         summarize(nvar = length(unique(varType))) %>%
         filter(nvar == 2) %>%
         nrow()
       
       o_record <- d_clean %>%
         filter(varType == variables[i] | varType == variables[j]) %>%
         group_by(study_pulse_record) %>%
         summarize(nvar = length(unique(varType))) %>%
         filter(nvar == 2) %>%
         nrow() 
       
       count_mat1[i, j] <- o_record
       count_mat2[i, j] <- o_record
       count_mat3[i, j] <- o_pulse2
       count_mat4[i, j] <- o_pulse2
     }
  }
}

# Convert to dataframe, label
count1_df <- data.frame(count_mat1) 
colnames(count1_df) <- variables
rownames(count1_df) <- variables

count2_df <- data.frame(count_mat2) 
colnames(count2_df) <- variables
rownames(count2_df) <- variables

count3_df <- data.frame(count_mat3) 
colnames(count3_df) <- variables
rownames(count3_df) <- variables

count4_df <- data.frame(count_mat4) 
colnames(count4_df) <- variables
rownames(count4_df) <- variables

# write out
if(!dir.exists("data_clean/tables")) {
  dir.create("data_clean/tables")
}
write.csv(count1_df, file = "data_clean/tables/matrix_study_record.csv")

write.csv(count2_df, file = "data_clean/tables/matrix_pulse_record.csv")

write.csv(count3_df, file = "data_clean/tables/matrix_studyleft_pulseright.csv")

write.csv(count4_df, file = "data_clean/tables/matrix_studyleft_pulseright_studydiag.csv")


# Populate matrix of the number of overlapping covariates
# Diagonal: # of records that overlap
# Above: # of studies with both variables
# Below: # of observations with both variables

# Important covariates for soil and air
# response variables on x-axis (top)
# covariates on y-axis (side)
unique(d_clean$varType)
cov_soil <- c("Tsoil", "SWC", "SWP")
cov_air <- c("Tair", "PAR", "RH", "VPD")

# Air covariates matrix
cov_mat_air <- matrix(NA, 
                     nrow = length(cov_air),
                     ncol = length(variables))

for(i in 1:length(cov_air)){
  for(j in 1:length(variables)){
    
    # check if study has both cov_air and variables
    o_study <- d_clean %>%
      filter(varType == cov_air[i] | varType == variables[j]) %>%
      group_by(Study.ID) %>%
      summarise(nvar = length(unique(varType))) %>%
      filter(nvar == 2)
    
    # if both are present, count the number of study pulses of variable
    # assuming cov_air encompasses all pulses
    o_pulse <- d_clean %>%
      filter(Study.ID %in% o_study$Study.ID & varType == variables[j]) %>%
      group_by(study_pulse) %>%
      summarise(npulse = length(unique(Pulse.ID))) %>%
      nrow()
    
    cov_mat_air[i,j] <- o_pulse
  }
}





# Soil covariates matrix
cov_mat_soil <- matrix(NA, 
                     nrow = length(cov_soil),
                     ncol = length(variables))

for(i in 1:length(cov_soil)){
  for(j in 1:length(variables)){
    
    # check if study has both cov_air and variables
    o_study <- d_clean %>%
      filter(varType == cov_soil[i] | varType == variables[j]) %>%
      group_by(Study.ID) %>%
      summarise(nvar = length(unique(varType))) %>%
      filter(nvar == 2)
    
    # if both are present, count the number of study pulses of variable
    # assuming cov_air encompasses all pulses
    o_pulse <- d_clean %>%
      filter(Study.ID %in% o_study$Study.ID & varType == variables[j]) %>%
      group_by(study_pulse) %>%
      summarise(npulse = length(unique(Pulse.ID))) %>%
      nrow()
    
    cov_mat_soil[i,j] <- o_pulse
  }
}

# Convert air and soil matrices to dataframes, label
# number of pulses with both the env variable and response
cov_mat_air_df <- data.frame(cov_mat_air) 
colnames(cov_mat_air_df) <- variables
rownames(cov_mat_air_df) <- cov_air

cov_mat_soil_df <- data.frame(cov_mat_soil) 
colnames(cov_mat_soil_df) <- variables
rownames(cov_mat_soil_df) <- cov_soil


# write out
if(!dir.exists("data_clean/tables")) {
  dir.create("data_clean/tables")
}
write.csv(cov_mat_air_df, file = "data_clean/tables/matrix_air_covariates.csv")

write.csv(cov_mat_soil_df, file = "data_clean/tables/matrix_soil_covariates.csv")

write.csv(count1_df, file = "data_clean/tables/count_1.csv")
write.csv(count2_df, file = "data_clean/tables/count_2.csv")



