### Initial Ricker model for ET subset of response variables

library(rjags)
load.module('dic')
library(tidyverse)
library(ggplot2)
library(mcmcplots)

load("models/01-test-Ricker/inputET.Rdata")


# Create study_pulse combination, create integer sID

pulse_table_short <- et2 %>%
  expand(nesting(Study.ID, Pulse.ID)) %>%
  mutate(sID = as.numeric(factor(Study.ID))) %>%
  arrange(Study.ID) %>%
  tibble::rownames_to_column() %>%
  rename(pID = rowname) %>%
  mutate(pID = as.numeric(pID)) %>%
  relocate(pID, .after = sID)

# pulse_table <- expand.grid(unique(et2$Study.ID),
#                            unique(et2$Pulse.ID)) %>%
#   rename(Study.ID = Var1,
#          Pulse.ID = Var2) %>%
#   mutate(sID = as.numeric(factor(Study.ID))) %>%
#   arrange(Study.ID) %>%
#   tibble::rownames_to_column() %>%
#   rename(pID = rowname)

# Join with et2
et2 <- et2 %>%
  left_join(pulse_table_short) %>%
  relocate(sID, pID)

et2 %>%
  filter(sID %in% c(2, 4:5, 8:11, 15)) %>%
  ggplot(aes(x = Days.relative.to.pulse + 1,
             y = LRR)) +
  geom_point(aes(color = as.factor(sID))) +
  geom_hline(yintercept = 0) +
  theme_bw()


ggplot(et2, aes(x = Days.relative.to.pulse + 1,
                y = LRR)) +
  geom_errorbar(aes(ymin = LRR - sqrt(poolVar),
                    ymax = LRR + sqrt(poolVar),
                    color = as.factor(pID)),
                width = 0) +
  geom_point(aes(color = as.factor(pID))) +
  geom_hline(yintercept = 0) +
  facet_wrap(~sID, scales = "free_y") +
  theme_bw() +
  guides(color = "none")

# Prepare data list
datlist <- list(et = et2$LRR,
                t = et2$Days.relative.to.pulse + 1,
                pID = et2$pID,
                sID = pulse_table_short$sID,
                N = nrow(et2),
                Npulse = nrow(pulse_table_short),
                Nstudy = max(et2$sID))

# Initial values
inits <- function(){
  list(M.Lt.peak = rnorm(1, 0, 10),
       M.y.peak = rnorm(1, 0, 10),
       sig.Lt.peak = runif(1, 0, 1),
       sig.y.peak = runif(1, 0, 1),
       S.Lt.peak = runif(1, 0, 1),
       S.y.peak = runif(1, 0, 1),
       tau = runif(1, 0, 1))
}
initslist <- list(inits(), inits(), inits())

# Initialize JAGS model
jm <- jags.model("models/01-test-Ricker/model1.jags",
                 data = datlist,
                 inits = initslist,
                 n.chains = 3)

update(jm, 10000)

# Run and monitor parameters
params <- c("deviance", "Dsum",
            "mu.Lt.peak", "mu.y.peak", "mu.t.peak",
            "M.Lt.peak", "M.y.peak", "M.t.peak",
            "t.peak", "y.peak",
            "sig", "sig.Lt.peak", "sig.y.peak", "S.Lt.peak", "S.y.peak",
            "tau")

jm_coda <- coda.samples(jm, variable.names = params,
                        n.iter = 9000, thin = 3)

# Plot output
mcmcplot(jm_coda, parms = c("deviance", "Dsum",
                            "M.t.peak", "M.y.peak", 
                            "S.Lt.peak", "S.y.peak"))
mcmcplot(jm_coda, parms = c("t.peak", "y.peak",
                            "mu.t.peak", "mu.y.peak",
                            "sig", "sig.Lt.peak", "sig.y.peak",
                            "tau"))

ricker = function(x, a = 1, b = 1) {
  a * x * exp(-b * x)
}
plot(seq(-1, 10, 0.1), ricker(seq(-1, 10, 0.1), 1.2, 0.8))
