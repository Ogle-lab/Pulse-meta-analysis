# Precipitation pulse dynamics are not ubiquitous: A global meta-analysis of plant and ecosystem carbon- and water-related pulse responses

## Folders

1. data_raw

2. data_clean

3. data_output

4. scripts
- 00_extract_world_clim_and_terra.R
- 01_clean_data.R
  - This script makes the terminology collected across papers more consistent and corrects some typos.
- 02_explore_pulse_plotting.R
  - This script is exploratory; to see the general pulse response shapes across the collected papers.
- 03_populate_matrix.R
  - This script is exploratory; to see which pulse responses are studied across the collected papers.
- 04_clean_vars.R
  - This script organizes the extracted data in preparation for model runs.
- 05_determine_mixture_weights.R
  - This script assigns mixture weights for pulse-experiments associated with poor convergence and mixing (see supplemental information for description).
- 06_is_there_a_pulse.R
  - This script uses the Bayesian model output to determine which category (e.g., classic, intermediate, linear, or no pulse) a pulse response falls under.
- 07_post_analysis.R
  - This script runs 4 generalized linear models to answer questions 1-3 and partially creates Figure 6 (code for a-c).
- functions.R
  - This script contains self-made helper functions that are called on throughout the workflow.

5. models
- 01-Ricker-simple
    - 01-run-model.R
      - This script runs Ricker_model.R.
    - Ricker_model.R
      - This script is the JAGS code for the Ricker model.
- 02-Linear-simple
    - 01-run-model.R
      - This script runs Linear_model.R.
    - Linear_model.R
      - This script is the JAGS code for the linear model.
- 03-Mixture-simple
    - 01-run-model.R
      - This script runs Mixture_model.R, Mixture_model_fixed_w.R, or Mixture_model_multivariate_fixed_w depending on the specified arguments.
    - Mixture_model.R
      - This script is the JAGS code for the simple mixture model with all stochastic weights.
    - Mixture_model_fixed_w.R
      - This script is the JAGS code for the univariate mixture model with fixed weights.
    - Mixture_model_multivariate_fixed_w.R
      - This script is the JAGS code for the multivariate mixture model with fixed weights.


6. figures
- This folder contains various scripts for making all figures and figure files.

6. shell_scripts
- shell scripts that were used to run the models on a HPC
- the files outside the folders (resnameEND and seedEND) are called by the shell scripts to parallelize the model runs on the HPC
