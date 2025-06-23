#!/bin/bash
#SBATCH --job-name=mixture_%a
##SBATCH --workdir
#SBATCH --output=/scratch/egr65/Pulse-meta-analysis/log/Mixture_multi_%A_%a.log
#SBATCH --cpus-per-task=3
#SBATCH --time=3:00:00
#SBATCH --mem=40000
#SBATCH --mail-type=all
#SBATCH --mail-user=egr65@nau.edu
#SBATCH --array=1-2

### %A is monsoon job number %a is interior array index

module load R/4.1.2 # load a specific R version

chmod +x shell_scripts/Mixture_multi_job.sh # for permissions
chmod +x models/03-Mixture-simple/01-run-model.R # for permissions

groupname=$(sed -n "$SLURM_ARRAY_TASK_ID"p groupnameEND)
seed=$(sed -n "$SLURM_ARRAY_TASK_ID"p seedEND)

# Run the analysis
srun ./shell_scripts/Mixture_multi_job.sh $groupname $seed
