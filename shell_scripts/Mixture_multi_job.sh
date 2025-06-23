#!/bin/bash
# analysis.sh - an analysis program
# $1 (input) and $2 (output) are the first and second arguments to this script

# strip off the directory paths to get just the filename
# BASE=`basename $1`
groupname=$1
seed=$2

echo 
echo "run('$1');"
date

# Run script
#R --no-save < $1
Rscript ./models/03-Mixture-simple/01-run-model.R $groupname $seed
echo "run('$1'); done"
date
