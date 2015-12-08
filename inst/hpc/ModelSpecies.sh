#!/bin/bash
#
#PBS -N ModelSpecies
#PBS -o RUN.log
#PBS -e RUN.err
#PBS -q debug
#PBS -l walltime=24:00:00
#PBS -l nodes=1:ppn=8
#PBS -m ae
#

ulimit -s unlimited
module load scripts

module swap cluster/raichu
module load R/3.0.2-ictce-5.5.0

ORIGDIR=$VSC_DATA/multimput
WORKDIR=$VSC_SCRATCH_NODE/$PBS_JOBID

echo Hostname: $(hostname)
echo ORIGDIR: $ORIGDIR
echo WORKDIR: $WORKDIR

mkdir -p $WORKDIR
cd $WORKDIR
pwd

cp $ORIGDIR/ModelSpecies.R $WORKDIR/
cp $ORIGDIR/waterfowl.rda $WORKDIR/

Rscript --verbose ModelSpecies.R > ModelSpecies.log

cp ModelSpecies.rda $ORIGDIR
cp ModelSpecies.log $ORIGDIR

cd

rm -Rf $WORKDIR

