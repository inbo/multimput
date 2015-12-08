#!/bin/bash
#
#PBS -N inla_impute2
#PBS -o inla_impute2.log
#PBS -e inla_impute2.err
#PBS -l walltime=24:00:00
#PBS -l nodes=20:ppn=4
#PBS -m abe
#PBS -l vmem=50gb
#

ulimit -v -s unlimited
module load scripts

module load R/3.1.0-ictce-5.5.0

DATADIR=$VSC_HOME/multimput_sims
ORIGDIR=$VSC_HOME/multimput/inst/hpc
WORKDIR=$VSC_SCRATCH_DELCATTY/$PBS_JOBID

echo Hostname: $(hostname)
echo ORIGDIR: $ORIGDIR
echo DATADIR: $DATADIR
echo WORKDIR: $WORKDIR

mkdir -p $WORKDIR/tmp
cd $WORKDIR
pwd

cp $ORIGDIR/inla_impute2.R $WORKDIR/
cp -r $DATADIR/tmp/dataset $WORKDIR/tmp
cp -r $DATADIR/tmp/inla $WORKDIR/tmp
cp -r $DATADIR/data $WORKDIR

Rscript --verbose inla_impute2.R $DATADIR > inla_impute2.log

cp -ru $WORKDIR/tmp $DATADIR
cp -ru $WORKDIR/data $DATADIR
cp -ru $WORKDIR/*.log $DATADIR
cd

rm -Rf $WORKDIR

