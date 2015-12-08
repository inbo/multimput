#!/bin/bash
#
#PBS -N inla_impute3
#PBS -o inla_impute3.log
#PBS -e inla_impute3.err
#PBS -l walltime=00:06:00
#PBS -l nodes=20:ppn=4
#PBS -m abe
#

ulimit -v -s unlimited
module load scripts

module load R/3.1.0-ictce-5.5.0

DATADIR=$VSC_DATA/multimput_sims
ORIGDIR=$VSC_HOME/multimput/inst/hpc
WORKDIR=$VSC_SCRATCH_DELCATTY/$PBS_JOBID

echo Hostname: $(hostname)
echo ORIGDIR: $ORIGDIR
echo WORKDIR: $WORKDIR

mkdir -p $WORKDIR/tmp
cd $WORKDIR
pwd

cp $ORIGDIR/inla_impute3.R $WORKDIR/
cp -r $DATADIR/tmp/dataset $WORKDIR/tmp
cp -r $DATADIR/tmp/inla $WORKDIR/tmp
cp -r $DATADIR/data $WORKDIR

Rscript --verbose inla_impute3.R $WORKDIR > inla_impute3.log

cp -ru $WORKDIR/tmp/inla $DATADIR/tmp
cp -ru $WORKDIR/*.log $DATADIR
cd

rm -Rf $WORKDIR

