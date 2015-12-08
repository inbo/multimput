#!/bin/bash
#
#PBS -N trim_create
#PBS -o trim_create.log
#PBS -e trim_create.err
#PBS -l walltime=00:12:00
#PBS -l nodes=10:ppn=1
#PBS -m abe
#

ulimit -s unlimited
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

cp $ORIGDIR/trim_create.R $WORKDIR/
cp -r $DATADIR/tmp/dataset $WORKDIR/tmp
cp -r $DATADIR/tmp/trim $WORKDIR/tmp
cp -r $DATADIR/data $WORKDIR

Rscript --verbose trim_create.R $DATADIR > truth_create.log

cp -ru $WORKDIR/tmp/trim $DATADIR/tmp
cp -ru $WORKDIR/*.log $DATADIR
cd

rm -Rf $WORKDIR

