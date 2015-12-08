#!/bin/bash
#
#PBS -N underhill
#PBS -o underhill.log
#PBS -e underhill.err
#PBS -l walltime=30:00:00
#PBS -l nodes=24:ppn=8
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

cp $ORIGDIR/underhill.R $WORKDIR/
cp -r $DATADIR/tmp/dataset $WORKDIR/tmp
cp -r $DATADIR/tmp/underhill $WORKDIR/tmp
cp -r $DATADIR/data $WORKDIR

Rscript --verbose underhill.R $DATADIR > underhill.log

cp -ru $WORKDIR/tmp/underhill $DATADIR/tmp
cp -ru $WORKDIR/*.log $DATADIR
cd

rm -Rf $WORKDIR

