#!/bin/bash
#
#PBS -N generateData
#PBS -o generateData.log
#PBS -e generateData.err
#PBS -l walltime=00:20:00
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

cp $ORIGDIR/generateData.R $WORKDIR/
cp -r $DATADIR/tmp/dataset $WORKDIR/tmp

Rscript --verbose generateData.R > generateData.log

cp -ru $WORKDIR/tmp/dataset $DATADIR/tmp

cd

rm -Rf $WORKDIR

