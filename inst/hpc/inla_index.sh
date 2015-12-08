#!/bin/bash
#
#PBS -N inla_index
#PBS -o inla_index.log
#PBS -e inla_index.err
#PBS -l walltime=03:30:00
#PBS -l nodes=5:ppn=4
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

cp $ORIGDIR/inla_index.R $WORKDIR/
cp -r $DATADIR/tmp/dataset $WORKDIR/tmp
cp -r $DATADIR/tmp/inla $WORKDIR/tmp
cp -r $DATADIR/data $WORKDIR

Rscript --verbose inla_index.R $DATADIR > inla_index.log

cp -ru $WORKDIR/tmp/inla/run*.* $DATADIR/tmp/inla
cp -ru $WORKDIR/*.log $DATADIR
cd

rm -Rf $WORKDIR

