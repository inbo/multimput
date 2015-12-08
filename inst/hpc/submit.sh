module load scripts
module load R/3.1.0-ictce-5.5.0
chmod 711 $VSC_HOME/multimput/inst/hpc/*.sh
dos2unix $VSC_HOME/multimput/inst/hpc/*.sh
R CMD INSTALL $VSC_HOME/multimput
qsub $VSC_HOME/multimput/inst/hpc/generateData.sh
qsub $VSC_HOME/multimput/inst/hpc/trim_create.sh
qsub $VSC_HOME/multimput/inst/hpc/truth.sh
qsub $VSC_HOME/multimput/inst/hpc/inla_impute.sh
qsub $VSC_HOME/multimput/inst/hpc/inla_impute3.sh
qsub $VSC_HOME/multimput/inst/hpc/inla_impute4.sh
qsub $VSC_HOME/multimput/inst/hpc/underhill.sh
qsub $VSC_HOME/multimput/inst/hpc/inla_index.sh
qstat
