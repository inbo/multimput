#source(system.file("paper_simulation/run_simulations.R", package = "multimput"))

library(multimput)
library(snowfall)
library(plyr)

if(R.Version()$platform %in% c("x86_64-w64-mingw32", "i386-w64-mingw32")){
#   tempdir <- "c:/Users/thierry_onkelinx/ownCloud/documents/artikels/simulation_multimput"
#   datadir <- "c:/Users/thierry_onkelinx/ownCloud/documents/artikels/multimput/data"
  tempdir <- "c:/tmp/multimput_definitief"
  datadir <- "c:/tmp/multimput"
  n.cpu <- 4
} else {
  tempdir <- "~/artikels/simulation_multimput"
  datadir <- "~/packages/multimput/data"
  n.cpu <- 1
}

set.seed(1827519864)
n.run <- 200
n.site <- 100
n.period <- 6
n.year <- 24

seeds <- sample(.Machine$integer.max, n.run)

message("generate data with observed missing pattern")
utils::flush.console()
source(system.file("paper_simulation/generate_data.R", package = "multimput"))

message("generate data with random missing pattern")
utils::flush.console()
source(system.file("paper_simulation/generate_data_random.R", package = "multimput"))

message("generate data with observed missing pattern and several designs")
utils::flush.console()
source(system.file("paper_simulation/generate_data_design.R", package = "multimput"))

message("create TRIM files")
utils::flush.console()
source(system.file("paper_simulation/trim_create.R", package = "multimput"))

message("calculate true indices")
utils::flush.console()
source(system.file("paper_simulation/truth.R", package = "multimput"))

message("create imputations with INLA")
utils::flush.console()
source(system.file("paper_simulation/inla_impute.R", package = "multimput"))

# message("create imputations with INLA with random walk")
# source(system.file("paper_simulation/inla_impute_2.R", package = "multimput"))
# utils::flush.console()

message("create imputations with true mean")
utils::flush.console()
source(system.file("paper_simulation/inla_impute_3.R", package = "multimput"))

message("calculate imputed indices")
utils::flush.console()
source(system.file("paper_simulation/inla_index.R", package = "multimput"))

message("calculate underhill indices")
utils::flush.console()
source(system.file("paper_simulation/underhill.R", package = "multimput"))

message("read TRIM output")
utils::flush.console()
source(system.file("paper_simulation/trim_output.R", package = "multimput"))

message("compare example dataset")
utils::flush.console()
source(system.file("paper_simulation/compare_example_dataset.R", package = "multimput"))

