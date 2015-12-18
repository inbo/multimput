library(multimput)
library(snowfall)
library(plyr)

args <- commandArgs(TRUE)
message(args)

tempdir <- paste0(args[1], "/tmp") # nolint
datadir <- paste0(args[1], "/data") # nolint
n.cpu <- 20

message(tempdir)
message(datadir)

set.seed(1827519864)
n.run <- 200
n.site <- 100
n.period <- 6
n.year <- 24

seeds <- sample(.Machine$integer.max, n.run)

source(system.file("paper_simulation/inla_impute_2.R", package = "multimput"))
