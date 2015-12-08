library(multimput)
library(snowfall)
library(plyr)

args <- commandArgs(TRUE)
tempdir <- paste0(args[1], "/tmp")
datadir <- paste0(args[1], "/data")
message(tempdir)
n.cpu <- 20

set.seed(1827519864)
n.run <- 200
n.site <- 100
n.period <- 6
n.year <- 24

seeds <- sample(.Machine$integer.max, n.run)

source(system.file("paper_simulation/trim_create.R", package = "multimput"))
