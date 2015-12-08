library(multimput)
library(snowfall)
library(plyr)

tempdir <- "tmp"
datadir <- "data"
n.cpu <- 10

set.seed(1827519864)
n.run <- 200
n.site <- 100
n.period <- 6
n.year <- 24

seeds <- sample(.Machine$integer.max, n.run)

source(system.file("paper_simulation/generate_data.R", package = "multimput"))
source(system.file("paper_simulation/generate_data_design.R", package = "multimput"))
source(system.file("paper_simulation/generate_data_random.R", package = "multimput"))
