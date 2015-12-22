singleRun <- function(run, path, seeds, n.imputation){
  require(multimput)

  this.run <- as.integer(substr(run, 1, 4))
  if (length(grep("tr", run)) == 1) {
    n.imputations <- n.imputation[as.integer(substr(run, 15, 15))]
    model <- 1
  } else if (length(grep("rw", run)) == 1) {
    n.imputations <- n.imputation[as.integer(substr(run, 15, 15))]
    model <- 2
  } else if (length(grep("sl", run)) == 1) {
    n.imputations <- n.imputation[as.integer(substr(run, 15, 15))]
    model <- 3
  } else {
    n.imputations <- n.imputation[as.integer(substr(run, 12, 12))]
    model <- 0
  }
  if (substr(run, 6, 6) == "1") {
    pattern <- 0
    missingness <- as.integer(substr(run, 8, 8))
    years <- 0
    sites <- 0
  } else {
    pattern <- 1
    missingness <- 0
    years <- as.integer(substr(run, 8, 8))
    sites <- as.integer(substr(run, 10, 10))
  }
  set.seed(seeds[this.run])

  data.file <- sprintf(
    "%s/run_%s.rda",
    gsub("inla$", "dataset", path),
    substr(run, 1, 10)
  )
  load(data.file)
  dataset <- output$dataset
  rm(output)
  data.file <- sprintf(
    "%s/imp_%s.rda",
    path,
    substr(run, 1, nchar(run) - 2)
  )
  load(data.file)

  total <- imputedTotals(
    data = dataset,
    imputations = imputation[, seq_len(n.imputations)],
    variable = "Observed",
    rhs = "Year + Period"
  )
  output <- summarizeImputationGLM.nb(data = total, rhs = "0 + Year + Period")
  colnames(output) <- c("Parameter", "Estimate", "Std. Error")
  output <- output[grep("Year", output$Parameter), ]
  output$Year <- as.integer(gsub("Year", "", output$Parameter))
  output$Parameter <- NULL
  output$"2.5 %" <- qnorm(
    0.025,
    mean = output$Estimate,
    sd = output$"Std. Error"
  )
  output$"97.5 %" <- qnorm(
    0.975,
    mean = output$Estimate,
    sd = output$"Std. Error"
  )
  output$Run <- this.run
  output$Pattern <- pattern
  output$Missing <- missingness
  output$Model <- model
  output$Imputations <- n.imputations
  output$Years <- years
  output$Sites <- sites
  filename <- sprintf("%s/run_%s.rda", path, run)
  save(output, file = filename)
  filename
}

path <- paste(tempdir, "inla", sep = "/")
to.do <- list.files(path, pattern = "^imp_[[:digit:]]{4}_0_0_0\\.rda$")
to.do <- gsub("^imp_", "", to.do)
to.do <- gsub("\\.rda$", "", to.do)
n.imputation <- c(9, 19, 49, 99, 199)
to.do <- sprintf(
  "%s_%01i",
  rep(to.do, each = length(n.imputation)),
  seq_along(n.imputation)
)

extra <- list.files(
  path,
  pattern = "^imp_[[:digit:]]{4}_[12]_[[:digit:]]_[[:digit:]]\\.rda$"
)
extra <- gsub("^imp_", "", extra)
extra <- gsub("\\.rda$", "", extra)
extra <- sprintf(
  "%s_%01i",
  extra,
  length(n.imputation)
)
to.do <- c(to.do, extra)

extra <- list.files(
  path,
  pattern = "^imp_[[:digit:]]{4}_(0_0|1_4)_0_(tr|rw|sl)\\.rda$"
)
extra <- gsub("^imp_", "", extra)
extra <- gsub("\\.rda$", "", extra)
extra <- sprintf(
  "%s_%01i",
  extra,
  length(n.imputation)
)
to.do <- c(to.do, extra)

done <- list.files(path, pattern = "^run_.*\\.rda$")
done <- gsub("^run_", "", done)
done <- gsub("\\.rda$", "", done)
to.do <- to.do[!to.do %in% done]
rm(done)

if(n.cpu > 1){
  sfInit(parallel = TRUE, cpus = n.cpu)
  results <- sfClusterApplyLB(
    to.do,
    singleRun,
    path = path,
    seeds = seeds,
    n.imputation = n.imputation
  )
  sfStop()
} else {
  results <- lapply(
    to.do,
    singleRun,
    path = path,
    seeds = seeds,
    n.imputation = n.imputation
  )
}

rm(to.do, results, singleRun)

done <- list.files(
  path,
  pattern = "^run_[[:digit:]]{4}_.*\\.rda$",
  full.names = TRUE
)
results.inla <- do.call(
  rbind,
  lapply(done, function(x){
    load(x)
    output
  })
)
save(results.inla, file = paste0(datadir, "/paper_inla.rda")) #nolint
rm(done, results.inla, path)
