singleRun <- function(run, path, seeds){
  require(multimput)

  this.run <- as.integer(substr(run, 1, 4))
  set.seed(seeds[this.run])
  data.file <- sprintf(
    "%s/run_%s.rda",
    gsub("inla$", "dataset", path),
    run
  )
  load(data.file)
  dataset <- output$dataset # nolint
  rm(output) # nolint

  imputation  <- try(imputeINLA(
    data = dataset,
    formula =
      Observed ~ f(Year, model = "rw1", replicate = as.integer(Site)) + Period,
    n.sim = 199
  ))
  if (class(imputation) == "try-error") {
    return()
  }
  filename <- sprintf("%s/imp_%s_rw.rda", path, run)
  save(imputation, file = filename)
  filename
}

datasetpath <- paste(tempdir, "dataset", sep = "/")
to.do <- list.files(datasetpath, pattern = "^run_[[:digit:]]{4}_0_0_0\\.rda$")
to.do <- gsub("^run_", "", to.do)
to.do <- gsub("\\.rda$", "", to.do)
rm(datasetpath)

path <- paste(tempdir, "inla", sep = "/")
if (file.exists(path)) {
  done <- list.files(
    path,
    pattern =
      "^imp_[[:digit:]]{4}_[[:digit:]]_[[:digit:]]_[[:digit:]]_rw\\.rda$"
  )
  done <- gsub("^imp_", "", done)
  done <- gsub("_rw\\.rda$", "", done)
  to.do <- to.do[!to.do %in% done]
  rm(done)
} else {
  dir.create(path)
}

if (n.cpu > 1) {
  sfInit(parallel = TRUE, cpus = n.cpu)
  results <- sfClusterApplyLB(
    to.do,
    singleRun,
    path = path,
    seeds = seeds
  )
  sfStop()
} else {
  results <- lapply(
    to.do,
    singleRun,
    path = path,
    seeds = seeds
  )
}

rm(to.do, results, singleRun)
