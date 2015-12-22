singleRun <- function(run, path, seeds){
  require(MASS)
  require(multimput)

  type <- as.integer(substr(run, 10, 10)) - 1
  this.run <- as.integer(substr(run, 1, 4))
  set.seed(seeds[this.run])
  data.file <- sprintf(
    "%s/run_%s.rda",
    gsub("underhill$", "dataset", path),
    gsub("_.$", "_0", run)
  )
  load(data.file)
  dataset <- output$dataset
  rm(output)

  if (0 == (type %% 2)) {
    initial <- round(exp(coef(glm.nb(Observed ~ 1, data = dataset))))
    method <- "M"
  } else {
    initial <- 0
    method <- "0"
  }
  if (type %/% 2 == 0) {
    imputation  <- imputeUnderhill(
      data = dataset,
      formula = Observed ~ Year + Period + Site,
      initial = initial
    )
    method <- paste0("UH", method)
  } else {
    imputation  <- imputeUnderhillAltered(
      data = dataset,
      formula = Observed ~ Year + Period + Site,
      initial = initial
    )
    method <- paste0("UA", method)
  }
  total <- aggregate(
    imputation$data[, "Observed"],
    imputation$data[, c("Year", "Period")],
    FUN = sum
  )
  model <- glm.nb(x ~ 0 + Year + Period, data = total)
  output <- cbind(
    coef(summary(model))[, c("Estimate", "Std. Error")],
    confint(model)
  )
  output <- output[grep("Year", rownames(output)), ]
  output <- as.data.frame(output)
  output$Year <- as.integer(
    gsub("Year", "", rownames(output))
  )
  rownames(output) <- NULL
  output$Run <- this.run
  output$Pattern <- as.integer(substr(run, 6, 6))
  output$Missing <- as.integer(substr(run, 8, 8))
  output$Method <- method
  filename <- sprintf("%s/run_%s.rda", path, run)
  save(output, file = filename)
  filename
}

datasetpath <- paste(tempdir, "dataset", sep = "/")
to.do <- list.files(
  datasetpath,
  pattern = "^run_[[:digit:]]{4}_0_0_0\\.rda$"
)
to.do <- gsub("^run_", "", to.do)
to.do <- gsub("_0\\.rda$", "", to.do)
rm(datasetpath)

to.do <- sprintf("%s_%i", rep(to.do, each = 4), 1:4)

path <- paste(tempdir, "underhill", sep = "/")
if (file.exists(path)) {
  done <- list.files(
    path,
    pattern = "^run_[[:digit:]]{4}_[[:digit:]]_[[:digit:]]_[[:digit:]]\\.rda$"
  )
  done <- gsub("^run_", "", done)
  done <- gsub("\\.rda$", "", done)
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

done <- list.files(
  path,
  pattern = "^run_[[:digit:]]{4}_[[:digit:]]_[[:digit:]]_[[:digit:]]\\.rda$",
  full.names = TRUE
)
results.underhill <- do.call(
  rbind,
  lapply(done, function(x){
    load(x)
    output
  })
)
save(results.underhill, file = paste0(datadir, "/paper_underhill.rda")) #nolint

rm(done, results.underhill, path)
