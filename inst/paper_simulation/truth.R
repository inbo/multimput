singleRun <- function(run, path, seeds){
  require(MASS)
  require(multimput)

  this.run <- as.integer(substr(run, 1, 4))
  set.seed(seeds[this.run])
  data.file <- sprintf(
    "%s/run_%s.rda",
    gsub("truth$", "dataset", path),
    run
  )
  load(data.file)
  dataset <- output$dataset
  rm(output)

  total <- aggregate(
    dataset[, "Count"],
    dataset[, c("Year", "Period")],
    FUN = sum
  )
  model <- glm.nb(x ~ 0 + Year + Period, data = total)
  output <- cbind(
    coef(summary(model))[, c("Estimate", "Std. Error")],
    confint(model)
  )
  period <- c(0, output[grep("Period", rownames(output)), "Estimate"]) # nolint
  output <- output[grep("Year", rownames(output)), ]

  model2 <- glm.nb(x ~ 0 + Year, data = total)
  output <- cbind(
    output,
    coef(summary(model2))[, c("Estimate", "Std. Error")],
    confint(model2)
  )
  output <- as.data.frame(output)

  colnames(output)[5:8] <- paste0(colnames(output)[5:8], "bis")
  output$Year <- as.integer(
    gsub("Year", "", rownames(output))
  )
  rownames(output) <- NULL
  output$Run <- this.run
  output$Years <- as.integer(substr(run, 8, 8))
  output$Sites <- as.integer(substr(run, 10, 10))

  filename <- sprintf("%s/run_%s.rda", path, run)
  save(output, file = filename)
  filename
}

datasetpath <- paste(tempdir, "dataset", sep = "/")
to.do <- list.files(
  datasetpath,
  pattern = "^run_[[:digit:]]{4}_[02]_[[:digit:]]_[[:digit:]]\\.rda$"
)
to.do <- gsub("^run_", "", to.do)
to.do <- gsub("\\.rda$", "", to.do)
rm(datasetpath)

path <- paste(tempdir, "truth", sep = "/")
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
results.truth <- do.call(
  rbind,
  lapply(done, function(x){
    load(x)
    output
  })
)
save(results.truth, file = paste0(datadir, "/paper_truth.rda")) #nolint

rm(done, results.truth, path)
