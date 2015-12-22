singleRun <- function(run, path, seeds, n.site = 40, n.year = 24, n.period = 6){
  require(multimput)

  intercept <- 2
  trend <- 0.01
  sd.rw.year <- 0.1
  amplitude.period <- 1
  sd.phase.period <- 0.2
  sd.site <- 1
  sd.rw.site <- 0.02
  sd.noise <- 0.01
  size <- 2

  this.run <- as.integer(substr(run, 1, 4))
  set.seed(seeds[this.run])
  dataset <- generateData(
    n.year = n.year,
    n.period = n.period,
    n.site = n.site,
    intercept = intercept,
    trend = trend,
    sd.rw.year = sd.rw.year,
    amplitude.period = amplitude.period,
    sd.phase.period = sd.phase.period,
    sd.site = sd.site,
    sd.rw.site = sd.rw.site,
    sd.noise = sd.noise,
    size = size,
    year.factor = TRUE,
    period.factor = TRUE,
    site.factor = TRUE,
    n.run = 1,
    as.list = FALSE
  )
  dataset <- missingObserved(dataset)
  output <- list(
    dataset = dataset,
    parameter = c(
      n.year = n.year,
      n.period = n.period,
      n.site = n.site,
      intercept = intercept,
      trend = trend,
      sd.rw.year = sd.rw.year,
      amplitude.period = amplitude.period,
      sd.phase.period = sd.phase.period,
      sd.site = sd.site,
      sd.rw.site = sd.rw.site,
      sd.noise = sd.noise,
      size = size
    ),
    missing.pattern = "Observed"
  )
  filename <- sprintf("%s/run_%s.rda", path, run)
  save(output, file = filename)
  filename
}

to.do <- sprintf("%04i_0_0_0", seq_len(n.run))
path <- paste(tempdir, "dataset", sep = "/")
if(file.exists(path)){
  done <- list.files(path, pattern = "^run_[[:digit:]]{4}_0_0_0\\.rda$")
  done <- gsub("^run_", "", done)
  done <- gsub("\\.rda$", "", done)
  to.do <- to.do[!to.do %in% done]
  rm(done)
} else {
  dir.create(path)
}

if(n.cpu > 1){
  sfInit(parallel = TRUE, cpus = n.cpu)
  results <- sfClusterApplyLB(
    to.do,
    singleRun,
    path = path,
    seeds = seeds,
    n.site = n.site,
    n.year = n.year,
    n.period = n.period
  )
  sfStop()
} else {
  results <- lapply(
    to.do,
    singleRun,
    path = path,
    seeds = seeds,
    n.site = n.site,
    n.year = n.year,
    n.period = n.period
  )
}

rm(to.do, path, results, singleRun)
