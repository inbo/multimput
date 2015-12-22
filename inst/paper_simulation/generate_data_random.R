singleRun <- function(
  run,
  path,
  seeds,
  n.site = 40,
  n.year = 24,
  n.period = 6,
  missing.prop = 0.5
){
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
  proportion <- missing.prop[as.integer(substr(run, 8, 8))]
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
  dataset <- missingAtRandom(dataset, proportion = proportion)
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
      size = size,
      proportion = proportion
    ),
    missing.pattern = "At random"
  )
  filename <- sprintf("%s/run_%s.rda", path, run)
  save(output, file = filename)
  filename
}


missing.prop <- c(0.01, 0.05, 0.25, 0.5, 0.75)
to.do <- sprintf(
  "%04i_1_%01i_0",
  rep(seq_len(n.run), length(missing.prop)),
  rep(seq_along(missing.prop), each = n.run)
)

path <- paste(tempdir, "dataset", sep = "/")
if(file.exists(path)){
  done <- list.files(
    path,
    pattern = "^run_[[:digit:]]{4}_1_[[:digit:]]_0\\.rda$"
  )
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
    n.period = n.period,
    missing.prop = missing.prop
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
    n.period = n.period,
    missing.prop = missing.prop
  )
}

rm(to.do, path, results, singleRun)
