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
  dataset <- output$dataset
  size <- output$parameter["size"]
  rm(output)

  imputation <- imputeTruth(
    data = dataset, 
    size = size,
    mean = "Mu",
    n.sim = 199
  )
  filename <- sprintf("%s/imp_%s_tr.rda", path, run)
  save(imputation, file = filename)
  filename
}

datasetpath <- paste(tempdir, "dataset", sep = "/")
to.do <- list.files(datasetpath, pattern = "^run_[0123456789]{4}_(0_0|1_4)_0\\.rda$")
to.do <- gsub("^run_", "", to.do)
to.do <- gsub("\\.rda$", "", to.do)
rm(datasetpath)

path <- paste(tempdir, "inla", sep = "/")
if(file.exists(path)){
  done <- list.files(path, pattern = "^imp_[0123456789]{4}_[0123456789]_[0123456789]_[0123456789]_tr\\.rda$")
  done <- gsub("^imp_", "", done)
  done <- gsub("_tr\\.rda$", "", done)
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
