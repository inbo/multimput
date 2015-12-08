singleRun <- function(run, path, seeds, n.year){
  library(plyr)
  
  this.run <- as.integer(substr(run, 1, 4))
  set.seed(seeds[this.run])
  
  data.file <- sprintf(
    "%s/run_%s.rda", 
    gsub("trim$", "dataset", path), 
    run
  )
  load(data.file)
  dataset <- output$dataset
  rm(output)
  
  dataset.trim <- ddply(
    dataset,
    .(Site, Year),
    summarize,
    Count = sum(Observed, na.rm = TRUE),
    Weights = 1 / sum(!is.na(Observed))
  )
  dataset.trim$Site <- as.integer(dataset.trim$Site)
  dataset.trim$Year <- as.integer(dataset.trim$Year)
  dataset.trim$Count[is.infinite(dataset.trim$Weights)] <- -1
  dataset.trim$Weights[is.infinite(dataset.trim$Weights)] <- 1
  
  filename <- sprintf("run_%s.poi", run)
  write.table(
    dataset.trim, 
    file = paste(path, filename, sep = "/"), 
    row.names = FALSE, 
    col.names = FALSE,
    quote = FALSE
  )
  text <- paste("FILE", filename, "\nTITLE", filename, "\nNTIMES", n.year, "\nNCOVARS 0\nLABELS \nEnd\nMISSING -1\nWEIGHT Present\n\nWEIGHTING on\nSERIALCOR on\nOVERDISP on\nMODEL 3\nSTEPWISE off\nRUN\n")
  writeLines(
    text, 
    paste(
      path, 
      gsub("poi", "TCF", filename),
      sep = "/"
    )
  )

  #TRIM files based on the complete dataset
  dataset.trim <- ddply(
    dataset,
    .(Site, Year),
    summarize,
    Count = sum(Count, na.rm = TRUE),
    Weights = 1 / length(Observed)
  )
  dataset.trim$Site <- as.integer(dataset.trim$Site)
  dataset.trim$Year <- as.integer(dataset.trim$Year)
  dataset.trim$Count[is.infinite(dataset.trim$Weights)] <- -1
  dataset.trim$Weights[is.infinite(dataset.trim$Weights)] <- 1
  
  filename <- sprintf("run_%s_c.poi", run)
  write.table(
    dataset.trim, 
    file = paste(path, filename, sep = "/"), 
    row.names = FALSE, 
    col.names = FALSE,
    quote = FALSE
  )
  text <- paste("FILE", filename, "\nTITLE", filename, "\nNTIMES", n.year, "\nNCOVARS 0\nLABELS \nEnd\nMISSING -1\nWEIGHT Present\n\nWEIGHTING on\nSERIALCOR on\nOVERDISP on\nMODEL 3\nSTEPWISE off\nRUN\n")
  writeLines(
    text, 
    paste(
      path, 
      gsub("poi", "TCF", filename),
      sep = "/"
    )
  )
}

datasetpath <- paste(tempdir, "dataset", sep = "/")
to.do <- list.files(datasetpath, pattern = "^run_[0123456789]{4}_[01]_[0123456789]_0\\.rda$")
to.do <- gsub("^run_", "", to.do)
to.do <- gsub("\\.rda$", "", to.do)
rm(datasetpath)

path <- paste(tempdir, "trim", sep = "/")
if(file.exists(path)){
  done <- list.files(path, pattern = "^run_[0123456789]{4}_[0123456789]_[0123456789]_0\\.poi$")
  done <- gsub("^run_", "", done)
  done <- gsub("\\.poi$", "", done)
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
    n.year = n.year
  )
  sfStop()
} else {
  results <- lapply(
    to.do, 
    singleRun,
    path = path,
    seeds = seeds,
    n.year = n.year
  )
}

rm(to.do, results, singleRun)
