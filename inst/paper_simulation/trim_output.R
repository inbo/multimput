path <- paste(tempdir, "trim", sep = "/")

done <- list.files(
  path,
  pattern = "^run_[[:digit:]]{4}_[[:digit:]]_[[:digit:]]_[[:digit:]]\\.out$",
  full.names = TRUE
)
results.trim <- do.call(rbind, lapply(done, function(file){
  base.file <- gsub("trim", "dataset", file)
  base.file <- gsub("\\.out$", ".rda", base.file)
  load(base.file)
  observation.year.site <- aggregate(
    cbind(MeanN = !is.na(Observed)) ~ Year + Site,
    data = output$dataset,
    FUN = sum
  )
  observation.year <- aggregate(
    MeanN ~ Year,
    data = subset(observation.year.site, MeanN > 0),
    FUN = mean
  )

  start <- which(readLines(file) == " TIME TOTALS")
  output <- read.table(file, header = TRUE, skip = start, nrows = n.year)
  output <- output[, c(1, 4, 5)]
  colnames(output) <- c("Year", "Estimate", "Std. Error")
  run <- gsub(
    "\\.out$",
    "",
    gsub("^.*run_", "", file)
  )
  output$"2.5 %" <- qnorm(0.025, output$Estimate, output$"Std. Error")
  output$"97.5 %" <- qnorm(0.975, output$Estimate, output$"Std. Error")
  output$Run <- as.integer(substr(run, 1, 4))
  output$Pattern <- as.integer(substr(run, 6, 6))
  output$Missing <- as.integer(substr(run, 8, 8))
  output <- merge(output, observation.year)

  complete.file <- gsub("\\.out", "_c.out", file)
  if (file.exists(complete.file)) {
    start <- which(readLines(complete.file) == " TIME TOTALS")
    output.complete <- read.table(
      complete.file,
      header = TRUE,
      skip = start,
      nrows = n.year
    )
    output.complete <- output.complete[, c(1, 4, 5)]
    colnames(output.complete) <- c("Year", "Estimate2", "Std. Error2")
    output.complete$"2.5 %2" <- qnorm(
      0.025,
      output.complete$Estimate2,
      output.complete$"Std. Error2"
    )
    output.complete$"97.5 %2" <- qnorm(
      0.975,
      output.complete$Estimate2,
      output.complete$"Std. Error2"
    )
    output <- merge(output, output.complete)
  } else {
    output[, c("Estimate2", "Std. Error2", "2.5 %2", "97.5 %2")] <- NA
  }
  return(output)
}))
save(results.trim, file = paste0(datadir, "/paper_trim.rda")) #nolint
rm(path, done, results.trim)
