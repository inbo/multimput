library(INLA)
library(MASS)

# loading dataset
the.dataset <- "0001_1_3_0"
birdstat.file <- paste0(tempdir, "/trim/run_", the.dataset, ".out") #nolint
birdstat.file2 <- paste0(tempdir, "/trim/run_", the.dataset, "_c.out") #nolint
load(paste0(tempdir, "/dataset/run_", the.dataset, ".rda")) #nolint
rm(the.dataset)
dataset <- output$dataset
rm(output)
dataset$YearPeriod <- interaction(dataset$Year, dataset$Period, drop = TRUE)
dataset$YearSite <- interaction(dataset$Year, dataset$Site, drop = TRUE)

# reading birdSTATS output
start <- which(readLines(birdstat.file) == " TIME TOTALS")
output <- read.table(
  birdstat.file,
  header = TRUE,
  skip = start,
  nrows = length(levels(dataset$Year))
)
rm(birdstat.file)
output <- output[, c(1, 4, 5)]
colnames(output) <- c("Year", "Estimate", "Std. Error")
output$LCL <- qnorm(0.025, output$Estimate, output$"Std. Error")
output$UCL <- qnorm(0.975, output$Estimate, output$"Std. Error")
output$Year <- as.integer(levels(dataset$Year))
output$"Std. Error" <- NULL
output$Type <- "Augmented"
output$Method <- "birdSTATs"
output$Family <- "Poisson"
result.birdstats <- output
rm(output)

# reading birdSTATS output
start <- which(readLines(birdstat.file2) == " TIME TOTALS")
output <- read.table(
  birdstat.file2,
  header = TRUE,
  skip = start,
  nrows = length(levels(dataset$Year))
)
rm(birdstat.file2, start)
output <- output[, c(1, 4, 5)]
colnames(output) <- c("Year", "Estimate", "Std. Error")
output$LCL <- qnorm(0.025, output$Estimate, output$"Std. Error")
output$UCL <- qnorm(0.975, output$Estimate, output$"Std. Error")
output$Year <- as.integer(levels(dataset$Year))
output$"Std. Error" <- NULL
output$Type <- "Complete"
output$Method <- "birdSTATs"
output$Family <- "Poisson"
result.birdstats.complete <- output
rm(output)

# impute using INLA with the complex model
dataset.inla <- imputeINLA(
  dataset,
  formula = Observed ~ Year + Period + f(Site, model = "iid") +
    f(YearPeriod, model = "iid") + f(YearSite, model = "iid"),
  n.sim = 199
)

total.inla <- imputedTotals(
  data = dataset,
  imputations = dataset.inla,
  variable = "Observed",
  rhs = "Year + Period"
)
output <- summarizeImputationGLM.nb(data = total.inla, rhs = "0 + Year")
colnames(output) <- c("Parameter", "Estimate", "Std. Error")
output <- output[grep("Year", output$Parameter), ]
output$Year <- as.integer(gsub("Year", "", output$Parameter))
output$Parameter <- NULL
output$LCL <- qnorm(0.025, mean = output$Estimate, sd = output$"Std. Error")
output$UCL <- qnorm(0.975, mean = output$Estimate, sd = output$"Std. Error")
output$"Std. Error" <- NULL
output$Type <- "Augmented"
output$Method <- "Multiple\nimputation"
output$Family <- "Negative binomial"
output[, c("Estimate", "LCL", "UCL")] <- exp(
  output[, c("Estimate", "LCL", "UCL")]
)
result.inla <- output
rm(output, dataset.inla, total.inla)

# impute using INLA with the complex model and Poisson distribution
dataset.inla <- imputeINLA(
  dataset,
  formula = Observed ~ Year + Period + f(Site, model = "iid") +
    f(YearPeriod, model = "iid") + f(YearSite, model = "iid"),
  n.sim = 199,
  family = "poisson"
)

total.inla <- imputedTotals(
  data = dataset,
  imputations = dataset.inla,
  variable = "Observed",
  rhs = "Year + Period"
)
output <- summarizeImputationGLM(data = total.inla, rhs = "0 + Year")
colnames(output) <- c("Parameter", "Estimate", "Std. Error")
output <- output[grep("Year", output$Parameter), ]
output$Year <- as.integer(gsub("Year", "", output$Parameter))
output$Parameter <- NULL
output$LCL <- qnorm(0.025, mean = output$Estimate, sd = output$"Std. Error")
output$UCL <- qnorm(0.975, mean = output$Estimate, sd = output$"Std. Error")
output$"Std. Error" <- NULL
output$Type <- "Augmented"
output$Method <- "Multiple\nimputation"
output$Family <- "Poisson"
output[, c("Estimate", "LCL", "UCL")] <- exp(
  output[, c("Estimate", "LCL", "UCL")]
)
result.inla <- rbind(result.inla, output)
rm(output, dataset.inla, total.inla)

# impute using Underhill (original with mean as initial value)
initial <- round(exp(coef(glm.nb(Observed ~ 1, data = dataset))))
imputation  <- imputeUnderhill(
  data = dataset,
  formula = Observed ~ Year + Period + Site,
  initial = initial
)
rm(initial)
total <- aggregate(
  imputation$data[, "Observed"],
  imputation$data[, c("Year", "Period")],
  FUN = sum
)
model <- glm.nb(x ~ 0 + Year, data = total)
output <- cbind(
  coef(summary(model))[, c("Estimate", "Std. Error")],
  confint(model)
)
colnames(output)[3:4] <- c("LCL", "UCL")
output <- output[grep("Year", rownames(output)), ]
output <- as.data.frame(output)
output$Year <- as.integer(
  gsub("Year", "", rownames(output))
)
rownames(output) <- NULL
output$Method <- "Underhill"
output$Type <- "Augmented"
output$Family <- "Negative binomial"
output[, c("Estimate", "LCL", "UCL")] <- exp(
  output[, c("Estimate", "LCL", "UCL")]
)
output$"Std. Error" <- NULL
result.underhill <- output
rm(output, total, model, imputation)

# impute using Underhill (original with mean as initial value)
initial <- round(exp(coef(glm(Observed ~ 1, data = dataset, family = poisson))))
imputation  <- imputeUnderhill(
  data = dataset,
  formula = Observed ~ Year + Period + Site,
  initial = initial,
  family = "poisson"
)
rm(initial)
total <- aggregate(
  imputation$data[, "Observed"],
  imputation$data[, c("Year", "Period")],
  FUN = sum
)
model <- glm(x ~ 0 + Year, data = total, family = poisson)
output <- cbind(
  coef(summary(model))[, c("Estimate", "Std. Error")],
  confint(model)
)
colnames(output)[3:4] <- c("LCL", "UCL")
output <- output[grep("Year", rownames(output)), ]
output <- as.data.frame(output)
output$Year <- as.integer(
  gsub("Year", "", rownames(output))
)
rownames(output) <- NULL
output$Method <- "Underhill"
output$Type <- "Augmented"
output$Family <- "Poisson"
output[, c("Estimate", "LCL", "UCL")] <- exp(
  output[, c("Estimate", "LCL", "UCL")]
)
output$"Std. Error" <- NULL
result.underhill <- rbind(result.underhill, output)
rm(output, total, model, imputation)

# observed only
total <- aggregate(
  dataset[, "Observed"],
  dataset[, c("Year", "Period")],
  FUN = sum,
  na.rm = TRUE
)
model <- glm.nb(x ~ 0 + Year, data = total)
output <- cbind(
  coef(summary(model))[, c("Estimate", "Std. Error")],
  confint(model)
)
colnames(output)[3:4] <- c("LCL", "UCL")
output <- output[grep("Year", rownames(output)), ]
output <- as.data.frame(output)
output$Year <- as.integer(
  gsub("Year", "", rownames(output))
)
rownames(output) <- NULL
output$Method <- "As is"
output$Type <- "Observed"
output$Family <- "Negative binomial"
output[, c("Estimate", "LCL", "UCL")] <- exp(
  output[, c("Estimate", "LCL", "UCL")]
)
output$"Std. Error" <- NULL
result.observed <- output
rm(output, total, model)

# observed only
total <- aggregate(
  dataset[, "Observed"],
  dataset[, c("Year", "Period")],
  FUN = sum,
  na.rm = TRUE
)
model <- glm(x ~ 0 + Year, data = total, family = poisson)
output <- cbind(
  coef(summary(model))[, c("Estimate", "Std. Error")],
  confint(model)
)
colnames(output)[3:4] <- c("LCL", "UCL")
output <- output[grep("Year", rownames(output)), ]
output <- as.data.frame(output)
output$Year <- as.integer(
  gsub("Year", "", rownames(output))
)
rownames(output) <- NULL
output$Method <- "As is"
output$Type <- "Observed"
output$Family <- "Poisson"
output[, c("Estimate", "LCL", "UCL")] <- exp(
  output[, c("Estimate", "LCL", "UCL")]
)
output$"Std. Error" <- NULL
result.observed <- rbind(result.observed, output)
rm(output, total, model)

# complete counts
total <- aggregate(
  dataset[, "Count"],
  dataset[, c("Year", "Period")],
  FUN = sum,
  na.rm = TRUE
)
model <- glm.nb(x ~ 0 + Year, data = total)
output <- cbind(
  coef(summary(model))[, c("Estimate", "Std. Error")],
  confint(model)
)
colnames(output)[3:4] <- c("LCL", "UCL")
output <- output[grep("Year", rownames(output)), ]
output <- as.data.frame(output)
output$Year <- as.integer(
  gsub("Year", "", rownames(output))
)
rownames(output) <- NULL
output$Method <- "As is"
output$Type <- "Complete"
output$Family <- "Negative binomial"
output[, c("Estimate", "LCL", "UCL")] <- exp(
  output[, c("Estimate", "LCL", "UCL")]
)
output$"Std. Error" <- NULL
result.complete <- output
rm(output, total, model)


# complete counts using poisson
total <- aggregate(
  dataset[, "Count"],
  dataset[, c("Year", "Period")],
  FUN = sum,
  na.rm = TRUE
)
model <- glm(x ~ 0 + Year, data = total, family = poisson)
output <- cbind(
  coef(summary(model))[, c("Estimate", "Std. Error")],
  confint(model)
)
colnames(output)[3:4] <- c("LCL", "UCL")
output <- output[grep("Year", rownames(output)), ]
output <- as.data.frame(output)
output$Year <- as.integer(
  gsub("Year", "", rownames(output))
)
rownames(output) <- NULL
output$Method <- "As is"
output$Type <- "Complete"
output$Family <- "Poisson"
output[, c("Estimate", "LCL", "UCL")] <- exp(
  output[, c("Estimate", "LCL", "UCL")]
)
output$"Std. Error" <- NULL
result.complete.poisson <- output
rm(output, total, model, dataset)

example <- rbind(
  result.observed,
  result.complete,
  result.complete.poisson,
  result.underhill,
  result.inla,
  result.birdstats.complete,
  result.birdstats
)
unique(example$Method)
example$Method <- factor(
  example$Method,
  levels = c("As is", "Multiple\nimputation", "birdSTATs", "Underhill")
)
example$Type <- factor(
  example$Type,
  levels = c("Observed", "Augmented", "Complete")
)
example$Family <- factor(example$Family)

save(example, file = paste0(datadir, "/paper_example.rda")) #nolint
rm(
  example, result.birdstats, result.birdstats.complete, result.complete,
  result.complete.poisson, result.inla, result.observed, result.underhill
)
