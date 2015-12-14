#' The indices for the example dataset
#'
#' Data for fig 9 in Onkelinx et al
#'
#' \itemize{
#'   \item \code{Estimate} The point estimate of the index
#'   \item \code{LCL} The lower confidence limit of the index
#'   \item \code{UCL} The upper confidence limit of the index
#'   \item \code{Year} The year of the index
#'   \item \code{Method} A factor indicating the imputation technique
#'   \item \code{Type} A factor indicating if the index is based on the observed, augmented or complete data
#'   \item \code{Family} A factor indicating if the index is based on the negative binomial or Poisson distribution
#' }
#'
#' @docType data
#' @keywords datasets
#' @name example
#' @usage data(example)
#' @format A data frame with 240 rows and 7 variables
NULL

#' The Monte Carlo simualtion results using INLA
#'
#' Data for fig 7 and 8 in Onkelinx et al
#'
#' \itemize{
#'   \item \code{Estimate} The point estimate of the $API$ index
#'   \item \code{Std. Error} The standard error of the $API$ index
#'   \item \code{Year} The year of the index
#'   \item \code{2.5 \%} The lower confidence limit of the $API$ index
#'   \item \code{97.5 \%} The upper confidence limit of the $API$ index
#'   \item \code{Run} The id of the simulated dataset
#'   \item \code{Pattern} Pattern of missingness: 0 = missing completely at random (MCAR), 1 = observed
#'   \item \code{Missing} The proportions of missingness: 0 = observed, 1 = 1\%, 2 = 5\%, 3 = 25\%, 4 = 50\%, 5 = 75\%
#'   \item \code{Model} The imputation model: 0 = basic, 1 = true mean, 3 = complex
#'   \item \code{Imputations} The number ofimputations
#' }
#'
#' @docType data
#' @keywords datasets
#' @name results.inla
#' @usage data(results.inla)
#' @format A data frame with 87600 rows and 12 variables
NULL

#' The Monte Carlo simualtion results using birdSTATs and TRIM
#'
#' Data for fig 4, 5 and 6 in Onkelinx et al
#'
#' \itemize{
#'   \item \code{Year} The year of the index
#'   \item \code{Estimate} The point estimate of the $APA$ index
#'   \item \code{Std. Error} The standard error of the $APA$ index
#'   \item \code{2.5 \%} The lower confidence limit of the $APA$ index
#'   \item \code{97.5 \%} The upper confidence limit of the $APA$ index
#'   \item \code{Run} The id of the simulated dataset
#' }
#'
#' @docType data
#' @keywords datasets
#' @name results.trim
#' @usage data(results.trim)
#' @format A data frame with 28800 rows and 10 variables
NULL

#' The Monte Carlo simualtion results using the complete data
#'
#' Data for fig 3 to 8 in Onkelinx et al
#'
#' \itemize{
#'   \item \code{Estimate} The point estimate of the $API$ index
#'   \item \code{Std. Error} The standard error of the $API$ index
#'   \item \code{2.5 \%} The lower confidence limit of the $API$ index
#'   \item \code{97.5 \%} The upper confidence limit of the $API$ index
#'   \item \code{Estimatebis} The point estimate of the $APA$ index
#'   \item \code{Std. Errorbis} The standard error of the $APA$ index
#'   \item \code{2.5 \%bis} The lower confidence limit of the $APA$ index
#'   \item \code{97.5 \%bis} The upper confidence limit of the $APA$ index
#'   \item \code{Year} The year of the index
#'   \item \code{Run} The id of the simulated dataset
#'   \item \code{Years} The number of years in the dataset: 0 = 24, 1 = 6, 2 = 12, 3 = 24
#'   \item \code{Sites} The number of sites in the dataset: 0 = 100, 1 = 25, 2 = 50, 3 = 100
#' }
#'
#' @docType data
#' @keywords datasets
#' @name results.truth
#' @usage data(results.truth)
#' @format A data frame with 25200 rows and 12 variables
NULL

#' The Monte Carlo simualtion results using Underhill
#'
#' Data for fig 3 in Onkelinx et al
#'
#' \itemize{
#'   \item \code{Estimate} The point estimate of the $API$ index
#'   \item \code{Std. Error} The standard error of the $API$ index
#'   \item \code{2.5 \%} The lower confidence limit of the $API$ index
#'   \item \code{97.5 \%} The upper confidence limit of the $API$ index
#'   \item \code{Year} The year of the index
#'   \item \code{Run} The id of the simulated dataset
#'   \item \code{Pattern} Pattern of missingness: 0 = missing completely at random (MCAR)
#'   \item \code{Missing} The proportions of missingness: 0 = observed
#'   \item \code{Method} The algorithm: UHM = original Underhill with mean as start, UH0, original Underhill with 0 as start, UAM = altered Underhill with mean as start, UA0 = altered Underhill with 0 as start
#' }
#'
#' @docType data
#' @keywords datasets
#' @name results.underhill
#' @usage data(results.underhill)
#' @format A data frame with 19200 rows and 9 variables
NULL

#' The observation pattern in the Flemish waterfowl dataset
#'
#' Data for fig 1 and 2 in Onkelinx et al
#'
#' \itemize{
#'   \item \code{Site} Site ID
#'   \item \code{Winter} Winter ID
#'   \item \code{Period} ID of the month
#'   \item \code{Species} Number of observed species
#'   \item \code{Birds} Total number of birds
#' }
#'
#' @docType data
#' @keywords datasets
#' @name waterfowl
#' @usage data(waterfowl)
#' @format A data frame with 77157 rows and 5 variables
NULL
