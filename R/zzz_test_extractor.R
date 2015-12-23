# Auxilay functions only relevant for the unit tests of extractor()
summary.N <- function(...){
  list(
    coefficients = matrix(
      integer(0),
      ncol = 1,
      dimnames = list(NULL, "X")
    )
  )
}
summary.E <- function(...){
  list(
    coefficients = matrix(
      integer(0),
      ncol = 1,
      dimnames = list(NULL, "Estimate")
    )
  )
}
summary.S <- function(...){
  list(
    coefficients = matrix(
      integer(0),
      ncol = 1,
      dimnames = list(NULL, "Std. Error")
    )
  )
}
summary.C <- function(...){
  list(
    coefficients = matrix(
      0,
      ncol = 2,
      dimnames = list(NULL, c("Std. Error", "Estimate"))
    )
  )
}
