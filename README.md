<!-- README.md is generated from README.Rmd. Please edit that file -->
[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.48423.svg)](http://dx.doi.org/10.5281/zenodo.48423)

| Branch  | Build status                                                                                                                                                                           | Code coverage                                                                                                                                  |
|---------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------|
| Master  | [![wercker status](https://app.wercker.com/status/5f154cd480de20b68cde62705e817436/s/master "wercker status")](https://app.wercker.com/project/bykey/5f154cd480de20b68cde62705e817436) | [![codecov.io](https://codecov.io/github/inbo/multimput/coverage.svg?branch=master)](https://codecov.io/github/inbo/multimput?branch=master)   |
| Develop | [![wercker status](https://app.wercker.com/status/5f154cd480de20b68cde62705e817436/s/master "wercker status")](https://app.wercker.com/project/bykey/5f154cd480de20b68cde62705e817436) | [![codecov.io](https://codecov.io/github/inbo/multimput/coverage.svg?branch=develop)](https://codecov.io/github/inbo/multimput?branch=develop) |

CAUTION: GitHub flavoured markdown doesn't support the rendering of mathematics at this moment. Hence the mathematics in this README are not rendered properly. The information below is also available as a vignette within the package. The mathematics will be rendered in the vignette. To read the vignette one needs to install the package first.

Installation instructions
=========================

This package requires the `INLA` package. You need to install it with `install.packages("INLA", repos = "https://www.math.ntnu.no/inla/R/stable")`. If this fails you can use `devtools::install_github("inbo/INLA")`. Note that the latter is just a read-only mirror which is infrequently updated. Hence installing `INLA` from <https://www.math.ntnu.no/inla> is highly recommended.

When `INLA` is installed, you can install `multimput` with `devtools::install_github("inbo/multimput", build_vignettes = TRUE)`. To view the vignette use `vignette("Impute", package = "multimput")`

A docker image with all the required dependencies is available from <https://hub.docker.com/r/inbobmk/multimput/>. Use `docker pull inbobmk/multimput` to get it.

The `multimput` package
=======================

The `multimput` package was originally intended to provide the data and code to replicate the results of Onkelinx, Devos, and Quataert (2016). This paper is freely available at <http://dx.doi.org/10.1007/s10336-016-1404-9>. The functions were all rewritten to make them more user-friendly and more generic. In order to make the package more compact, we removed the original code and data starting for version 0.2.6. However both the original code and data remain available in [the older releases](https://github.com/inbo/multimput/releases).
