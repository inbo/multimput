[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![License](http://img.shields.io/badge/license-GPL--3-blue.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)
[![Release](https://img.shields.io/github/release/qubyte/rubidium.svg)](https://github.com/inbo/multimput/releases)
[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.48423.svg)](http://dx.doi.org/10.5281/zenodo.48423)
[![wercker status](https://app.wercker.com/status/5f154cd480de20b68cde62705e817436/s/master "wercker status")](https://app.wercker.com/project/bykey/5f154cd480de20b68cde62705e817436) 
[![Build status](https://ci.appveyor.com/api/projects/status/auiumf7qteqttgxa/branch/master?svg=true)](https://ci.appveyor.com/project/ThierryO/multimput/branch/master)
[![codecov.io](https://codecov.io/github/inbo/multimput/coverage.svg?branch=master)](https://codecov.io/github/inbo/multimput?branch=master)

# multimput

## Rationale

The `multimput` package was originally intended to provide the data and code to replicate the results of Onkelinx, Devos, and Quataert (2016). This paper is freely available at <http://dx.doi.org/10.1007/s10336-016-1404-9>. The functions were all rewritten to make them more user-friendly and more generic. In order to make the package more compact, we removed the original code and data starting for version 0.2.6. However both the original code and data remain available in [the older releases](https://github.com/inbo/multimput/releases).

## Documentation

All helpfiles and vignettes are available at https://inbo.github.io/drat/docs/multimput/index.html

## Installation

This package requires the `INLA` package. You need to install it with `install.packages("INLA", repos = "https://inla.r-inla-download.org/R/stable")`. If this fails you can use `devtools::install_github("inbo/INLA")`. Note that the latter is just a read-only mirror which is infrequently updated. Hence installing `INLA` from <https://inla.r-inla-download.org> is highly recommended.

When `INLA` is installed, you can install `multimput`. For Windows, we recommend to use our [`drat` repository](https://inbo.github.io/drat). First install the `drat` package (`install.packages("drat")`), then add our `drat` repository to your session (`drat:addRepo("inbo")`). Finally you can install `multimput` (`install.packages("drat")`). For Linux or Mac OS, you can either follow the Windows instructions or install the development version with `devtools`. First install `devtools` (`install.packages("devtools")`). Then install `multimput` (`devtools::install_github("inbo/multimput", build_vignettes = TRUE)`). To view the vignette use `vignette("Impute", package = "multimput")`. The vignette is also available at https://inbo.github.io/drat/docs/multimput/articles/Impute.html

A docker image with all the required dependencies is available from <https://hub.docker.com/r/inbobmk/multimput/>. Use `docker pull inbobmk/multimput` to get it.

## Folder structure

The folder structure is that of a typical R packages with the mandatory `R` folder (definition of the functions) and `man` (helpfiles in Rd format). `data` is an optional folder in which some data sets are stored. The optional `test` folder contains the unit tests using the infrastructure from the `testthat` package. The optional `vignette` folder contains examples of the available themes. `man-roxygen` contains `roxygen2` templates for the documentation.

```
multimput
|-- data
|-- man
|-- man-roxygen
|-- R
|-- tests
   |-- testthat
|-- vignettes
```
