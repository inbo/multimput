<!-- README.md is generated from README.Rmd. Please edit that file -->
<table style="width:49%;">
<colgroup>
<col width="11%" />
<col width="18%" />
<col width="6%" />
<col width="12%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">Branch</th>
<th align="left">Build status</th>
<th align="left">Code</th>
<th align="left">coverage</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Master</td>
<td align="left"><a href="https://app.wercker.com/project/bykey/b4d00d5173468e492a589578cb4647aa"><img src="https://app.wercker.com/status/b4d00d5173468e492a589578cb4647aa/m/master" title="wercker status" alt="wercker status" /></a></td>
<td align="left"><a href="https://codecov.io/github/ThierryO/multimput?branch=master"><img src="https://codecov.io/github/ThierryO/multimput/coverage.svg?branch=master" alt="codecov.io" /></a></td>
<td align="left"><img src="https://codecov.io/github/ThierryO/multimput/branch.svg?branch=master" alt="codecov.io" /></td>
</tr>
<tr class="even">
<td align="left">Develop</td>
<td align="left"><a href="https://app.wercker.com/project/bykey/b4d00d5173468e492a589578cb4647aa"><img src="https://app.wercker.com/status/b4d00d5173468e492a589578cb4647aa/m/develop" title="wercker status" alt="wercker status" /></a></td>
<td align="left"><a href="https://codecov.io/github/ThierryO/multimput?branch=develop"><img src="https://codecov.io/github/ThierryO/multimput/coverage.svg?branch=develop" alt="codecov.io" /></a></td>
<td align="left"><img src="https://codecov.io/github/ThierryO/multimput/branch.svg?branch=develop" alt="codecov.io" /></td>
</tr>
</tbody>
</table>

CAUTION: GitHub flavoured markdown doesn't support the rendering of mathematics at this moment. The information below is available as a vignette within the package. The mathematics will be rendered in the vignette. To read the vignette one needs to install the package first.

Installation instruction
========================

This package requires the `INLA` package. You need to install it with `install.packages("INLA", repos = "https://www.math.ntnu.no/inla/R/stable")`. If this fails you can use `devtools::install_github("INBO-BMK/INLA")`. Note that the latter is just a read-only mirror which is infrequently updated. Hence installing `INLA` from <https://www.math.ntnu.no/inla> is highly recommended.

When `INLA` is installed, we can install `multimput` with `devtools::install_github("ThierryO/multimput", build_vignettes = TRUE)`. To view the vignette use `vignette("Impute", package = "multimput")`

Very short intro to multiple imputation
=======================================

1.  Create the imputation model
2.  Generate imputations for the missing observation
3.  Aggregate the imputed data
4.  Model the aggregated imputed data

Short intro to multiple imputation
==================================

The imputations are based on a model \(Y \sim X \beta^*\) which the user has to specify. For a missing value \(i\) with covariates \(x_i\), we draw a random value \(y_i\) from the distribution of \(\hat{y}_i\). In case of a linear model, we sample a normal distribution \(y_i \sim N(\hat{y}_i, \sigma_i)\). An imputation set \(l\) holds an impute value \(y_i\) for each missing value.

With the missing values replaced by imputation set \(l\), the dataset is complete. So we can apply the analysis that we wanted to do in the first place. This can, but don't has to, include aggregating the dataset prior to analysis. The analysis results in a set of coefficients \({\gamma_a}_l\) and their standard error \({\sigma_a}_l\). Offcourse, this set will depend on the imputed values of the imputation set \(l\). Another imputation set has different imputed values and will hence lead to different coefficients.

Therefor the imputation, aggregation and analysis is repeated for \(L\) different imputation sets, resulting in \(L\) sets of coefficients and their standard errors. They are aggregated by the formulas below. The coefficient will be the average of the coefficient in all imputation sets. The standard error of a coefficient is the square root of a sum of two parts. The first part is the average of the squared standard error in all imputation sets. The second part is the variance of the coefficient among the imputation sets, multiplied by a correction factor \(1 + \frac{1}{L}\).

\[\bar{\gamma}_a = \frac{\sum_{l = 1}^L{\gamma_a}_l}{L}\] \[\bar{\sigma}_a = \sqrt{\frac{\sum_{l = 1}^J {{\sigma_a^2}_l}}{L} + (1 + \frac{1}{L}) 
\frac{\sum_{l = 1}^L({\gamma_a}_l - \bar{\gamma}_a) ^ 2}{L - 1}}\]

The dataset
===========

First, let's generate a dataset and set some observations missing. `generateData()` creates a balanced dataset with repeated visits of a number of sites. Each site is visited several years and multiple times per year. Have a look at the help-file of `generateData()` for more details on the model.

``` r
library(multimput)
set.seed(123)
prop.missing <- 0.5
dataset <- generateData(
  n.year = 10, n.period = 6, n.site = 50, 
  n.run = 1
)
dataset$Observed <- dataset$Count
which.missing <- sample(nrow(dataset), size = nrow(dataset) * prop.missing)
dataset$Observed[which.missing] <- NA
dataset$fYear <- factor(dataset$Year)
dataset$fPeriod <- factor(dataset$Period)
dataset$fSite <- factor(dataset$Site)
str(dataset)
#> 'data.frame':    3000 obs. of  10 variables:
#>  $ Year    : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ Period  : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ Site    : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ Mu      : num  11.36 9.31 11.33 10.98 9.79 ...
#>  $ Run     : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ Count   : num  8 2 10 21 2 2 13 8 10 4 ...
#>  $ Observed: num  NA NA 10 21 NA 2 13 8 10 4 ...
#>  $ fYear   : Factor w/ 10 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
#>  $ fPeriod : Factor w/ 6 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ fSite   : Factor w/ 50 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
```

Variables in dataset

Year  
The year of the observation as an integer

fYear  
The year of the observation as a factor

Period  
The period of the observation as an integer

fPeriod  
The period of the observation as a factor

Site  
The ID of the site as an integer

fSite  
The ID of the site as a factor

Mu  
The expected value of a negative binomial distribution

Count  
A realisation of a negative binomial distribution with expected value `Mu`

Observed  
The `Count` variable with missing data

``` r
library(ggplot2)
ggplot(dataset, aes(x = Year, y = Mu, group = Site)) + 
  geom_line() + 
  facet_wrap(~Period) + 
  scale_y_log10()
```

![](README-plot_data-1.png)<!-- -->

Create the imputation model
===========================

We will create several models, mainly to illustrate the capabilities of the `multimput` package. Hence several of the models are not good for a real life application.

``` r
imp.lm <- lm(Observed ~ fYear + fPeriod + fSite, data = dataset)
library(INLA)
#> Loading required package: sp
#> Loading required package: Matrix
#> Loading required package: splines
imp.inla.p <- inla(
  Observed ~ fYear + fPeriod + f(Site, model = "iid"), 
  data = dataset, 
  family = "poisson", 
  control.predictor = list(compute = TRUE)
)
imp.inla.nb <- inla(
  Observed ~ fYear + fPeriod + f(fSite, model = "iid"), 
  data = dataset, 
  family = "nbinomial", 
  control.predictor = list(compute = TRUE)
)
dataset$YearCopy <- dataset$Year
imp.better <- inla(
  Observed ~ f(Year, model = "rw1") + f(YearCopy, model = "ar1", replicate = Site) + 
    fPeriod, 
  data = dataset, 
  family = "nbinomial", 
  control.predictor = list(compute = TRUE)
)
```

Apply the imputation model
==========================

Most models have a `predict` method. In such a case `impute()` requires both a `model` and a `data` argument. Note that this implies that one can apply an imputation on any dataset as long as the dataset contains the necessary variables.

`inla` do the prediction simultaneously with the model fitting. Hence the model contains all required information and the `data` is not used.

`n.imp` is the number of imputations. The default is `n.imp = 19`.

``` r
raw.lm <- impute(imp.lm, data = dataset)
raw.inla.p <- impute(imp.inla.p)
raw.inla.nb <- impute(imp.inla.nb)
raw.better <- impute(imp.better)
raw.better.199 <- impute(imp.better, n.imp = 9)
```

Aggregate the imputated dataset
===============================

Suppose that we are interested in the sum of the counts over all sites for each combination of year and period. Then we must aggregate the imputations on year and period. The resulting object will only contain the imputated response and the grouping variables. The easiest way to have a variable like year both a continuous and factor is to add both `Year` and `fYear` to the `grouping`.

``` r
aggr.lm <- aggregate_impute(
  raw.lm, 
  grouping = c("fYear", "fPeriod", "Year"), 
  fun = sum
)
aggr.inla.p <- aggregate_impute(
  raw.inla.p, 
  grouping = c("fYear", "fPeriod", "Year"), 
  fun = sum
)
aggr.inla.nb <- aggregate_impute(
  raw.inla.nb, 
  grouping = c("fYear", "fPeriod", "Year"), 
  fun = sum
)
aggr.better <- aggregate_impute(
  raw.better, 
  grouping = c("fYear", "fPeriod", "Year"), 
  fun = sum
)
aggr.better.199 <- aggregate_impute(
  raw.better.199, 
  grouping = c("fYear", "fPeriod", "Year"), 
  fun = sum
)
```

Model the aggregated imputed dataset
====================================

Simple example
--------------

`model_impute()` will apply the `model.fun` to each imputation set. The covariates are defined in the `rhs` argument. The tricky part of this function the `extractor` argument. This is a user defined function which must have an argument called `model`. The function should return a `data.frame` or `matrix` with two columuns. The first column hold the estimate of a parameter of the `model`, the second column their standard error. Each row represents a parameter.

``` r
extractor.lm <- function(model){
  summary(model)$coefficients[, c("Estimate", "Std. Error")]
}  
model_impute(
  aggr.lm, 
  model.fun = lm, 
  rhs = "0 + fYear + fPeriod", 
  extractor = extractor.lm
)
#>           Estimate       SE
#> fYear1    915.6794 143.7884
#> fYear2   1039.5812 144.8056
#> fYear3   1319.8559 137.0545
#> fYear4   1248.5774 136.5014
#> fYear5   1034.5023 142.1748
#> fYear6   1543.1000 160.2901
#> fYear7   1641.6491 135.1280
#> fYear8   1325.5156 141.3124
#> fYear9   1184.6325 131.2830
#> fYear10  1092.5166 146.4315
#> fPeriod2  394.9609 136.6279
#> fPeriod3  536.5071 136.9948
#> fPeriod4  354.1306 129.3417
#> fPeriod5 -125.4260 133.4333
#> fPeriod6 -421.7390 131.9829
```

Return only the parameters associated with `fYear`
--------------------------------------------------

The `extractor` function requires more work from the user. This cost is compensated by the high degree of flexibility. The user doesn't depend on the predefined extractor functions. This is illustrated by the following examples.

``` r
extractor.lm2 <- function(model){
  cf <- summary(model)$coefficients
  cf[grepl("fYear", rownames(cf)), c("Estimate", "Std. Error")]
}  
model_impute(
  aggr.lm, 
  model.fun = lm, 
  rhs = "0 + fYear + fPeriod", 
  extractor = extractor.lm2
)
#>          Estimate       SE
#> fYear1   915.6794 143.7884
#> fYear2  1039.5812 144.8056
#> fYear3  1319.8559 137.0545
#> fYear4  1248.5774 136.5014
#> fYear5  1034.5023 142.1748
#> fYear6  1543.1000 160.2901
#> fYear7  1641.6491 135.1280
#> fYear8  1325.5156 141.3124
#> fYear9  1184.6325 131.2830
#> fYear10 1092.5166 146.4315
```

Predict a smoother for predefined values
----------------------------------------

Note that we pass extra arguments to the `extractor` function through the `extractor.args` argument. This has to be a list. We recommend to use a named list to avoid confusion.

``` r
library(mgcv)
#> Loading required package: nlme
#> This is mgcv 1.8-12. For overview type 'help("mgcv-package")'.
new.set <- expand.grid(
  Year = pretty(dataset$Year, 20),
  fPeriod = dataset$fPeriod[1]
)
extractor.lm3 <- function(model, newdata){
  predictions <- predict(model, newdata = newdata, se.fit = TRUE)
  cbind(
    predictions$fit,
    predictions$se.fit
  )
}  
model.gam <- model_impute(
  aggr.lm, 
  model.fun = gam, 
  rhs = "s(Year) + fPeriod", 
  extractor = extractor.lm3,
  extractor.args = list(newdata = new.set)
)
model.gam <- cbind(new.set, model.gam)
model.gam$LCL <- qnorm(0.025, mean = model.gam$Estimate, sd = model.gam$SE)
model.gam$UCL <- qnorm(0.975, mean = model.gam$Estimate, sd = model.gam$SE)
ggplot(model.gam, aes(x = Year, y = Estimate, ymin = LCL, ymax = UCL)) + 
  geom_ribbon(alpha = 0.1) + 
  geom_line()
```

![](README-model_aggregate_lm3-1.png)<!-- -->

Compare the results using different imputation models
-----------------------------------------------------

``` r
covar <- data.frame(
  Year = sort(unique(dataset$Year))
)
extractor.inla <- function(model){
  fe <- model$summary.fixed[, c("mean", "sd")]
  fe[grepl("fYear", rownames(fe)), ]
}
model.p <- model_impute(
  object = aggr.inla.p,
  model.fun = inla,
  rhs = "0 + fYear + f(fPeriod, model = 'iid')",
  model.args = list(family = "nbinomial"),
  extractor = extractor.inla
)
model.nb <- model_impute(
  object = aggr.inla.nb,
  model.fun = inla,
  rhs = "0 + fYear + f(fPeriod, model = 'iid')",
  model.args = list(family = "nbinomial"),
  extractor = extractor.inla
)
model.better <- model_impute(
  object = aggr.better,
  model.fun = inla,
  rhs = "0 + fYear + f(fPeriod, model = 'iid')",
  model.args = list(family = "nbinomial"),
  extractor = extractor.inla
)
aggr.complete <- aggregate(
  dataset[, "Count", drop = FALSE],
  dataset[, c("fYear", "fPeriod")],
  FUN = sum
)
m.complete <- inla(
  Count ~ 0 + fYear + f(fPeriod, model = "iid"),
  data = aggr.complete,
  family = "nbinomial"
)
model.complete <- extractor.inla(m.complete)
colnames(model.complete) <- c("Estimate", "SE")
parameters <- rbind(
  cbind(covar, model.p, Model = "poisson"),
  cbind(covar, model.nb, Model = "negative binomial"),
  cbind(covar, model.better, Model = "better"),
  cbind(covar, model.complete, Model = "complete")
)
parameters$LCL <- qnorm(0.025, mean = parameters$Estimate, sd = parameters$SE)
parameters$UCL <- qnorm(0.975, mean = parameters$Estimate, sd = parameters$SE)
ggplot(parameters, aes(x = Year, y = Estimate, ymin = LCL, ymax = UCL)) + 
  geom_ribbon(, alpha = 0.2) + 
  geom_line() + 
  facet_wrap(~Model)
```

![](README-model_inla-1.png)<!-- -->
