# multimput 0.2.14

* In case all imputed values are identical, `model_impute()` only runs a single
  model on one imputation.
  It reports the mean and standard errors based on the single model as-is.
* `model_impute()` handles empty data.
* `model_impute()` can filter the covariates with a user supplied function.
* `model_impute()` gains a `timeout` argument.
* Bugfix in generating zero-inflated negative binomial data.

# multimput 0.2.13

* `aggregate_impute()` handles the corner case when `join` results in an empty 
  dataset.
* The `model_fun` argument of `model_impute()` can be either a function or a
  string containing the name of a function (like `"glm"`).
  Include the package name in case the function is not available in base R (like
  `"INLA::inla"`).

# multimput 0.2.12

* `impute()` gains an `extra` argument.
  Use it for observations not in the model that you still want to add in the
  follow-up analysis.
  For example: exclude rare observations from the model but you want them in the
  aggregations.
* `impute()` on INLA models now also handles the binomial, the zero-inflated
  Poison (type 0 and 1) and the zero-inflated negative binomial (type 0 and 1)
  distributions.
* Add `hurdle_impute()` to fit a hurdle model based on a model of the presences
  and a model of the counts.
* Added validation rules for `rawImputed` and `aggregatedImputed` objects.
* Update [`checklist`](https://inbo.github.io/checklist/) infrastructure.

# multimput 0.2.11

* Vignette runs without INLA.
  Required to make the package build on https://inbo.r-universe.dev

# multimput 0.2.10

* Use [`checklist`](https://inbo.github.io/checklist/) infrastructure. 

# multimput 0.2.7.9000

* `aggregate_impute()` now also works on `aggregatedImputed` objects (#34)
