library(INLA)

load("waterfowl.rda")

model.species <- inla(
  Species ~ f(Winter, model = "rw1") + f(Site, model = "iid") + f(Period, model = "iid"),
  data = waterfowl,
  family = "nbinomial",
  control.compute = list(dic = TRUE)
)
save(model.species, "ModelSpecies.rda")
