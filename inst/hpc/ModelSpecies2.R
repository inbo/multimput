library(INLA)

load("waterfowl.rda")

model.species <- inla(
  Species ~ f(Winter, model = "rw1", replicate = Site) + f(Period, model = "iid"),
  data = waterfowl,
  family = "nbinomial",
  control.compute = list(dic = TRUE)
)
save(model.species, "ModelSpecies.rda")
