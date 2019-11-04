set.seed(77551264L)

library(salso)

load("probs.Rbin")
probs
salso(probs)                    # We get it

library(mcclust.ext)
minVI(probs, method="greedy")   # But they don't

partitions <- enumerate.partitions(9L)
min(VI.lb(partitions, probs))   # Via enumeration
