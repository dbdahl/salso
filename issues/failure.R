set.seed(77551264L)

library(salso)

load("probs.Rbin")
probs
salso(probs)                    # We get it

library(mcclust.ext)
minVI(probs, method="greedy")   # But they don't

salso:::minimize.by.enumeration(probs, loss="VI.lb")


data("cls.draw1.5")
probs2 <- psm(cls.draw1.5)

library(microbenchmark)
microbenchmark(
  salso(probs2, probExploration = 0.001),
  times=2
)

