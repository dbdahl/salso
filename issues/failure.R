library(salso)

load("probs.Rbin")
result <- salso(probs)
result

salso(probs, loss="binder")

library(mcclust.ext)
minVI(probs, method="greedy")

library(mcclust)
data(cls.draw1.5)
probs <- psm(cls.draw1.5)

r1 <- salso(probs, nPermutations=5000, probExploration=0.001)
r2 <- salso(probs, nPermutations=5000, probExploration=0.0)
table(r1$estimate,r2$estimate)
r1
r2

minVI(probs, method="greedy")
