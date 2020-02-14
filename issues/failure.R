set.seed(77551264L)

library(salso)
library(mcclust.ext)

load("probs.Rbin")
probs

# VI.lb

a <- salso(probs)$estimate
b <- minVI(probs, method="greedy")$cl   # But they don't
d <- salso:::minimize.by.enumeration(probs)

VI.lb(d, probs)   # Global minimum
VI.lb(a, probs)   # SALSO gets it
VI.lb(b, probs)   # W & G don't

all.equal(d,a)
all.equal(d,b)


n <- 3
partitions <- salso::enumerate.partitions(n)
nPartitions <- nrow(partitions)
partitions <- split(partitions, rep(1:nrow(partitions), each = ncol(partitions)))
adjMatrices <- lapply(partitions, salso:::mkAdjacencyMatrix)

mkPSM <- function() {
  x <- diff(c(0,sort(runif(nPartitions)),1))
  mapply(function(x,y) x * y, x, adjMatrices)
}
mkPSM()

