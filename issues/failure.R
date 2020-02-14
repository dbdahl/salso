# set.seed(77551264L)

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





seed <- runif(1,0,.Machine$integer.max)
set.seed(seed)

n <- 4
partitions <- salso::enumerate.partitions(n)
nPartitions <- nrow(partitions)
adjMatrices <- lapply(lapply(asplit(partitions,1), as.vector), salso:::mkAdjacencyMatrix)

mkPSM <- function() {
  x <- diff(c(0,sort(runif(nPartitions-1)),1))
  y <- mapply(function(x,y) x * y, x, adjMatrices, SIMPLIFY=FALSE)
  list(weights=x, probs=Reduce("+", y))
}

while ( TRUE ) {
  x <- mkPSM()
  a <- suppressWarnings(salso(x$probs, loss="binder")$estimate)
  if ( max(a) < 2 ) next
  b1 <- minbinder(x$probs, method="laugreen")$cl
  if ( isTRUE(all.equal(a,b1)) ) next
  b2 <- minbinder.ext(x$probs, method="greedy")$cl
  if ( isTRUE(all.equal(a,b2)) ) next
  b3 <- minbinder(x$probs, method="avg", max.k=n)$cl
  if ( isTRUE(all.equal(a,b3)) ) next
  b4 <- minbinder(x$probs, method="comp", max.k=n)$cl
  if ( isTRUE(all.equal(a,b4)) ) next
  break
}

binder(a,x$probs)
binder(b1,x$probs)
binder(b2,x$probs)
binder(b3,x$probs)
binder(b4,x$probs)

d <- salso:::minimize.by.enumeration(x$probs, loss="binder")
binder(d,x$probs)

x$seed <- seed
x$salso <- a
x$lg <- b1
x$wg <- b2
x$avg <- b3
x$comp <- b4
x$enum <- d
x$adjMatrices <- adjMatrices

saveRDS(x,sprintf("only-salso-wins-%s.rds",runif(1,0,100000)))

