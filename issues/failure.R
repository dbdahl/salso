library(salso)

load("probs.Rbin")
result <- salso(probs, parallel=TRUE, maxScans=1, nPermutations=5000, probExploration=0.001)
result
result$estimate
VI.lb(result$estimate, probs)
result$expectedLoss

# But, I can do better "by-hand"
VI.lb(c(0,0,1,1,1,2,3,3,2),probs)


VI.lb(c(0,0,1,1,1,2,3,3,0),probs)
VI.lb(c(0,0,1,1,1,2,3,3,1),probs)
VI.lb(c(0,0,1,1,1,2,3,3,2),probs)
VI.lb(c(0,0,1,1,1,2,3,3,3),probs)
VI.lb(c(0,0,1,1,1,2,3,3,4),probs)


VI.lb(c(0,0,1,1,1,2,3,3,4),probs)
VI.lb(c(0,1,2,3,4,5,6,7,8),probs)
VI.lb(c(0,1,2,2,2,3,4,5,6),probs)    # Best



VI.lb2 <- mcclust.ext::VI.lb
VI.lb2(result$estimate, probs)
VI.lb2(c(0,0,1,1,1,2,3,3,4),probs)
VI.lb2(c(0,1,2,3,4,5,6,7,8),probs)
VI.lb2(c(0,1,2,2,2,3,4,5,6),probs)




p <- probs[1:4,1:4]
VI.lb2(c(0,1,2,2),p)
VI.lb2(c(0,1,2,3),p)


library(mcclust.ext)
minVI(probs, method="greedy")


library(mcclust)
data(cls.draw1.5)
probs <- psm(cls.draw1.5)

r1 <- salso(probs, nPermutations=5000, probExploration=0.001)
r2 <- salso(probs, nPermutations=5000, probExploration=0.0)
table(r1$estimate,r2$estimate)

# minVI(probs, method="greedy")
