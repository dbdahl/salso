library(salso)

load("probs.Rbin")
result <- salso(probs)

result$estimate
result$expectedLoss

# But, I can do better "by-hand"
VI.lb(c(0,0,1,1,1,2,3,3,4),probs)

