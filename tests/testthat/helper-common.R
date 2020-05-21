draws <- salso::iris.clusterings
psm <- salso::psm(draws)
subset <- draws[sample(1:nrow(draws),10),]
n <- ncol(draws)
p1 <- draws[1,]
p2 <- draws[2,]
