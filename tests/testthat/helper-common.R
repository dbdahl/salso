draws <- salso::iris.clusterings
psm <- salso::psm(draws)
# Leave out draws with one cluster: their losses against each other are 0/0, which aricode and
# mcclust give as NaN.
subset <- draws[sample(which(apply(draws, 1, \(x) length(unique(x))) > 1),10),]
n <- ncol(draws)
p1 <- draws[1,]
p2 <- draws[2,]
