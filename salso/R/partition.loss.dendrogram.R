partition.loss.dendrogram <- function(estimate, x, loss) {
  isPSM <- isPSM(x)
  height <- partition.loss(estimate, x, loss)
  labels <- unique(estimate)
  specific.heights <- sapply(labels, function(label) {
    subset <- estimate == label
    y <- if (isPSM) {
      x[subset, subset, drop = FALSE]
    } else {
      x[, subset, drop = FALSE]
    }
    partition.loss(estimate[subset], y, loss)
  })
  min <- min(specific.heights)
  range <- max(specific.heights) - min
  candidates <- lapply(labels, function(label) {
    scale.factor <- if (range > 0) {
      (specific.heights[label] - min) / range
    } else {
      1
    }
    new.height <- height * scale.factor
    class(label) <- "dendrogram"
    attr(label, "members") <- 1
    attr(label, "height") <- new.height
    attr(label, "label") <- as.character(label)
    attr(label, "leaf") <- TRUE
    label
  })
  state <- list(estimate, candidates)
  while (length(unique(state[[1]])) != 1) {
    state <- agglomerate.dendrogram(state, x, loss)
  }
  state[[2]][[unique(state[[1]])]]
}

agglomerate.dendrogram <- function(state, x, loss) {
  estimate <- state[[1]]
  candidates <- state[[2]]
  tab <- t(apply(combn(unique(estimate), 2), 2, function(pair) {
    estimate[estimate == pair[2]] <- pair[1]
    c(pair, partition.loss(estimate, x, loss))
  }))
  which <- which.min(tab[, 3])
  pair <- tab[which, 1:2]
  estimate[estimate == pair[2]] <- pair[1]
  height <- tab[which, 3]
  candidates[[pair[1]]] <- {
    branch1 <- candidates[[pair[1]]]
    branch2 <- candidates[[pair[2]]]
    xx <- list()
    xx[[1]] <- branch1
    xx[[2]] <- branch2
    class(xx) <- "dendrogram"
    attr(xx, "members") <- attr(branch1, "members") + attr(branch2, "members")
    mid1 <- if (isTRUE(attr(branch1, "leaf"))) 0 else attr(branch1, "midpoint")
    mid2 <- if (isTRUE(attr(branch2, "leaf"))) 0 else attr(branch2, "midpoint")
    attr(xx, "midpoint") <- (mid1 + attr(branch1, "members") + mid2) / 2
    attr(xx, "height") <- height
    xx
  }
  candidates[pair[2]] <- list(NULL)
  list(estimate, candidates)
}
