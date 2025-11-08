library(salso)
data(synthetic)

out <- chips(synthetic$partitions)
out$AUChips

# Plot probability vs subpartition size
plot(
  out$n_items,
  out$probability,
  ylab = "Probability of Subpartition",
  xlab = "Subpartition Size",
  ylim = c(0, 1),
  type = "l"
)

# Find subpartition with probability threshold gamma = 0.95
chips_thresholded <- threshold(out, threshold = 0.95)
chips_members <- which(chips_thresholded$chips_partition != -1)
subpartition <- chips_thresholded$chips_partition[chips_members]

# Posterior probability of subpartition
p <- chips_thresholded$probability

# Identify items not allocated to subpartition and their qmax value
qi <- chips_thresholded$unallocated[, 2] / nrow(synthetic$partitions)

# Set up empty plot for later
plot(
  type = "n",
  synthetic$data,
  xlab = expression(x[1]),
  ylab = expression(x[2]),
  cex = 1.5
)

# Plot subpartition
points(
  synthetic$data[chips_members, , drop = FALSE],
  col = subpartition + 1,
  pch = 20
)

# Plot points not included in subpartition.
# Radius of each point reflects the qmax value.
excluded <- synthetic$data[chips_thresholded$unallocated[, 1], , drop = FALSE]
if (nrow(excluded) > 0) {
  points(excluded, col = "gray", cex = 1.5 * max(0, p - qi))
}

# Cluster specific parameter estimation
# Find posterior draws that contain the subpartition
indices <- which(VI(subpartition, synthetic$partitions[, chips_members]) == 0)

# Only keep MCMC samples of mean for iterates that have our subpartition
chips_mu <- synthetic$means[indices, , , drop = FALSE]

# Cluster-specific posterior means
apply(chips_mu, c(2, 3), mean)

# Cluster-specific credible intervals
apply(chips_mu, c(2, 3), \(x) quantile(x, c(0.025, 0.975)))
