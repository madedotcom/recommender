
getSimilarProducts <- function(sim.matrix, skus, values, exclude.same, groups = NULL) {
  # Provides similar product recommendations
  #
  # Params:
  # @sim.matrix - similarity matrix.
  # @skus - skus derived from the visitor history.
  # @values - required number of recommendations.
  # @exclude.same - excludes recommendations for values in skus.
  # @groups - named vector of sku categories.

  missing.skus <- setdiff(skus, rownames(sim.matrix))
  if(length(missing.skus) > 0) {
    warning("Following skus are missing from the sim.matrix: ", paste(missing.skus, collapse = ", "))
  }
  # only keep skus that are in the similarity matrix
  skus <- setdiff(skus, missing.skus)
  
  # Turn recommendations matrix into a normalised data table
  # We are filtering to the list of relevant skus.
  product.affinity <- melt(sim.matrix[skus, , drop=FALSE], na.rm = T)
  colnames(product.affinity) <- c("sku", "sku.rec", "sim")
  product.affinity <- data.table(product.affinity, key = c("sku", "sku.rec"))
  if(exclude.same) {
    product.affinity <- product.affinity[!(sku.rec %in% skus)]
  }
  combined.scores <- product.affinity[, list(sim = mean(sim)), by = sku.rec]
  setkey(combined.scores, "sku.rec")


  if(!missing(groups)) {
    # Append group data to affinity table
    groups.table <- data.table(sku = names(groups), group = groups, key= "sku")
    combined.scores <- combined.scores[groups.table, nomatch = 0]

    # Get the best performing sku per group
    # http://stackoverflow.com/questions/16573995/subset-by-group-with-data-table
    combined.scores <- combined.scores[combined.scores[, .I[sim == max(sim)], by = group]$V1]
  }

  # Limit results to the requested number of skus
  res <- head(combined.scores[order(sim, decreasing = T)]$sku.rec, values)

  return (res)
}

