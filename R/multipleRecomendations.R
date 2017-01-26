library(data.table)

getNextItem <- function(product.sim, items) {
  # The function that given a set of items and a data matrix returns the 
  # closest SKU to that group
  # product.sim - normalized matrix of similarities (0 - 1)
  # items - index of the items viewed, bought etc from a user
  
  # Select the appopriate columns
  data <- product.sim[, items]
  
  # Calculate the similarity per SKU with the set of items we already
  # know and flat out the ones we inputed. 
  if (length(items) > 1) {
    closest.sku <- rowSums(data)
  } else {
    closest.sku <- data
  }
  
  closest.sku[items] <- 0
  
  # Return the index of the most similar SKU in the product.sim
  return (which.max(closest.sku))
}

productRecommendation <- function(data, SKU, n.of.rec) {
  # The function that returns a vector of recomended SKUs
  # for a given set of items and a matrix of normalized similarities
  # data - normalized matrix of similarities (0 - 1)
  # SKU - index of the items viewed, bought etc from a user
  # n.of.rec - number of recommendations to return
  
  items <- unlist(lapply(SKU, function(x) {
    grep(x, colnames(data))
  })
  )
  
  if (length(items) < 1) {
    return (NA)
  }
  
  recomend <- rep(NA, n.of.rec)
  
  for(i in 1:n.of.rec){
    index <- getNextItem(data, items)
    recomend[i] <- colnames(data)[index] 
    items <- c(items, index)
  }
  
  #Return a vector of recommendations
  return (recomend)
}


getSimilarProducts <- function(sim.matrix, skus, values, exclude.same, groups = NULL) {
  # Provides similar product recommendations
  #
  # Params:
  # @sim.matrix - similarity matrix.
  # @skus - skus derived from the visitor history.
  # @values - required number of recommendations.
  # @exclude.same - excludes recommendations for values in skus.
  # @groups - named vector of sku categories.
  
  # Turn recommendations matrix into a normalised data table
  # We are filtering to the list of relevant skus.
  product.affinity <- melt(sim.matrix[skus, ], na.rm = T)
  colnames(product.affinity) <- c("sku", "sku.rec", "sim")
  product.affinity <- data.table(product.affinity, key = c("sku", "sku.rec"))
  levels(product.affinity$sku) <- levels(product.affinity$sku.rec)
  if(exclude.same) {
    product.affinity <- product.affinity[sku != sku.rec]
    product.affinity <- product.affinity[!sku.rec %in% skus]
  }
  combined.scores <- product.affinity[, list(sim = mean(sim)), by = sku.rec]
  setkey(combined.scores, "sku.rec")
  
  
  if(!missing(groups)) {
    # Append group data to affinity table
    groups.table <- data.table(sku = names(groups), group = groups, key= "sku")
    combined.scores <- combined.scores[groups.table]
    
    # Get the best performing sku per group
    combined.scores <- combined.scores[combined.scores[, .I[sim == max(sim)], by = group]$V1]
  }

  # Limit results to the requested number of skus
  res <- head(combined.scores[order(sim, decreasing = T)]$sku.rec, values)
  
  return (res)
}

