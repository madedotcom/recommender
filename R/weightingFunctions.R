#' A function that applies a weighting factor to the columns of a table
#'
#' @param data a table with similarity scores
#' @param weights a vector with weights for the columns of the data table
#' @param default a default weight to be applied
abjustSimMatrix <- function(data, weights, default = 0) {
  weights.init <-  data.table(sku = colnames(data),
                              weights = default)
  weights.given <- data.table(sku = names(weights),
                              weights = weights)
  weights.final <- merge(weights.init, weights.given, by = "sku", all.x=TRUE)

  weights.final[, weight := pmax(weights.x, weights.y, na.rm = TRUE)]

  # Vector of weights to adjust values in similarity matrix
  weights <- weights.final$weight
  names(weights) <- weights.final$sku

  # Multiply values in columns by corresponding values in vector
  res <- sweep(data, MARGIN = 2, weights, `*`)
  return (res)
}
