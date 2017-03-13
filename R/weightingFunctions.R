
abjustSimMatrix <- function(data, weights) {
  weights.init <-  data.table(sku = colnames(data),
                              weights = 0)
  weights.given <- data.table(sku = names(weights),
                              weights = weights)
  weights.final <- merge(weights.init, weights.given, by = "sku", all.x=TRUE)

  weights.final[, weight := pmax(weights.x, weights.y, na.rm = TRUE)]
  weights <- weights.final$weight
  names(weights) <- weights.final$sku
  
  transformed.data <- sapply(names(weights), function(x) {
      data[, colnames(data) %in% x] <- data[, colnames(data) %in% x] * weights[[x]]
  })
  
  return (transformed.data)
}
