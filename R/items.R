library(data.table)

userProductHitsToMatrix <- function(product.hits) {
  # takes hits data.table with columns:
  # 1. visitor.id
  # 2. sku, or any other product attribute
  # Returns sparse matrix for interactions between products within user sessions.
  # Meaning number of visitors that viewed/bought products together.
  colnames(product.hits) <- c("visitor.id", "sku")

  product.matches <- merge(product.hits, product.hits, by = "visitor.id", allow.cartesian = TRUE)
  product.matches <- subset(product.matches, sku.x != sku.y)

  product.matrix <- dcast(product.matches, sku.x ~ sku.y, value.var = "visitor.id", fun.aggregate = length)
  product.matrix <- data.frame(product.matrix, check.names = F)
  rownames(product.matrix) <- product.matrix[, 1]
  product.matrix <- product.matrix[, -1]
  res <- as.matrix(product.matrix)
  return (res)
}

cosineMatrix <- function(m) {
  res <- cosineCpp(m)
  colnames(res) <- colnames(m)
  rownames(res) <- rownames(m)
  return (res)
}
