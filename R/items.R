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


# defenition of the cosine similarity metric for two vectors
cosine <- function (x, y) crossprod(x, y) / sqrt(crossprod(x) * crossprod(y))


cosineMatrix <- function(m) {
  # takes hits item matrix and calculates cosine similarity between items.
  # returns matrix
  n <- length(m[, 1])
  res <- matrix(NA, nrow = n, ncol = n, dimnames = dimnames(m))

  # calculate crossproduct matrix.
  prod.crossprod <- crossprod(m)

  # calculate similarity matrix for products.
  for(i in 1:n) {
    for(j in 1:i) {
      res[i,j] <- prod.crossprod[i, j] / sqrt(prod.crossprod[i, i] * prod.crossprod[j, j])
    }
  }
  res[upper.tri(res)] <- t(res)[upper.tri(res)]
  return(res)
}


