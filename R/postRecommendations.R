#' Function to return complimentary product recommendations
#'
#' @export
#' @import data.table
#' @param sim.matrix - similarity matrix.
#' @param skus - skus derived from the visitor history.
#' @param values - required number of recommendations.
#' @param exclude.same - excludes recommendations for values in skus.
#' @param groups - named vector of sku categories.
#' @param affinity - affinity matrix
#' @param sku.details - named vector of skus and types
#' @param n.of.types - number of types the results should be selected from
getComplimentaryProducts <- function(sim.matrix, skus, values, exclude.same = TRUE, groups = NULL,
                                     affinity, sku.details, n.of.types) {
  category <- NULL

  sku.types <- names(skus)
  types <- getNextOrderTopTypes(sku.types, affinity, n.of.types, exclude.same)
  selected.skus <- sku.details[category %in% types]

  positions.to.retain <- (row.names(sim.matrix) %in% selected.skus$sku)
  rec <- getSimilarProducts(sim.matrix[, positions.to.retain], skus, values, exclude.same = TRUE)

  return (rec)
}

#' The function that return the n nearest types to the given ones
#'
#' @export
#' @param types - the vector of types (subcategory/category/type) to be checked
#' @param nextOrderMatrix - the matrix of connections between the types
#' @param n.of.types - the number of n nearest types
#' @param exclude.same - remove the same type from the results
getNextOrderTopTypes <- function(types, nextOrderMatrix, n.of.types = 3,
                           exclude.same = TRUE) {
  type <- NULL

  res <-  nextOrderMatrix[type %in% types]
  if (dim(res)[1] == 0) {
    return (NA)
  }
  res[, type := NULL]
  res <- colSums(res)

  order.data <- order(-res)
  res.type <- names(res)[order.data]
  if (exclude.same){
    res.type <- res.type[!(res.type %in% types)]
  }
  return (res.type[1:n.of.types])
}

#' Given a list of transactions the post order affinity table is calculated.
#' The expected format of transactions is a a data.table with
#' UID - unique customer identifier
#' reference - Grouping of orders
#' type - type per item in an order
#'
#' @export
#' @param transactions - a data.table with transaction data
#' @return data.table with counts of type affinities based on the transaction data
getProductGroupAffinities <- function(transactions) {
  # Calculate the type
  uid.orders <- simplify.transactions(transactions$UID, transactions$reference)

  # Calculate the types connection matrix
  orders.type <- simplify.transactions(transactions$reference, transactions$type)

  map.type   <- calculateConnectionMatrix(uid.orders, orders.type)
  map.matrix <- connectionsToCounts(map.type)

  return (map.matrix)
}
