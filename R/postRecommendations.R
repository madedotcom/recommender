#' Function to return complimentary product recommendations
#'
#' @param sim.matrix - similarity matrix.
#' @param skus - skus derived from the visitor history.
#' @param values - required number of recommendations.
#' @param exclude.same - excludes recommendations for values in skus.
#' @param groups - named vector of sku categories.
#' @param affinity - affinity matrix
#' @param sku.details - named vector of skus and types
#' @param n.of.types - number of types the results should be selected from
getComplimentaryProducts <- function(sim.matrix, skus, values, exclude.same = TRUE, groups = NULL, 
                                     affinity, sku.details, n.of.types){

  sku.types <- names(skus)
  
  types <- returnTopTypes(sku.types, affinity, n.of.types, exclude.same) 

  selected.skus <- sku.details[category %in% types]
  
  positions.to.retain <- (row.names(sim.matrix) %in% selected.skus$sku)
  rec <- getSimilarProducts(sim.matrix[, positions.to.retain], skus, values, exclude.same)

  return (rec)
}

#' Return the affinity matrix from s3
#'
#' @param type - the given the level of affinity connection (subcategory, category, type)
getAffinity <- function(type){
  affinity <- s3GetFile(paste0("recommendations/", type, "_next_order.csv"))
  return (affinity)
}


#' The function that return the n nearest types to the given ones
#'
#' @param type - the type (subcategory/category/type) to be checked
#' @param data - the matrix of connections between the types
#' @param n.of.types - the number of n nearest types
#' @param exlude.same - remove the same type from the results
returnTopTypes <- function(type, data, n.of.types = 3,
                           exclude.same = TRUE) {
  res <-  data[Type %in% type]
  if (dim(res)[1] == 0) {
    return (NA)
  }
  res[, Type := NULL]
  res <- colSums(res)
  
  order.data <- order(-res)
  res.type <- names(res)[order.data]
  if (exclude.same){
    res.type <- res.type[!(res.type %in% type)]
  }
  return (res.type[1:n.of.types])
}

