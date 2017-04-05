#' Function to return complimentary product recommendations
#'
#' @param sim.matrix - similarity matrix.
#' @param skus - skus derived from the visitor history.
#' @param values - required number of recommendations.
#' @param exclude.same - excludes recommendations for values in skus.
#' @param groups - named vector of sku categories.
#' @param affinity.groups - grouping affinity
#' @param affinity - affinity matrix
getComplimentaryProducts <- function(sim.matrix, skus, values, exclude.same, groups = NULL, 
                                     affinity){

  poi
  
  getSimilarProducts(sim.matrix[, positions.to.retain], 
                     browsed.skus, results, exlcude.same)  
    
}

#' Return the affinity matrix from s3
#'
#' @param type - the given the level of affinity connection (subcategory, category, type)
getAffinity <- function(type){
  affinity <- s3GetFile(paste0("recommendations/", type, "_next_order.csv"))
  return (affinity)
}

#'
#'
#'
#'
complimentaryFiltering <- function(affinity, ){
  complimentary.skus <- NULL
  
  return (complimentary.skus)
}

