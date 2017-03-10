#' Calculates a weighting factor based on the stock level of a SKU
#' Everything with 0 stock is weighted to 0 and everything else
#' is fitted with a linear fit to y = 0.005x with max 1.0 at 200 stock levels
#' 
#' @param stock number of items in stock
#' 
fitStockLevel <- function(stock){
  if (stock <= 0 || is.na(stock)) return (0)
  return ( ifelse(stock >= 200, 1.0, stock * 0.005))
}

#' Calculates a weighting factor based on the lead time of a SKU
#' Everything with a negative or more than 80 days lead will be set to 0 
#' and all other will be fitted to a linear fit of y = 0.0125x with max 1.0 
#' between 0 and 20 lead days
#' 
#' 
#' @param lead lead time for a given sku
#' 
fitLeadLevel <- function(lead){
  if (lead < 0 || lead > 80 || is.na(lead)) return (0)
  return (ifelse(lead <= 20, 1.0, 1 - (lead * 0.0125)))
}

#' General function to calculate the weights for a 
#' number of skus
#' 
#' @param level number to be fitted
#' @param type type of data given "stock" or "lead"
#' 
getProductkWeights <- function(level, type){
  
  if (type == "stock"){
    res <- sapply(level, fitStockLevel)
  } 
  if (type == "lead"){
    res <- sapply(level, fitLeadLevel)
  }
  
  return(res)
}
