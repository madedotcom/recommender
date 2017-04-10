#' Simplify two vector lists
#'
#' @param f - Vector to be used as the grouping 
#' @param s - Vector to include the values per grouping category
simplify.transactions <- function(f, s) {
  tapply(s, f, function(x) {unique(x)}, simplify = T)
}

#' Calculates for each UID the connection as a list of two column results
#' on the first resulting column there is the first order type and on the 
#' second column is the subsequent order type.
#'
#' @param orders - a list of orders mapped per customer
#' @param mapping - a list of types per item of each order
calculateConnectionMatrix <- function(orders, mapping) {
  res <-lapply(orders, function(x, o){
    if( length(x) > 1) {
      connectOrderMapping(x, o)
    } else {
      NA
    }
  }, o = mapping)
  return (res)
}

#' Calculates the connections matrix given the pair of orders
#' 
#'@param data - a data table with sku1 and sku2 columns that hold the first order type and second order type
connectionsToCounts <- function(data) {
  data <- data[!is.na(data)]
  data.list <- rbindlist(data)
  
  res <- dcast(data.list, sku1 ~ sku2, fun.aggregate = length)
  res[, type := gsub(" ", "_", sku1)]
  res[, sku1 := NULL]
  
  return (res)
}


#' The function that drills in each order and associates the types
#' based on the order history
#' 
#'
#' @param list.of.orders - a single list of orders
#' @param order.mapping - a single list of items per order
connectOrderMapping <- function(list.of.orders, order.mapping){
  sku1 <- NULL
  sku2 <- NULL
  for (i in 1:(length(list.of.orders) - 1)){
    tmpSkus <- order.mapping[[list.of.orders[i]]] # Get the categories of the first order
    tmpSkus2<- order.mapping[[list.of.orders[i+1]]] # Get the categories of the next order
    sku1 <- c(sku1, rep(tmpSkus,  length(tmpSkus2))) # Each item of the first order connects to each item of the second order
    sku2 <- c(sku2, rep(tmpSkus2, length(tmpSkus))) # Repeat the second order to match each items of the first
  }
  return (data.frame(t(rbind(sku1, sku2))))
}
