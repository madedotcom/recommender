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
#'@param data - a data table with type1 and type2 columns that hold the first order type and second order type
connectionsToCounts <- function(data) {
  data <- data[!is.na(data)]
  data.list <- rbindlist(data)
  
  res <- dcast(data.list, type1 ~ type2, fun.aggregate = length)
  res[, type := gsub(" ", "_", type1)]
  res[, type1 := NULL]
  
  return (res)
}


#' The function that drills in each order and associates the types
#' based on the order history
#' 
#'
#' @param list.of.orders - a single list of orders
#' @param order.mapping - a single list of items per order
connectOrderMapping <- function(list.of.orders, order.mapping){
  type1 <- NULL
  type2 <- NULL
  for (i in 1:(length(list.of.orders) - 1)){
    tmpTypes <- order.mapping[[list.of.orders[i]]] # Get the categories of the first order
    tmpTypes2<- order.mapping[[list.of.orders[i+1]]] # Get the categories of the next order
    type1 <- c(type1, rep(tmpTypes,  length(tmpTypes2))) # Each item of the first order connects to each item of the second order
    type2 <- c(type2, rep(tmpTypes2, length(tmpTypes))) # Repeat the second order to match each items of the first
  }
  return (data.frame(t(rbind(type1, type2))))
}
