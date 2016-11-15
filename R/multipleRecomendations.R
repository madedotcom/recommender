get.next.item <- function(product.sim, items) {
  # The function that given a set of items and a data matrix returns the 
  # closest SKU to that group
  # product.sim - normalized vector of similarities (0 - 1)
  # items - index of the items viewed, bought etc from a user
  
  # Select the appopriate columns
  data<-product.sim[,items]
  
  # Calculate the similarity per SKU with the set of items we already
  # know and flat out the ones we inputed
  closestSKU<-rowSums(data)
  closestSKU[items]<-0
  
  # Return the index of the most similar SKU in the product.sim
  return (which.max(closestSKU))
}

product.recommendation <- function(data,SKU,n.of.rec) {
  # The function that returns a vector of recomended SKUs
  # for a given set of items and a matrix of normalized similarities
  # data - normalized vector of similarities (0 - 1)
  # SKU - index of the items viewed, bought etc from a user
  # n.of.rec - number of recommendations to return
  
  items <- unlist(lapply(SKU, function(x) {
    grep(x,colnames(data))
  })
  )
  
  if (length(items) < 1){
    return (NA)
  }
  
  recomend <- rep(NA,n.of.rec)
  
  for(i in 1:n.of.rec){
    index <- get.next.item(data,items)
    recomend[i] <- colnames(data)[index] 
    items <- c(items,index)
  }
  
  #Return a vector of recommendations
  return (recomend)
}

