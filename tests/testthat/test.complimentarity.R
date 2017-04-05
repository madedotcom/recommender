context("Post order functions.")

test_that("Top types are correctly retrieved", {
  
  type <- "A"
  data <- data.table(Type = c("A","B","C","D"), 
                     A = c(1.0, 0.4, 0.5, 0.1), 
                     B = c(0.1, 1.0, 0.2, 0.8), 
                     C = c(0.2, 0.6, 1.0, 0.9), 
                     D = c(0.6, 0.2, 0.8, 1.0))
  
  res <- returnTopTypes(type, data, 1, TRUE)
  expect_equal(res, "D", "Return the correct match.")
  
  res <- returnTopTypes(type, data, 1, FALSE)
  expect_equal(res, "A", "Return the correct match if not excluding itself.")
  
  type <- c("A", "D")
  res <- returnTopTypes(type, data, 1, TRUE)
  expect_equal(res, "C", "Return the correct match multiple types.")
  
  type <- c("A", "D")
  res <- returnTopTypes(type, data, 1, FALSE)
  expect_equal(res, "D", "Return the correctmatch multiple types if not excluding itself.")
  
  type <- c("A", "D", "Error")
  res <- returnTopTypes(type, data, 1, FALSE)
  expect_equal(res, "D", "Return the correctmatch multiple types if not excluding itself
               with extra unknown columns.")
  
  type <- c("As", "Ds")
  res <- returnTopTypes(type, data, 1, FALSE)
  expect_equal(res, NA, "Return NA if the types are unknown.")
  
})


test_that("Post order recommendations correctly retrieved", {
  
  sku.details <- data.table(sku = c("A", "B", "C", "D"),
                            category = c("A1", "A1", "A2", "A3"))
  sim.matrix <- c(1.00, 0.52, 0.41, 0.52,
                  0.60, 0.60, 0.23, 0.26,
                  0.40, 0.30, 0.83, 0.26,
                  0.62, 0.80, 0.25, 0.16 )
  sim.matrix <- matrix(sim.matrix, nrow = 4, ncol = 4)
  colnames(sim.matrix) <- rownames(sim.matrix) <- c("A", "B", "C", "D")
  
  post.order.matrix <- data.table(Type = c("A1", "A2", "A3"),
                                  A1 = c(1.0, 0.5, 0.2),
                                  A2 = c(0.5, 1.0, 0.1),
                                  A3 = c(0.2, 0.1, 1.0))
  skus <- "A"
  sku.types <- "A1"
  
  res <- crossSaleRecommendations(sim.matrix, sku.details, post.order.matrix,
                                  skus, sku.types, 2, exclude.same = TRUE)
  expect_equal(as.character(res), c("D", "C"), "Bring the correct post recommendations.")
  
  skus <- c("A", "C")
  sku.types <- c("A1", "A2")
  
  res <- crossSaleRecommendations(sim.matrix, sku.details, post.order.matrix,
                                  skus, sku.types, 2, exclude.same = TRUE)
  expect_equal(as.character(res), c("D", "C"), "Bring the correct post recommendations.")
  
})

