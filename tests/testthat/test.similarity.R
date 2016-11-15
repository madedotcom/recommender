context("Similarity functions.")

test_that("Similarity matrix is calculated correctly.", {
   m <- matrix(c(1, 0, 0, 1, 1, 0, 0, 1, 1), nrow = 3, ncol = 3)
   res <- cosineCpp(m)
   expect_identical(res[1, 3], 0, "Products are ortoganal, cosine is zero.")
   expect_identical(res[2, 2], 1, "Product self cosine is one.")
   expect_identical(res[2, 3], 0.5, "Product have overlapping values.")
   
   # test that colnames and rownames are not changed
   colnames(m) <- rownames(m) <- c("a", "b", "c")
  
   res <- cosineMatrix(m)
   
   expect_equal(colnames(res), colnames(m), "Column names in result are same as original matrix.")
   expect_equal(rownames(res), rownames(m), "Row names in result are same as original matrix.")
})

test_that("Identifies the correct row", {
  m <- matrix(c(1.00, 0.52, 0.41, 0.52, 
                0.60, 0.60, 0.23, 0.26, 
                0.40, 0.30, 0.83, 0.26, 
                0.62, 0.80, 0.25, 0.16 ), nrow = 4, ncol = 4)
  
  res <- getNextItem(m, c(1, 2))
  
  # Test that the function behaves correctly and identifies the 
  # correct row
  expect_equal(4, res, "The correct row was identified")
})

test_that("Wrong SKU's don't break the reccomendations", {
  items <- c("IamNotaSKU", "IamAlsoNotaSKU")
  m <- matrix(c(1.00, 0.52, 0.41, 0.52, 
                0.60, 0.60, 0.23, 0.26, 
                0.40, 0.30, 0.83, 0.26, 
                0.62, 0.80, 0.25, 0.16 ), nrow = 4, ncol = 4)
  
  # Test that a wrong input SKU will return NA
  res <-  productRecommendation(m, items, 2)
  expect_equal(res, NA, "NA is returned as expected")
})

