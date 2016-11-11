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
