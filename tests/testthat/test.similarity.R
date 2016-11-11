context("Similarity functions.")

test_that("Similarity matrix is calculated correctly.", {
   m <- matrix(c(1, 0, 0, 1, 1, 0, 0, 1, 1), nrow = 3, ncol = 3)
   res <- cosineCpp(m)
   expect_identical(res[1, 3], 0, "Products are ortoganal, cosine is zero.")
   expect_identical(res[2, 2], 1, "Product self cosine is one.")
   expect_identical(res[2, 3], 0.5, "Product have overlapping values.")
})
