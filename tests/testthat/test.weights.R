context("Weights for the similarity matrix")

test.sim.matrix <- c(1.00, 0.52, 0.41, 0.52,
                     0.60, 0.60, 0.23, 0.26,
                     0.40, 0.30, 0.83, 0.26,
                     0.62, 0.80, 0.25, 0.16 )

test.sim.matrix <- matrix(test.sim.matrix, nrow = 4, ncol = 4)
colnames(test.sim.matrix) <- rownames(test.sim.matrix) <- c("a", "b", "c", "d")

test_that("Weighting a similarity matrix", {
  
  m <- test.sim.matrix
  w <- c("a" = 1, "e" = 0.22, "b" = 0.75, "c" = 0.5, "d" = 0.25)
  res <- adjustSimMatrix(m, w)
  
  expect_identical(m[, 1], res[, 1], "First column returned correctly with no changes")
  expect_identical(m[, 2]*0.75, res[, 2], "Order is kept and calculation is correct")
  
  w <- c("a" = 1)
  res <- adjustSimMatrix(m, w)
  expect_identical(m[, 3] * 0, res[, 3], "Defaulting is working as expected")
  
  w <- c("b" = 0.2, "a" = 1)
  res <- adjustSimMatrix(m, w)
  expect_identical(m[, 2] * 0.2, res[, 2], "Ordering is kept intact")
  
  w <- c("a" = 0.5, "b" = 0.2, "a" = 1)
  res <- adjustSimMatrix(m, w)
  expect_identical(m[, 2] * 0.2, res[, 2], "Correct weight is applied for the second product")
  expect_identical(m[, 1] * 0.5, res[, 1], "Duplicate weight is ignored for the first product")
  
})
