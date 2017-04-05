context("Complimentarity functions.")

test.sim.matrix <- c(1.00, 0.52, 0.41, 0.52,
                     0.60, 0.60, 0.23, 0.26,
                     0.40, 0.30, 0.83, 0.26,
                     0.62, 0.80, 0.25, 0.16 )

test.sim.matrix <- matrix(test.sim.matrix, nrow = 4, ncol = 4)
colnames(test.sim.matrix) <- rownames(test.sim.matrix) <- c("a", "b", "c", "d")

test_that("Return top types.", {
  expect_equal(1==0, "Wrong type returned")
})