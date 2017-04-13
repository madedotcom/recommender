context("Library functions.")

test_that("Simplify lists", {
  
  f <- c("A", "B", "C")
  s <- c("a", "b", "c")
  
  res <- simplify.transactions(f, s)

  expect_true(res[["A"]] == "a", "Correct naming applied")
  
  f <- c("A", "B", "A")
  s <- c("a", "a", "b")
  
  res <- simplify.transactions(f, s)
  expect_true(res[["A"]][2] ==  "b", "Correct grouping applied")

})

test_that("Connect order types ", {
  
  f <- c("A", "B", "A")
  s <- c("a", "b", "c")
  
  orders <- simplify.transactions(f, s)

  f <- c("a", "b", "c")
  s <- c("a1", "b1", "a2")
  
  mapping <- simplify.transactions(f, s)
  
  # Given the two lists orders and mapping constructs a melted version of a two column connection between
  # them based of s1 and f1 
  counts <- calculateConnectionMatrix(orders, mapping)
  # Casting the result of the melt into a table of counts
  res <- connectionsToCounts(counts)
  
  expect_true(res[type == "a1"]$a2 == 1, "Correct counts returned")
})
