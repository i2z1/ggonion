test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
testthat::test_that("plot ggonion", {
  x <- c("one", "two", "three")
  clr <- c("red","yellow", "green")
  ggonion(x, ratio = 2, bias = 0, color = clr)
})
