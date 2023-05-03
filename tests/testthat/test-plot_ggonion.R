test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
testthat::test_that("plot ggonion", {
  x <- c("one", "two", "three")
  clr <- c("red","yellow", "green")
  ggonion(x, ratio = 2, bias = 0, color = clr)
})
testthat::test_that("plot ggonion with themes", {
  x <- c("one", "two", "three")
  clr <- c("red","yellow", "green")
  base <- ggonion(x, ratio = 2, bias = 0, color = clr)
  base + theme_grey() + ggtitle("theme_grey()")
  base + theme_bw() + ggtitle("theme_bw()")
  base + theme_linedraw() + ggtitle("theme_linedraw()")
})
