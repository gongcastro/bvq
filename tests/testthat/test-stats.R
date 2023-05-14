test_that("adjusted proportions are computed correctly", {
  prop <- prop_adj(4, 60)
  prop_0 <- prop_adj(0, 60)
  prop_1 <- prop_adj(60, 60)

  expect_identical(round(prop, digits = 5), expected = 0.09375)
  expect_gt(prop_0, 0)
  expect_lt(prop_1, 1)
})
