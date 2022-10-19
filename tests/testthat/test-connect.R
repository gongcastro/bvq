test_that("bvq_connect returns the correct credentials", {

  expect_true(
    bvq_connect(),
    "bvq_connect returns success code after loggin in"
  )

})



