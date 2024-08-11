test_that("bvq_connect returns the correct credentials", {
  skip("skip for now")

  expect_true(
    bvq_connect(password = Sys.getenv("FORMR_PWD")),
    "bvq_connect returns success code after loggin in"
  )

  expect_invisible(
    bvq_connect(password = Sys.getenv("FORMR_PWD")),
    "bvq_connect does not print messages in console if verbose is FALSE"
  )
})
