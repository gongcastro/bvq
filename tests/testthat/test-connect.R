test_that("bvq_connect returns the correct credentials", {
  expect_true(
    bvq_connect(),
    "bvq_connect returns success code after loggin in"
  )
    expect_invisible(
        bvq_connect(verbose = FALSE),
        "bvq_connect does not print messages in console if verbose is FALSE"
    )
})
