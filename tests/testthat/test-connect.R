test_that("bvq_connect returns the correct credentials", {
    withr::local_envvar(FORMR_PWD = Sys.getenv("FORMR_PWD"))
    
    expect_true(
        bvq_connect(verbose = FALSE, password = Sys.getenv("FORMR_PWD")),
        "bvq_connect returns success code after loggin in"
    )
    expect_invisible(
        bvq_connect(verbose = FALSE, password = Sys.getenv("FORMR_PWD")),
        "bvq_connect does not print messages in console if verbose is FALSE"
    )
})
