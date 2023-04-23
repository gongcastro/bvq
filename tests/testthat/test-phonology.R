test_that("flatten_xsampa works", {
    vct <- pool$xsampa[1:10]
    vct_flat <- flatten_xsampa(vct)
    vct_corr <- c("p@siGOL@s", "@B4@sa", "uB4i", "@k@Ba", "L@nsa", "@p@Ga",
                  "@p4End4@", "@zg@r@pa", "@ZuDa", "b@La")
    expect_length(vct_flat, length(vct))
    expect_type(vct_corr, "character")
    expect_equal(vct_flat, vct_corr)
})

test_that("syllabify_xsampa works", {
    vct <- pool$xsampa[1:10]
    syll <- syllabify_xsampa(vct)
    corr <- list(c("p@", "si", "GO", "L@s"),
                 c("@", "B4@", "sa"),
                 c("u", "B4i"),
                 c("@", "k@", "Ba"),
                 c("L@n", "sa"),
                 c("@", "p@", "Ga"),
                 c("@", "p4En", "d4@"), 
                 c("@z", "g@", "r@", "pa"),
                 c("@", "Zu", "Da"),
                 c("b@", "La"))
    expect_length(syll, length(vct))
    expect_type(syll, "list")
    expect_type(syll[[1]], "character")
    expect_equal(syll, corr)
})

test_that("syllable_str_xsampa works", {
    vct <- pool$xsampa[1:10]
    syll <- syllable_str_xsampa(vct)
    corr <- list(c("CV", "CV", "CV", "CVC"),
                        c("V", "CCV", "CV"),
                        c("V", "CCV"),
                        c("V", "CV", "CV"),
                        c("CVC", "CV"),
                        c("V", "CV", "CV"),
                        c("V", "CCVC", "CCV"),
                        c("VC", "CV", "CV", "CV"), 
                        c("V", "CV", "CV"),
                        c("CV", "CV"))
    expect_length(syll, length(vct))
    expect_type(syll, "list")
    expect_type(syll[[1]], "character")
    expect_equal(syll, corr)
})




