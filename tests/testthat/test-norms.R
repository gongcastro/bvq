test_that("values have plausible values", {
    norms <- readRDS(test_path("fixtures", "norms.rds"))
    
    expect_true(all(between(norms$proportion, 0, 1)))
    expect_false(any(norms$yes < 0))
    expect_false(any(norms$n < 0))
    expect_false(any(norms$se < 0))
    expect_true(all(between(norms$ci_lower, 0, 1)))
    expect_false(any(norms$ci_lower < 0))
    expect_true(all(between(norms$ci_upper, 0, 1)))
    expect_false(any(norms$ci_upper < 0))
    
})

test_that("column classes are the right ones", {
    norms <- readRDS(test_path("fixtures", "norms.rds"))
    
    expect_true(all(class(norms$te) == "integer"))
    expect_true(all(class(norms$item) == "character"))
    expect_true(all(class(norms$language) == "character"))
    expect_true(all(class(norms$age_bin) == "numeric"))
    expect_true(all(class(norms$type) == "character"))
    expect_true(all(class(norms$lp) == "character"))
    expect_true(all(class(norms$semantic_category) == "character"))
    expect_true(all(class(norms$item_dominance) == "character"))
    expect_true(all(class(norms$label) == "character"))
    expect_true(all(class(norms$yes) == "integer"))
    expect_true(all(class(norms$proportion) == "numeric"))
    expect_true(all(class(norms$se) == "numeric"))
    expect_true(all(class(norms$ci_lower) == "numeric"))
    expect_true(all(class(norms$ci_upper) == "numeric"))
})
