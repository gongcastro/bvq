test_that("values have plausible values", {
    participants <- readRDS(test_path("fixtures", "participants.rds"))
    responses <- readRDS(test_path("fixtures", "responses.rds"))
    norms <- bvq_norms(participants, responses)

    expect_true(all(between(norms$.prop, 0, 1)))
    expect_false(any(norms$.sum < 0))
    expect_false(any(norms$.n < 0))

})

test_that("column classes are the right ones", {
    participants <- readRDS(test_path("fixtures", "participants.rds"))
    responses <- readRDS(test_path("fixtures", "responses.rds"))
    norms <- bvq_norms(participants, responses)
    
    expect_true(all(class(norms$te) == "integer"))
    expect_true(all(class(norms$item) == "character"))
    expect_true(all(class(norms$language) == "character"))
    expect_true(all(class(norms$age_bin) == "numeric"))
    expect_true(all(class(norms$type) == "character"))
    expect_true(all(class(norms$lp) == "character"))
    expect_true(all(class(norms$semantic_category) == "character"))
    expect_true(all(class(norms$item_dominance) == "character"))
    expect_true(all(class(norms$label) == "character"))
    expect_true(all(class(norms$.sum) == "integer"))
    expect_true(all(class(norms$.n) == "integer"))
    expect_true(all(class(norms$.prop) == "numeric"))
})
