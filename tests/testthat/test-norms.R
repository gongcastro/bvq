participants <- readRDS(test_path("fixtures", "participants.rds"))
responses <- readRDS(test_path("fixtures", "responses.rds"))
norms <- bvq_norms(participants, responses)

test_that("values have plausible values", {
  expect_true(all(between(norms$.prop, 0, 1)))
  expect_false(any(norms$.sum < 0))
  expect_false(any(norms$.n < 0))
})

test_that("column classes are the right ones", {
  expect_true(all(class(norms$te) == "integer"))
  expect_true(all(class(norms$item) == "character"))
  expect_true(all(class(norms$age) == "numeric"))
  expect_true(all(class(norms$type) == "character"))
  expect_true(all(class(norms$item_dominance) == "character"))
  expect_true(all(class(norms$label) == "character"))
  expect_true(all(class(norms$.sum) == "integer"))
  expect_true(all(class(norms$.n) == "integer"))
  expect_true(all(class(norms$.prop) == "numeric"))
})

test_that("test item argument with single item", {
  norms <- bvq_norms(participants, responses, item = "cat_gat")
  expect_equal(colnames(norms), c(
    "te", "item", "label", "age", "type",
    "item_dominance", ".sum", ".n", ".prop"
  ))
  expect_equal(unique(norms$item), "cat_gat")
})

test_that("test item argument with multiple items", {
  norms <- bvq_norms(participants, responses, item = c("cat_gat", "spa_perro"))
  expect_equal(colnames(norms), c(
    "te", "item", "label", "age", "type",
    "item_dominance", ".sum", ".n", ".prop"
  ))
  expect_equal(unique(norms$item), c("cat_gat", "spa_perro"))
})

test_that("test te = TRUE argument with single item", {
  norms <- bvq_norms(participants, responses, item = "cat_gat", te = TRUE)

  expect_equal(colnames(norms), c(
    "te", "item", "label", "age", "type",
    "item_dominance", ".sum", ".n", ".prop"
  ))
  expect_equal(unique(norms$item), c("cat_gat", "spa_gato"))
})

test_that("test te = TRUE argument with multiple items", {
  norms <- bvq_norms(participants,
    responses,
    item = c("cat_gat", "cat_gos"),
    te = TRUE
  )
  expect_equal(colnames(norms), c(
    "te", "item", "label", "age", "type",
    "item_dominance", ".sum", ".n", ".prop"
  ))
  expect_equal(unique(norms$item), c(
    "cat_gat", "spa_gato",
    "cat_gos", "spa_perro"
  ))
})

test_that("test te = FALSE argument", {
  norms <- bvq_norms(participants, responses, item = "cat_gat", te = FALSE)
  expect_equal(colnames(norms), c(
    "te", "item", "label", "age", "type",
    "item_dominance", ".sum", ".n", ".prop"
  ))
  expect_equal(unique(norms$item), c("cat_gat"))
})

test_that("test te = 175 argument", {
  norms <- bvq_norms(participants, responses, item = "cat_cuc", te = 175)

  expect_equal(colnames(norms), c(
    "te", "item", "label", "age", "type",
    "item_dominance", ".sum", ".n", ".prop"
  ))
  expect_equal(unique(norms$item), c("cat_cuc", "spa_gusano"))
})

test_that("test te = 9999 throws an error", {
  expect_error(bvq_norms(participants, responses, item = "cat_gat", te = 9999))
})

test_that("test item = 'XXXXXX' throws an error", {
  expect_error(bvq_norms(participants, responses, item = "XXXXXX"))
})

test_that("test if items not in `te` are excluded with a warning", {
  suppressMessages({
    expect_message(bvq_norms(participants, responses,
      te = 175,
      item = c("spa_gusano", "cat_gat")
    ))


    norms <- bvq_norms(participants, responses,
      te = 175,
      item = c("spa_gusano", "cat_gat")
    )

    expect_equal(unique(norms$item), c("cat_cuc", "spa_gusano"))
  })
})

test_that("test that the ... argument works", {
  norms <- bvq_norms(participants,
    responses,
    lp,
    semantic_category,
    item = "cat_gat",
    age = c(10, 12)
  )
  expect_true(all(c("lp", "semantic_category") %in% colnames(norms)))
})
