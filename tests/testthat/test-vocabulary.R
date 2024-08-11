responses <- readRDS(system.file("fixtures/responses.rds",
  package = "bvq"
))

participants <- readRDS(system.file("fixtures/participants.rds",
  package = "bvq"
))

vocabulary <- bvq_vocabulary(participants,
  responses,
  .scale = c("prop", "count")
)

test_that("bvq_vocabulary throws errors when appropriate", {
  expect_error(bvq_vocabulary(participants, responses, .scale = "XXX"))
  expect_error(bvq_vocabulary(participants, responses, lp, XXXX))
})

test_that("vocabulary scores are plausible", {
  expect_lte(max(vocabulary$total_prop, na.rm = TRUE), 1)
  expect_gte(max(vocabulary$total_prop, na.rm = TRUE), 0)
  expect_gte(max(vocabulary$total_count, na.rm = TRUE), 0)

  expect_lte(max(vocabulary$l1_prop, na.rm = TRUE), 1)
  expect_gte(max(vocabulary$l1_prop, na.rm = TRUE), 0)
  expect_gte(max(vocabulary$l1_count, na.rm = TRUE), 0)

  expect_lte(max(vocabulary$l2_prop, na.rm = TRUE), 1)
  expect_gte(max(vocabulary$l2_prop, na.rm = TRUE), 0)
  expect_gte(max(vocabulary$l2_count, na.rm = TRUE), 0)

  expect_lte(max(vocabulary$concept_prop, na.rm = TRUE), 1)
  expect_gte(max(vocabulary$concept_prop, na.rm = TRUE), 0)
  expect_gte(max(vocabulary$concept_count, na.rm = TRUE), 0)

  expect_lte(max(vocabulary$te_prop, na.rm = TRUE), 1)
  expect_gte(max(vocabulary$te_prop, na.rm = TRUE), 0)
  expect_gte(max(vocabulary$te_count, na.rm = TRUE), 0)
})

test_that("column classes are the right ones", {
  expect_true(all(class(vocabulary$child_id) == "character"))
  expect_true(all(class(vocabulary$response_id) == "character"))
  expect_true(all(class(vocabulary$type) == "character"))

  expect_true(all(class(vocabulary$total_count) == "integer"))
  expect_true(all(class(vocabulary$l1_count) == "integer"))
  expect_true(all(class(vocabulary$l2_count) == "integer"))
  expect_true(all(class(vocabulary$concept_count) == "integer"))
  expect_true(all(class(vocabulary$te_count) == "integer"))

  expect_true(all(class(vocabulary$total_prop) == "numeric"))
  expect_true(all(class(vocabulary$l1_prop) == "numeric"))
  expect_true(all(class(vocabulary$l2_prop) == "numeric"))
  expect_true(all(class(vocabulary$concept_prop) == "numeric"))
  expect_true(all(class(vocabulary$te_prop) == "numeric"))

  expect_true(all(class(vocabulary$contents) == "list"))
})

test_that("the ... argument works", {
  vocabulary <- bvq_vocabulary(
    participants,
    responses,
    lp,
    semantic_category
  )

  expect_in(c("lp", "semantic_category"), colnames(vocabulary))
})
