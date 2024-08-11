items <- readRDS(system.file("fixtures/items.rds", package = "bvq"))

survey_name <- "bvq_06_words_catalan"
version <- "bvq-1.0.0"

test_that("bvq_items returns a list with the right names and types", {
  expect_type(bvq_items(survey_name, version), "list")
  expect_length(bvq_items(survey_name, version), 3)
  expect_named(bvq_items(survey_name, version),
    expected = c("survey", "choices", "settings")
  )
  expect_type(bvq_items(survey_name, version)[["survey"]], "list")
  expect_type(bvq_items(survey_name, version)[["choices"]], "list")
  expect_type(bvq_items(survey_name, version)[["settings"]], "list")
})
