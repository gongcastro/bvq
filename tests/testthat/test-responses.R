responses <- readRDS(system.file("fixtures/responses.rds", 
                                 package = "bvq"))

test_that("columns are the right classes", {
  expect_true(is.character(responses$id))
  expect_true(is.numeric(responses$time))
  expect_true(is.character(responses$code))
  expect_true(is.character(responses$study))
  expect_true(is.character(responses$version))
  expect_true(is.character(responses$item))
  expect_true(is.numeric(responses$response))
  expect_true(lubridate::is.Date(responses$date_birth))
  expect_true(lubridate::is.Date(responses$date_started))
  expect_true(lubridate::is.Date(responses$date_finished))
  expect_true(is.character(responses$edu_parent1))
  expect_true(is.character(responses$edu_parent2))
  expect_true(is.numeric(responses$doe_catalan))
  expect_true(is.numeric(responses$doe_spanish))
  expect_true(is.numeric(responses$doe_others))
})

test_that("responses have the right values", {
  expect_true(all(responses$response %in% c(2, 1, 3, NA)))
})

test_that("all participants have at least one non-missing response", {
  non_missing_responses <- responses %>%
    group_by(id, time) %>%
    summarise(not_missing = sum(!is.na(response)), .groups = "drop") %>%
    pull(not_missing)
  expect_false(all(non_missing_responses))
})
