participants <- readRDS(system.file("fixtures/participants.rds",
  package = "bvq"
))

test_that("bvq_participants columns are the right classes", {
  expect_true(is.character(participants$child_id))
  expect_true(is.character(participants$response_id))
  expect_true(is.numeric(participants$time))
  expect_true(lubridate::is.Date(participants$date_birth))
  expect_true(is.numeric(participants$age_now))
  expect_true(is.character(participants$version))
  expect_true(is.character(participants$version_list))
  expect_true(lubridate::is.Date(participants$date_test))
  expect_true(lubridate::is.Date(participants$date_sent))
  expect_true(is.character(participants$call))
})

test_that("participants and times are not duplicated", {
  expect_false(any(duplicated(select(
    participants, child_id, time
  ))))
})
