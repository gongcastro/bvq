test_that("bvq_participants columns are the right classes", {
    participants <- readRDS(test_path("fixtures", "participants.rds"))
    
    expect_true(is.character(participants$id))
    expect_true(is.character(participants$id_exp))
    expect_true(is.character(participants$id_db))
    expect_true(is.character(participants$code))
    expect_true(is.numeric(participants$time))
    expect_true(lubridate::is.Date(participants$date_birth))
    expect_true(is.numeric(participants$age_now))
    expect_true(is.character(participants$study))
    expect_true(is.character(participants$version))
    expect_true(is.character(participants$randomisation))
    expect_true(lubridate::is.Date(participants$date_test))
    expect_true(lubridate::is.Date(participants$date_sent))
    expect_true(is.character(participants$call))
    expect_true(is.character(participants$comments))
})

test_that("participants and times are not duplicated", {
    participants <- readRDS(test_path("fixtures", "participants.rds"))
    
    expect_false(any(duplicated(select(participants, id, time))))
})

