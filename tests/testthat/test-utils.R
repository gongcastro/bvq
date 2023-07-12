test_that("times are handled correctly", {
    diff <- diff_in_time(as.Date("2023-02-01"), 
                         as.Date("2022-02-01"))
    
    expect_identical(round(diff, digits = 2), expected = 11.99)
    expect_type(diff, "double")
    
    diff_days <-
        diff_in_time(as.Date("2023-02-01"),
                     as.Date("2022-02-01"),
                     "days")
    
    expect_identical(round(diff_days, digits = 2), expected = 365)
    expect_type(diff_days, "double")
})

test_that("get_time_stamp works", {
    x <- data.frame(start = as.Date(c("2023-02-01", "2023-02-20")),
                    finish = as.Date(c("2023-03-15", "2023-02-21")))
    
    y <- mutate(
        x,
        time_stamp_default = get_time_stamp(start, finish),
        time_stamp_first = get_time_stamp(start, finish, which = "first"),
        time_stamp_last = get_time_stamp(start, finish, which = "last")
    )
    expect_error(mutate(x,
                        time_stamp_default = get_time_stamp(start,
                                                            finish,
                                                            which = "XXXX")))
    expect_type(y$time_stamp_default, "double")
    expect_type(y$time_stamp_first, "double")
    expect_type(y$time_stamp_last, "double")
    
    expect_equal(y$time_stamp_default, as_datetime(x$start))
    expect_equal(y$time_stamp_first, as_datetime(x$start))
    expect_equal(y$time_stamp_last, as_datetime(x$finish))
})

test_that("get_doe works", {
    x <- data.frame(
        doe_cat_1 = seq(0, 1, 0.1),
        doe_cat_2 = c(0, rep(c(0.1, 0), each = 5)),
        doe_spa_1 = c(0, rep(c(0.1, 0), each = 5)),
        doe_spa_2 = c(1, 0.7, 0.6, 0.5, 0.3, 0.1, 0.4, 0.3, 0.2, 0.1, 0)
    )
    
    y <- mutate(
        x,
        doe_other = 1 - get_doe(matches("cat|spa")),
        doe_cat = get_doe(doe_cat_1, doe_cat_2),
        doe_spa = get_doe(matches("spa"))
    )
    
    expect_true(all(vapply(y, class, "character") == "numeric"))
    expect_true(all(vapply(y, \(x) (x >= 0) &
                               (x <= 1), logical(nrow(
                                   y
                               )))))
    expect_equal(y$doe_other, 1 - rowSums(x))
    expect_equal(y$doe_cat, rowSums(x[, c("doe_cat_1", "doe_cat_2")]))
    expect_equal(y$doe_spa, rowSums(x[, c("doe_spa_1", "doe_spa_2")]))
})

test_that("get_longitudinal works", {
    id <- c(1, 1, 1, 2, 2, 3, 4, 4, 4, 4, 5, 6, 7, 7, 8, 9, 10, 10)
    sums <- rle(sort(id))[["lengths"]]
    time <- unlist(lapply(sums, function(x) seq(1, x)))
    dat <- data.frame(id, time = time)
    long_ids <- dat$id %in% c(1, 2, 4, 7, 10)
    
    expect_identical(get_longitudinal(dat), dat)
    expect_identical(get_longitudinal(dat, "all"), dat)
    expect_identical(get_longitudinal(dat, "only")$id, dat$id[long_ids])
    expect_identical(get_longitudinal(dat, "no")$id, dat$id[!long_ids])
    expect_identical(get_longitudinal(dat, "first")$id, unique(dat$id))
    expect_identical(get_longitudinal(dat, "last")$id, unique(dat$id))
})


test_that("fix_code works", {
    code_vctr <- c(
        "BL0123",
        "bl0123",
        "Bl0123",
        "BLBL0123",
        "bi0123",
        "blo123",
        "B0123",
        "BI0123",
        " BL0123"
    )
    
    expect_equal(unique(fix_code(code_vctr)), "BL0123")
})
