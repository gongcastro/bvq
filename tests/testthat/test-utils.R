test_that("times are handled correctly", {
    diff <- diff_in_time(as.Date("2023-02-01"), as.Date("2022-02-01"))
    
    expect_identical(round(diff, digits = 2), expected = 11.99)
    expect_type(diff, "double")
    
    diff_days <- diff_in_time(as.Date("2023-02-01"), as.Date("2022-02-01"), "days")
    
    expect_identical(round(diff_days, digits = 2), expected = 365)
    expect_type(diff_days, "double")
})

test_that("get_longitudinal works correctly", {
    id <- c(1, 1, 1, 2, 2, 3, 4, 4, 4, 4, 5, 6, 7, 7, 8, 9, 10, 10)
    sums <- rle(sort(id))[["lengths"]]
    dat <- data.frame(id, time = unlist(sapply(sums, function(x) seq(1, x))))
    
    expect_identical(get_longitudinal(dat), dat)
    expect_identical(get_longitudinal(dat, "all"), dat)
    expect_identical(get_longitudinal(dat, "only")$id, dat$id[dat$id %in% c(1, 2, 4, 7, 10)])
    expect_identical(get_longitudinal(dat, "no")$id, dat$id[!(dat$id %in% c(1, 2, 4, 7, 10))])
    expect_identical(get_longitudinal(dat, "first")$id, unique(dat$id))
    expect_identical(get_longitudinal(dat, "last")$id, unique(dat$id))
    
})
