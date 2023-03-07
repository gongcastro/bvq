test_that("times are handled correctly", {
    diff <- diff_in_months(as.Date("2023-02-01"), as.Date("2022-02-01"))
    
    expect_identical(round(diff, digits = 2), expected = 11.99)
    expect_error(diff_in_months(as.Date("2022-02-01"), as.Date("2023-02-01")))
    expect_type(diff, "double")
})

test_that("adjusted proportions are computed correctly", {
    prop <- prop_adj(4, 60)  
    prop_se <- prop_adj_se(4, 60)  
    prop_ci <- prop_adj_ci(4, 60)  
    prop_0 <- prop_adj(0, 60) 
    prop_1 <- prop_adj(60, 60) 
    
    expect_identical(round(prop, digits = 5), expected = 0.09375)
    expect_identical(round(prop_se, digits = 5), expected = 0.03644)
    expect_type(prop_ci, "double")
    expect_vector(prop_ci, size = 2)
    expect_identical(round(prop_ci, 5), c(0.02234, 0.16516))
    expect_gt(prop_0, 0)
    expect_lt(prop_1, 1)

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

