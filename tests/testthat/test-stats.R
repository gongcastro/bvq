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


