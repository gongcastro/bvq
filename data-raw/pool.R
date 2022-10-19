## code to prepare `inst/extdata/pool.xlsx` dataset goes here
pool <- system.file("inst/extdata/pool.xlsx", package = "bvqdev") %>% 
    readxl::read_xlsx() 

usethis::use_data(pool, overwrite = TRUE)
