vowels <- readRDS(system.file("fixtures/vowels.rds", 
                              package = "bvq"))

test_that("vowels columns are the right classes", {
  expect_type(vowels$xsampa, "character")
  expect_type(vowels$openness, "list")
  expect_type(vowels$frontness, "list")
})

test_that("vowels column values are right", {
  vowels_vct <- c(
    "i", "y", "1", "}", "M", "u", "I", "Y", "I\\", "U\\",
    "U", "e", "2", "\\@", "8", "7", "o", "e_o", "2_o", "@",
    "o_o", "E", "9", "3", "3\\", "V", "O", "\\{", "6", "a",
    "&", "a_\"", "A", "Q"
  )

  openness_vct <- c(
    "Close", "Near close", "Close-mid", "Mid", "Open-mid",
    "Near-open", "Open"
  )

  frontness_vct <- c("Front", "Central", "Back")

  expect_true(all(unique(unlist(vowels$xsampa)) %in% vowels_vct))
  expect_true(all(unique(unlist(vowels$openness)) %in% openness_vct))
  expect_true(all(unique(unlist(vowels$frontness)) %in% frontness_vct))
})

test_that("vowels are not duplicated", {
  expect_false(any(duplicated(vowels$xsampa)))
})
