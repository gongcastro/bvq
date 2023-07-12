## code to prepare `vowels` dataset goes here
library(dplyr)
library(readxl)

# vowels
openness_levels <- c(
  "Close" = 0,
  "Near close" = 1,
  "Close-mid" = 2,
  "Mid" = 3,
  "Open-mid" = 4,
  "Near-open" = 5,
  "Open" = 6
)

frontness_levels <- c(
  "Back" = 0,
  "Central" = 1,
  "Front" = 2
)

vowels <- read_xlsx(system.file("extdata/vowels.xlsx",
  package = "bvq"
)) %>%
  mutate(
    openness = factor(openness,
      levels = openness_levels,
      labels = names(openness_levels)
    ),
    frontness = factor(frontness,
      levels = frontness_levels,
      labels = names(frontness_levels)
    )
  ) %>%
  rename(xsampa = phoneme) %>%
  mutate(
    across(-xsampa, as.factor),
    across(
      c(openness, frontness),
      function(x) strsplit(as.character(x), ", ")
    )
  ) %>%
  relocate(xsampa, openness, frontness)

# export for future testing
saveRDS(vowels, test_path("fixtures", "vowels.rds"))

# export database
usethis::use_data(vowels, overwrite = TRUE, internal = FALSE)
