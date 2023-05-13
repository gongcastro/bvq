## code to prepare `pool` dataset goes here
library(dplyr)
library(readxl)

# uni lemmas
worbank_lemmas <- read.delim(
  system.file("extdata/lemmas.txt",
    package = "bvq"
  ),
  sep = "\t",
  quote = ""
) %>%
  select(te, item, ends_with("lemma"))

# import pool
pool <- read_xlsx("inst/extdata/pool.xlsx") %>%
  left_join(worbank_lemmas,
    by = join_by(item, te)
  ) %>%
  mutate(
    language = ifelse(grepl("cat_", item),
      "Catalan",
      "Spanish"
    ),
    xsampa = ipa(ipa, "xsampa")
  ) %>%
  select(
    item, language, te, label, xsampa,
    n_lemmas, is_multiword, subtlex_lemma, wordbank_lemma,
    childes_lemma, semantic_category, class, version, include
  ) %>%
  drop_na(version) %>%
  mutate(across(c(te, n_lemmas), as.integer),
    across(c(is_multiword, include), as.logical),
    version = strsplit(version, split = ",")
  )

# export for future testing
saveRDS(pool, test_path("fixtures", "pool.rds"))

# export pool database
usethis::use_data(pool, overwrite = TRUE, internal = FALSE)
