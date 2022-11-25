library(dplyr)
library(readxl)

# uni lemmas
worbank_lemmas <- read.delim(system.file("extdata/lemmas.txt", package = "bvqdev"), sep = "\t") %>% 
    select(te, item, ends_with("lemma")) 

# import pool
pool <- read_xlsx(system.file("extdata/pool.xlsx", package = "bvqdev")) %>% 
    left_join(worbank_lemmas) %>% 
    mutate(language = ifelse(grepl("cat_", item), "Catalan", "Spanish")) %>% 
    select(item, language, te, label, ipa, sampa, 
           n_lemmas, is_multiword, subtlex_lemma, wordbank_lemma, 
           childes_lemma, semantic_category, class, version, include) %>% 
    drop_na(version) %>% 
    mutate(
        across(c(te, n_lemmas), as.integer),
        across(c(is_multiword, include),as.logical),
        version = strsplit(version, split = ",")
    ) 

test_that("pool columns are the right classes", {
    expect_true(is.character(pool$item))
    expect_true(is.character(pool$language))
    expect_true(is.integer(pool$te))
    expect_true(is.character(pool$label))
    expect_true(is.character(pool$ipa))
    expect_true(is.character(pool$sampa))
    expect_true(is.integer(pool$n_lemmas))
    expect_true(is.logical(pool$is_multiword))
    expect_true(is.character(pool$subtlex_lemma))
    expect_true(is.character(pool$wordbank_lemma))
    expect_true(is.character(pool$childes_lemma))
    expect_true(is.character(pool$semantic_category))
    expect_true(is.character(pool$class))
    expect_true(is.list(pool$version))
    expect_true(is.logical(pool$include))
})

test_that("pool columns values are right", {
    expect_true(all(unique(pool$language) %in% c("Catalan", "Spanish")))
    expect_true(all(unique(pool$te) %in% 1:804))
    expect_true(all(unique(pool$n_lemmas) > 0))
    expect_true(all(unique(pool$class) %in% c(
        "Adjective", "Adverb", "Auxiliar", "Connective", "Interjection", "Noun", 
        "Pronoun", "Quantifier", "Verb"
        )))
    expect_true(all(unique(pool$semantic_category) %in% c(
        "Action words", "Adventures", "Adverbs", "Animals", "Auxiliary words", "Body parts", 
        "Clothes", "Colours", "Connectives", "Descriptive words", "Food and drink",
        "Furniture and rooms", "Games and rutines", "Household items", "Interjections",
        "Online", "Outside", "Parts of animals", "Parts of things", "People", "Pronouns",
        "Quantifiers", "Question words", "Time", "Toys", "Vehicles"
    )))    
    expect_true(all(unique(pool$include) %in% c(TRUE, FALSE)))
})


dups_exist <- test_that("items are not duplicated", {
    expect_false(any(duplicated(select(pool, te, item))))
})

