test_that("pool columns are the right classes", {
    pool <- readRDS(test_path("fixtures", "pool.rds"))
    
    expect_true(is.character(pool$item))
    expect_true(is.character(pool$language))
    expect_true(is.integer(pool$te))
    expect_true(is.character(pool$label))
    expect_true(is.character(pool$xsampa))
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

test_that("pool column values are right", {
    pool <- readRDS(test_path("fixtures", "pool.rds"))
    
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


test_that("items are not duplicated", {
    pool <- readRDS(test_path("fixtures", "pool.rds"))
    
    expect_false(any(duplicated(select(pool, te, item))))
})

