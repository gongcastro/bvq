pool <- readRDS(system.file("fixtures/pool.rds",
                            package = "bvq"))

test_that("pool columns are the right classes", {
    expect_vector(pool$item, character())
    expect_vector(pool$language, character())
    expect_vector(pool$te, integer())
    expect_vector(pool$label, character())
    expect_vector(pool$xsampa, character())
    expect_vector(pool$n_lemmas, integer())
    expect_vector(pool$is_multiword, logical())
    expect_vector(pool$subtlex_lemma, character())
    expect_vector(pool$wordbank_lemma, character())
    expect_vector(pool$childes_lemma, character())
    expect_vector(pool$semantic_category, character())
    expect_vector(pool$class, character())
    expect_vector(pool$version, list())
    expect_vector(pool$include, logical())
})

test_that("pool column values are right", {
    unique_classes <-
        c(
            "Adjective",
            "Adverb",
            "Auxiliar",
            "Connective",
            "Interjection",
            "Noun",
            "Pronoun",
            "Quantifier",
            "Verb"
        )
    unique_categories <-
        c(
            "Action words",
            "Adventures",
            "Adverbs",
            "Animals",
            "Auxiliary words",
            "Body parts",
            "Clothes",
            "Colours",
            "Connectives",
            "Descriptive words",
            "Food and drink",
            "Furniture and rooms",
            "Games and rutines",
            "Household items",
            "Interjections",
            "Online",
            "Outside",
            "Parts of animals",
            "Parts of things",
            "People",
            "Pronouns",
            "Quantifiers",
            "Question words",
            "Time",
            "Toys",
            "Vehicles"
        )
    
    expect_in(unique(pool$language), c("Catalan", "Spanish"))
    expect_in(unique(pool$te), 1:804)
    expect_true(all(unique(pool$n_lemmas) > 0))
    expect_in(unique(pool$class), unique_classes)
    expect_in(unique(pool$semantic_category), unique_categories)
    expect_in(unique(pool$include), c(TRUE, FALSE))
})


test_that("items are not duplicated", {
    expect_false(any(duplicated(select(pool, te, item))))
})
