
test_that("vocabulary proportions are plausible", {
    participants <- readRDS(test_path("fixtures", "participants.rds"))
    responses <- readRDS(test_path("fixtures", "responses.rds"))
    vocabulary <- bvq_vocabulary(participants, responses)
    
    n_total <- studies %>%
        distinct(version, language, .keep_all = TRUE) %>%
        summarise(n_total = sum(n), .by = version)
    
    vocabulary <- vocabulary %>%
        left_join(select(participants, id, time, version, randomisation),
                  by = join_by(id, time),
                  multiple = "all") %>%
        drop_na(version, randomisation) %>%
        mutate(version = case_when(
            grepl("cbc", id) ~ "CBC",
            grepl("devlex", id) ~ "DevLex",
            .default = paste(version, randomisation, sep = "-"))) %>%
        left_join(n_total,
                  by = join_by(version),
                  multiple = "all")
    
    expect_true(all(between(vocabulary$vocab_prop_total, 0, 1)))
    expect_true(all(between(vocabulary$vocab_prop_dominance_l1[!is.na(vocabulary$vocab_prop_dominance_l1)], 0, 1)))
    expect_true(all(between(vocabulary$vocab_prop_dominance_l2[!is.na(vocabulary$vocab_prop_dominance_l2)], 0, 1)))
    expect_true(all(between(vocabulary$vocab_prop_conceptual, 0, 1)))
    expect_true(all(between(vocabulary$vocab_prop_te, 0, 1)))
    expect_false(any(vocabulary$vocab_count_total[!is.na(vocabulary$vocab_count_total)] < 0))
    expect_false(any(vocabulary$vocab_count_dominance_l1[!is.na(vocabulary$vocab_count_dominance_l1)] < 0))
    expect_false(any(vocabulary$vocab_count_dominance_l2[!is.na(vocabulary$vocab_count_dominance_l2)] < 0))
    expect_false(any(vocabulary$vocab_count_conceptual[!is.na(vocabulary$vocab_count_conceptual)] < 0))
    expect_false(any(vocabulary$vocab_count_te[!is.na(vocabulary$vocab_count_te)] < 0))
})


test_that("column classes are the right ones", {
    participants <- readRDS(test_path("fixtures", "participants.rds"))
    responses <- readRDS(test_path("fixtures", "responses.rds"))
    vocabulary <- bvq_vocabulary(participants, responses)
    
    expect_true(all(class(vocabulary$id) == "character"))
    expect_true(all(class(vocabulary$time) == "numeric"))
    expect_true(all(class(vocabulary$age) == "numeric"))
    expect_true(all(class(vocabulary$type) == "character"))
    expect_true(all(class(vocabulary$vocab_count_total) == "integer"))
    expect_true(all(class(vocabulary$vocab_count_dominance_l1) == "integer"))
    expect_true(all(class(vocabulary$vocab_count_dominance_l2) == "integer"))
    expect_true(all(class(vocabulary$vocab_count_conceptual) == "integer"))
    expect_true(all(class(vocabulary$vocab_count_te) == "integer"))
    expect_true(all(class(vocabulary$vocab_prop_total) == "numeric"))
    expect_true(all(class(vocabulary$vocab_prop_dominance_l1) == "numeric"))
    expect_true(all(class(vocabulary$vocab_prop_dominance_l2) == "numeric"))
    expect_true(all(class(vocabulary$vocab_prop_conceptual) == "numeric"))
    expect_true(all(class(vocabulary$vocab_prop_te) == "numeric"))
})

