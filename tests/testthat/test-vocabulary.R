Sys.getenv(FORMR_PWD)

bvq_connect(verbose = FALSE)
participants <- bvq_participants()
responses <- bvq_responses(participants, verbose = FALSE)
logs <- bvq_logs(participants, responses, verbose = FALSE)

n_total <- studies %>%
    distinct(version, language, .keep_all = TRUE) %>%
    group_by(version) %>%
    summarise(n_total = sum(n))

vocabulary <- bvq_vocabulary(participants, responses, 
                             scale = c("count", "prop")) %>%
    left_join(select(participants, id, time, version, randomisation),
              multiple = "all") %>%
    drop_na(version, randomisation) %>%
    mutate(
        version = case_when(
            grepl("cbc", id) ~ "CBC",
            grepl("devlex", id) ~ "DevLex",
            .default = paste(version, randomisation, sep = "-")
        )
    ) %>%
    left_join(n_total,
              multiple = "all")

test_that("vocabulary proportions are plausible", {
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

