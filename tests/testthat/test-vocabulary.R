
test_that("vocabulary proportions are plausible", {
    participants <- readRDS(test_path("fixtures", "participants.rds"))
    responses <- readRDS(test_path("fixtures", "responses.rds"))
    vocabulary <- bvq_vocabulary(participants,
                                 responses, 
                                 .scale = c("prop", "count"))
    
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
    
    expect_true(all(between(vocabulary$total_prop, 0, 1)))
    expect_true(all(between(vocabulary$l1_prop[!is.na(vocabulary$l1_prop)], 0, 1)))
    expect_true(all(between(vocabulary$l2_prop[!is.na(vocabulary$l2_prop)], 0, 1)))
    expect_true(all(between(vocabulary$concept_prop, 0, 1)))
    expect_true(all(between(vocabulary$te_prop, 0, 1)))
    expect_false(any(vocabulary$total_count[!is.na(vocabulary$total_count)] < 0))
    expect_false(any(vocabulary$l1_count[!is.na(vocabulary$l1_count)] < 0))
    expect_false(any(vocabulary$l2_count[!is.na(vocabulary$l2_count)] < 0))
    expect_false(any(vocabulary$concept_count[!is.na(vocabulary$concept_count)] < 0))
    expect_false(any(vocabulary$te_count[!is.na(vocabulary$te_count)] < 0))
})


test_that("column classes are the right ones", {
    participants <- readRDS(test_path("fixtures", "participants.rds"))
    responses <- readRDS(test_path("fixtures", "responses.rds"))
    vocabulary <- bvq_vocabulary(participants,
                                 responses, 
                                 .scale = c("prop", "count"))
    
    expect_true(all(class(vocabulary$id) == "character"))
    expect_true(all(class(vocabulary$time) == "numeric"))
    expect_true(all(class(vocabulary$type) == "character"))
    expect_true(all(class(vocabulary$total_count) == "integer"))
    expect_true(all(class(vocabulary$l1_count) == "integer"))
    expect_true(all(class(vocabulary$l2_count) == "integer"))
    expect_true(all(class(vocabulary$concept_count) == "integer"))
    expect_true(all(class(vocabulary$te_count) == "integer"))
    expect_true(all(class(vocabulary$total_prop) == "numeric"))
    expect_true(all(class(vocabulary$l1_prop) == "numeric"))
    expect_true(all(class(vocabulary$l2_prop) == "numeric"))
    expect_true(all(class(vocabulary$concept_prop) == "numeric"))
    expect_true(all(class(vocabulary$te_prop) == "numeric"))
})

