responses <- readRDS(system.file("fixtures/responses.rds",
                                 package = "bvq"))
participants <- readRDS(system.file("fixtures/participants.rds",
                                    package = "bvq"))
logs <- bvq_logs(participants, responses)

test_that("columns are the right classes", {
    expect_true(is.character(logs$id))
    expect_true(is.character(logs$code))
    expect_true(is.numeric(logs$time))
    expect_true(is.character(logs$study))
    expect_true(is.character(logs$version))
    expect_true(lubridate::is.Date(logs$date_birth))
    expect_true(lubridate::is.Date(logs$date_started))
    expect_true(lubridate::is.Date(logs$date_finished))
    expect_true(is.numeric(logs$age))
    expect_true(is.numeric(logs$duration))
    expect_true(is.character(logs$edu_parent1))
    expect_true(is.character(logs$edu_parent2))
    expect_true(is.character(logs$dominance))
    expect_true(is.character(logs$lp))
    expect_true(is.numeric(logs$doe_spanish))
    expect_true(is.numeric(logs$doe_catalan))
    expect_true(is.numeric(logs$doe_others))
    expect_true(is.logical(logs$completed))
})

test_that("variables contains possible values", {
    edu_levels <- c(
        "complementary",
        "noeducation",
        "primary",
        "secondary",
        "university",
        "vocational",
        NA
    )
    
    # expect_true(all(logs$id %in% participants$id))
    # expect_true(all(logs$id_exp %in% participants$id_exp))
    # expect_true(all(logs$id_db %in% participants$id_db))
    expect_in(logs$code, participants$code)
    expect_true(all(logs$time > 0))
    expect_in(logs$study, participants$study)
    expect_in(logs$version, responses$version)
    expect_true(all(logs$duration[!is.na(logs$duration)] >= 0))
    expect_true(all(logs$date_birth <= today()))
    expect_true(all(logs$date_finished <= today()))
    expect_true(all(logs$age >= 0))
    expect_in(logs$edu_parent1, edu_levels)
    expect_in(logs$edu_parent2, edu_levels)
    expect_in(logs$lp, c("Monolingual", "Bilingual", "Other"))
    expect_true(all(between(logs$doe_catalan, 0, 1)))
    expect_true(all(between(logs$doe_spanish, 0, 1)))
    expect_true(all(between(logs$doe_others, 0, 1)))
})



test_that("missing values are only where expected", {
    expect_false(any(is.na(logs$id)))
    expect_false(any(is.na(logs$time)))
    expect_false(any(is.na(logs$study)))
    expect_false(any(is.na(logs$version)))
    expect_false(any(is.na(logs$date_birth)))
    expect_false(any(is.na(logs$date_finished)))
    expect_false(any(is.na(logs$age)))
    expect_false(any(is.na(logs$lp)))
})

# check bilingual_threshold and other_threshold --------------------------------

get_does <- function(logs) {
    doe_cols <- names(logs)[grepl("^doe_", names(logs))]
    doe_1 <- apply(cbind(logs[, doe_cols]), 1, max)
    logs_doe <- cbind(logs[, c("lp", doe_cols)], doe_1)
    return(logs_doe)
}

test_that("argument bilingual_threshold behaves correctly when default", {

    logs <- bvq_logs(participants, responses)
    does <- get_does(logs)
    expect_gte(min(does$doe_1[does$lp=="Monolingual"]), 0.80)
    expect_lt(min(does$doe_1[does$lp=="Bilingual"]), 0.80)

})

test_that("argument bilingual_threshold behaves correctly when 0.60", {
    
    logs <- bvq_logs(participants, responses, bilingual_threshold = 0.60)
    does <- get_does(logs)
    
    expect_gte(min(does$doe_1[does$lp=="Monolingual"]), 0.60)
    expect_lt(min(does$doe_1[does$lp=="Bilingual"]), 0.60)
        expect_lt(min(does$doe_1[does$lp=="Bilingual"]), 0.80)

})

test_that("argument other_threshold behaves correctly when default", {
    
    logs <- bvq_logs(participants, responses)
    does <- get_does(logs)
    
    expect_lte(max(does$doe_others[does$lp!="Other"]), 0.10)
    expect_gt(max(does$doe_others[does$lp=="Other"]), 0.10)
    
})

test_that("argument other_threshold behaves correctly when 0.05", {
    
    logs <- bvq_logs(participants, responses, other_threshold = 0.05)
    does <- get_does(logs)
    
    expect_lte(max(does$doe_others[does$lp!="Other"]), 0.05)
    expect_gt(max(does$doe_others[does$lp=="Other"]), 0.05)
    
})

# check args -------------------------------------------------------------------

test_that("return error for incorrect bilingual_threshold", {
    
    expect_error(bvq_logs(participants, responses, bilingual_threshold = "a"))
    expect_error(bvq_logs(participants, responses, bilingual_threshold = -1))
    expect_error(bvq_logs(participants, responses, bilingual_threshold = 2))
    
})

test_that("return error for incorrect other_threshold", {
    
    expect_error(bvq_logs(participants, responses, other_threshold = "a"))
    expect_error(bvq_logs(participants, responses, other_threshold = -1))
    expect_error(bvq_logs(participants, responses, other_threshold = 2))
    
})

