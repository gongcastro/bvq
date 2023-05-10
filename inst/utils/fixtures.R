make_fixtures <- function() {
    participants <- bvq_participants()
    responses <- bvq_responses(participants = participants)
    logs <- bvq_logs(participants, responses)
    vocabulary <- bvq_vocabulary(participants, responses)
    norms <- bvq_norms(participants, responses)
    
    obj_lst <- lst(participants, responses, logs, vocabulary, norms, pool)
    
    invisible({
        map2(obj_lst, names(obj_lst),
             function(x, y = names(x)) {
                 paths <- paste0(testthat::test_path("fixtures", y), ".rds")
                 saveRDS(x, paths)
                 paths <- paste0("vignettes/fixtures/", y, ".rds")
                 saveRDS(x, paths)
             })
    })
} 
