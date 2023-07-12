make_fixtures <- function() {
    participants <- bvq_participants()
    responses <- bvq_responses(participants = participants)
    logs <- bvq_logs(participants, responses)
    vocabulary <- bvq_vocabulary(participants, responses)
    norms <- bvq_norms(participants, responses)
    
    obj_lst <- lst(participants, responses, logs, vocabulary, norms, pool)
    
    invisible({
        mapply(function(x, y = names(x)) {
                 paths <- file.path(system.file(package = "bvq"), 
                                    "fixtures",
                                    paste0(y, ".rds"))
                 saveRDS(x, paths)
                 saveRDS(x, paths)
             }, obj_lst, names(obj_lst))
    })
} 
