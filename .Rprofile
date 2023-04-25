
if(interactive()) {
    
    suppressWarnings(require(usethis, quietly = TRUE))
    suppressWarnings(require(devtools, quietly = TRUE))
    
    cat("\014") # clear screen
    cli::cli_text("")
    cli::cli_alert_info(R.version$version.string)
    cli::cli_text("")
    
    # customise the prompt
    prompt::set_prompt(function(...){
        branch <- suppressWarnings(purrr::safely(gert::git_branch)())
        if(is.null(branch$result)) return("> ")
        return(paste0("[", branch$result, "] > "))
    })
    
    
    # usethis options
    options(usethis.protocol = "ssh")
    options(usethis.full_name = "gongcastro")
    
    # bias against scientific notation
    options(scipen = 4)
    
    # print loaded packages
    cli::cli_alert_success(
        paste0("Loaded: ", 
               paste0(
                   sessioninfo::session_info()$packages[sessioninfo::session_info()$packages$attached, 1], 
                   collapse = ", "
               )
        )
    )      
    cli::cli_text("")
    
    load_all()
    
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
                 })
        })
    } 
    
}
