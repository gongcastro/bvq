if(interactive()) {
    
    suppressMessages(require(usethis))
    suppressMessages(require(devtools))
    
    cat("\014") # clear screen
    cli::cli_text("")
    cli::cli_alert_info(R.version$version.string)
    cli::cli_text("")
    
    # customise the prompt
    prompt::set_prompt(function(...){
        branch <- (purrr::safely(gert::git_branch))()
        if(is.null(branch$result)) return("> ")
        return(paste0("[", branch$result, "] > "))
    })
    
    # usethis options
    options(usethis.protocol = "ssh")
    options(usethis.full_name = "gongcastro")
    
    # bias against scientific notation
    options(scipen = 4)
    
    # hey gurl
    
    cli::cli_alert_success(
        paste0("Loaded: ", 
               paste0(
                   session_info()$packages[session_info()$packages$attached, 1], 
                   collapse = ", "
               )
        )
    )      
    cli::cli_text("")
    
}
