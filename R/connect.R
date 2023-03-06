#' Authenticate in Google and formr
#' 
#' @export bvq_connect
#' @importFrom googlesheets4 gs4_has_token
#' @importFrom googlesheets4 gs4_auth
#' @importFrom formr formr_connect
#' @importFrom cli cli_alert_success
#' @importFrom cli cli_abort
#' @importFrom cli cli_text
#' @param google_email E-mail used in Google Drive account. If NULL (default), it is assumed to be the same as formr_email.
#' @param verbose Should progress messages and warnings be printed in the console
#' @details This function tries to log in to the formr API with the user-provided password (argument \code{password}) or retrieving it from the global environment (FORMR_PWD in .Renviron)
#' @return Logical. TRUE if Google and formr authentication was successful, FALSE if authentication of any of the two failed.
bvq_connect <- function(google_email = NULL,
                        verbose = TRUE,
                        password = Sys.getenv("FORMR_PWD", unset = NA)) {
    
    formr_email <- "gonzalo.garciadecastro@upf.edu"
    
    # ask for email in console is everything is NULL
    if (is.null(google_email)) google_email <- formr_email
    
    # if key does not exist and is not provided, create it
    if (is.na(password)) {
        cli_abort("{.field password} is missing. Add your password to {.path .Renviron} in your base path as {.code FORMR_PWD} or pass it along the {.code password} argument")
    }
    
    if (!is.na(password) & !is.na(Sys.getenv("FORMR_PWD", unset = NA))) {
        cli_alert_info("Using global variable FORMR_PWD as password")
    }
    
    # if key exists, use it to log in
    tryCatch(
        suppressWarnings(
            formr_connect(
                email = formr_email,
                password = pwd,
                host = "https://formr.org/"
            )
        ), 
        error = function(e){
            cli_abort(
                strwrap(
                    prefix = " ",
                    initial = "",
                    "Could not connect to formR. Please check your internet connection or make sure you have set the right formR password."
                )
            )
        }
    )
    
    # check if Google credentials exists, ask for them if not
    is_key_google <- gs4_has_token()
    
    if (!is_key_google) {
        
        tryCatch(
            suppressWarnings(gs4_auth(email = google_email)), 
            error = function(e){
                cli_abort(
                    strwrap(
                        prefix = " ",
                        initial = "",
                        "Could not connect to Google. Please check your internet connection or grant the necessary permissions."
                    )
                )
            }
        )
    }
    
    # return success code but do not print it
    if (interactive() && verbose && gs4_has_token()){
        cli_alert_success("Connected to BVQ")
    }
    
    invisible(gs4_has_token())
}
