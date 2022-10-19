#' Authenticate in Google and formr
#' @export bvq_connect
#' @importFrom googlesheets4 gs4_has_token
#' @importFrom googlesheets4 gs4_auth
#' @importFrom formr formr_connect
#' @importFrom keyring key_list
#' @importFrom keyring key_get
#' @importFrom keyring key_set
#' @importFrom keyring key_set_with_value
#' @param google_email E-mail used in Google Drive account. If NULL (default), it is assumed to be the same as formr_email.
#' @details This function tries to log in to the formr API by trying to retrieve its corresponding key via the keyring package. If no key exists under the name "multilex", the user is prompted to create it first.
#' @return Logical. TRUE if Google and formr authentication was successful, FALSE if authentication of any of the two failed.
#' @examples
#' my_email <- "gonzalo.garciadecastro@upf.edu"
#' bvq_connect(google_email = my_email)
bvq_connect <- function(google_email = NULL,
                        verbose = TRUE) {
    formr_email <- "gonzalo.garciadecastro@upf.edu"
    
    # ask for email in console is everything is NULL
    spinny <- cli::make_spinner(template = "{spin} Connecting to formR")
    working <- TRUE
    while(working){
        if (verbose) spinny$spin()
        if (is.null(google_email)) google_email <- formr_email
        
        # if key does not exist and is not provided, create it
        is_key_formr_missing <- !("bvq" %in% key_list()$service)
        if (is_key_formr_missing) key_set("bvq", formr_email)
        
        # if key exists, use it to log in
        suppressWarnings(
            formr_connect(
                email = formr_email,
                password = key_get("bvq", formr_email),
                host = "https://formr.org/"
            )
        )
        
        working <- FALSE
    }
    
    # check if Google credentials exists, ask for them if not
    is_key_google <- gs4_has_token()
    if (!is_key_google) gs4_auth(email = google_email)
    
    # return success code but do not print it
    if (verbose && gs4_has_token()){
        cli::cli_text("")
        cli::cli_alert_success("Connected to BVQ")
    }
    
    invisible(gs4_has_token())
}
