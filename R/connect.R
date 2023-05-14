#' Authenticate in Google and formr
#'
#' This function tries to log in to the formr API with the user-provided
#' password (argument `password`) or retrieving it from the global environment
#' (`FORMR_PWD` in .Renviron)
#'
#' @export bvq_connect
#'
#' @importFrom googlesheets4 gs4_has_token
#' @importFrom googlesheets4 gs4_auth
#' @importFrom formr formr_connect
#' @importFrom cli cli_alert_success
#' @importFrom cli cli_abort
#'
#' @param google_email E-mail used in Google Drive account. If `NULL` (default),
#'   it is assumed to be the same as `formr_email`.
#' @param password Character string with the password to formr (`NULL` by
#'   default).
#'   
#' @return Logical. `TRUE` if Google and formr authentication was successful,
#'   `FALSE` if authentication of any of the two failed.
#' 
#' @examples
#' \dontrun{bvq_connect()}
#' 
bvq_connect <- function(google_email = NULL,
                        password = NULL) {
  formr_email <- "gonzalo.garciadecastro@upf.edu"

  # ask for email in console is everything is NULL
  if (is.null(google_email)) google_email <- formr_email

  if (is.null(password)) {
    password <- Sys.getenv("FORMR_PWD", unset = NA)
    if (is.na(password)) {
      cli_abort("Please, provide a password")
    }
  }

  # if key exists, use it to log in
  tryCatch(
    suppressWarnings(
      formr_connect(
        email = formr_email,
        password = password,
        host = "https://formr.org/"
      )
    ),
    error = function(e) {
      cli_abort(
        strwrap(
          prefix = " ",
          initial = "",
          "Could not connect to {.url https://formr.org/}. \
          Please check your internet connection or \
          make sure you have set the right formr password."
        )
      )
    }
  )

  # check if Google credentials exists, ask for them if not
  if (!gs4_has_token()) {
    tryCatch(
      suppressWarnings(gs4_auth(
        email = google_email,
        token = Sys.getenv("GOOGLE_TOKEN", unset = NA)
      )),
      error = function(e) {
        cli_abort(
          strwrap(
            prefix = " ",
            initial = "",
            "Could not connect to Google.\
            Please check your internet connection or \
            grant the necessary permissions."
          )
        )
      }
    )
  }

  invisible(gs4_has_token())
}
