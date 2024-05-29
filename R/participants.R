#' Retrieve and update local and/or remote data from formr
#'
#' This function generates a data frame with the information of all participants
#' that have participated or are candidates to participate in any of the
#' versions of BVQ.
#'
#' @importFrom cli cli_abort
#' @importFrom googlesheets4 read_sheet
#'
#' @param ... Unused.
#'
#' @export bvq_participants
#'
#' @return A data frame (actually, a [tibble::tibble]) with all participants
#'   that have participated or are candidates to participate in any of the
#'   versions of BVQ Each row corresponds to a questionnaire response and each
#'   column represents a variable. The output includes the following variables:
#' * child_id: a character string with five digits indicating a participant's identifier in the database from the [Laboratori de Recerca en Infància](https://www.upf.edu/web/cbclab) at Universitat Pompeu Fabra. This value is always the same for each participant, so that different responses from the same participant share the same `id`.
#' * response_id: a character string identifying a single response to the questionnaire. This value is always unique for each response to the questionnaire, even for responses from the same participant.
#' * time: a numeric value indicating how many times a given participant has been sent the questionnaire, regardless of whether they completed it or not.
#' * date_birth: a date value in `yyyy/mm/dd` format indicating participants birth date.
#' * age_now: a numeric value indicating the number of months elapsed since participants' birth date until the present day, as indicated by [lubridate::now()].
#' * version: a character string indicating what version of the questionnaire a given participant filled in. Different versions may contain a different subset of items, and the administration instructions might vary slightly (see formr questionnaire templates in the [GitHub repository](https://github.com/gongcastro/multilex). Also, different versions were designed, implemented, and administrated at different time points (e.g., before/during/after the COVID-related lockdown).
#' * version_list: a character string indicating the specific list of
#'   items a participant was assigned to. Only applies in the case of short
#'   versions of BVQ, such as bvq-short, bvq-long, bvq-lockdown, or bvq-1.0.0, where the
#'   list of items was partitioned into several versions.
#' * date_test: a date value (see lubridate package) in `yyyy/mm/dd`
#'   format indicating the date in which the participant was tested in the
#'   associated study, if any.
#' * date_sent: a date value (see lubridate
#'   package) in `yyyy/mm/dd` format indicating the date in which the
#'   participant was sent the questionnaire.
#' * call: a character string indicating the status of the participant's response: 
#'      - `"successful"`: participant completed the questionnaire)
#'      - `"sent"`: participant has been sent the email but has not completed it 
#'      yet)
#'      - `"pending"`: participant is still to be sent the questionnaire.
#'      - `"reminded"`: a week has elapsed since the participant was sent the questionnaire,
#'       and has been already reminded of it.
#'      - `"stop"`: participant has not completed the questionnaire after 
#'       two weeks since they were sent the questionnaire.
#'
#' @author Gonzalo Garcia-Castro
#' 
#' @examples
#' \dontrun{
#' bvq_participants()
#' }
#' 
#' @md
bvq_participants <- function(...) {
    
    bvq_connect() # get credentials to Google and formr
    
    # download Sheets
    suppressMessages({
        sheet <- read_sheet(ss = "164DMKLRO0Xju0gdfkCS3evAq9ihTgEgFiuJopmqt7mo",
                        sheet = "Participants", 
                        col_types = "cccciDnccccDDclcc",
                        .name_repair = janitor::make_clean_names)
    })
    
    participants <- sheet %>% 
        filter(!is.na(code),
               include) %>% 
        select(child_id = id, response_id = code,
               time, date_birth, date_sent, version,
               version_list = randomisation, call) %>% 
        mutate(response_id = gsub("BL", "", response_id),
               version = gsub("bl-", "", tolower(version))) %>% 
        # reorder rows
        arrange(desc(as.numeric(response_id)))
    
    # make sure no columns are lists
    # (probably due to inconsistent cell types)
    is_col_list <- vapply(participants, is.list, logical(1))
    if (any(is_col_list)) { 
        col <- names(which(is_col_list))
        cli::cli_abort("{col} {?has/have} class {.cls list}")
    }
    
    return(participants)
}
