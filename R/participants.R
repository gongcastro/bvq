#' Retrieve and update local and/or remote data from formr
#'
#' This function generates a data frame with the information of all participants
#' that have participated or are candidates to participate in any of the
#' versions of BVQ.
#' 
#' @import dplyr
#' @importFrom tidyr drop_na
#' @importFrom lubridate as_date
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
#' * id: a character string indicating a participant's identifier. This value is always the same for each participant, so that different responses from the same participant share the same `id`.
#' * id_exp: a character string indicating a participant's identifier in the context of the particular study in which the participant was tested and invited to fill in the questionnaire. This value is always the same for each participant within the same study, so that different responses from the same participant in the same study share `id_exp`. The same participant may have different `id_exp` across different studies.
#' * id_db: a character string with five digits indicating a participant's identifier in the database from the [Laboratori de Recerca en Infància (https://www.upf.edu/web/cbclab) at Universitat Pompeu Fabra. This value is always the same for each participant, so that different responses from the same participant share the same `id_db`.
#' * code: a character string identifying a single response to the questionnaire. This value is always unique for each response to the questionnaire, even for responses from the same participant.
#' * time: a numeric value indicating how many times a given participant has been sent the questionnaire, regardless of whether they completed it or not.
#' * date_birth: a date value (see lubridate package) in `yyyy/mm/dd` format indicating participants birth date.
#' * age_now: a numeric value indicating the number of months elapsed since participants' birth date until the present day, as indicated by [lubridate::now()].
#' * study: a character string indicating the study in which the participant was invited to fill in the questionnaire. Frequently, participants that filled in the questionnaire came to the lab to participant in a study, and were then invited to fill in the questionnaire later. This value indicates what study each participant was tested in before being sent the questionnaire.
#' * version: a character string indicating what version of the questionnaire a given participant filled in. Different versions may contain a different subset of items, and the administration instructions might vary slightly (see formr questionnaire templates in the [GitHub repository](https://github.com/gongcastro/multilex). Also, different versions were designed, implemented, and administrated at different time points (e.g., before/during/after the COVID-related lockdown).
#' * randomisation: a character string indicating the specific list of
#'   items a participant was assigned to. Only applies in the case of short
#'   versions of BVQ, such as BL-Short, BL-Short-2 or BL-Lockdown, where the
#'   list of items was partitioned into several versions.
#' * date_test: a date value (see lubridate package) in `yyyy/mm/dd`
#'   format indicating the date in which the participant was tested in the
#'   associated study, if any.
#' * date_sent: a date value (see lubridate
#'   package) in `yyyy/mm/dd` format indicating the date in which the
#'   participant was sent the questionnaire.
#' * call: a character string indicating the status of the participant's response: `successful`
#'   (participant completed the questionnaire), `sent` (participant has been
#'   sent the email but has not completed it yet), `pending` (participant is
#'   still to be sent the questionnaire, `reminded` (a week has elapsed since
#'   the participant was sent the questionnaire, and has been already reminded
#'   of it), or `stop` (participant has not completed the questionnaire after
#'   two weeks since they were sent the questionnaire).
#' * comments: a character string indicating useful information for database management.
#'
#' @author Gonzalo Garcia-Castro
#' @md
bvq_participants <- function(...) {
    suppressMessages({
        
        bvq_connect() # get credentials to Google and formr
        
        ss <- "164DMKLRO0Xju0gdfkCS3evAq9ihTgEgFiuJopmqt7mo"
        participants <- read_sheet(ss, sheet = "Participants") %>% 
            drop_na(code) %>%
            mutate(across(c(date_birth, date_test, date_sent), as_date)) %>% 
            select(-link) %>%
            arrange(desc(as.numeric(gsub("BL", "", code))))
    })
    # make sure no columns are lists (probably due to inconsistent cell types)
    if (any(map_lgl(participants, is.list))) {
        col <- names(which(map_lgl(participants, is.list)))
        cli_abort("At last one column is a list. Check that all values are of the same format.")
    }
    return(participants)
}
