#' Import lockdown data
#' @import dplyr
#' @importFrom formr formr_raw_results
#' @importFrom purrr map
#' @importFrom purrr set_names
#' @importFrom purrr reduce
#' @importFrom lubridate as_datetime
#' @importFrom lubridate time_length
#' @importFrom tidyr drop_na
#' @importFrom tidyr pivot_longer
#' @importFrom janitor clean_names
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @importFrom cli cli_alert_success
#' @param surveys Name of formr surveys from the bilexicon_lockdown run
#' @param verbose Should progress messages and warnings be printed in the console
import_formr_lockdown <- function(surveys = c(
    "bilexicon_lockdown_01_log",
    "bilexicon_lockdown_02_welcome",
    "bilexicon_lockdown_03_consent",
    "bilexicon_lockdown_04_demo",
    "bilexicon_lockdown_05_language",
    "bilexicon_lockdown_06_words_catalan",
    "bilexicon_lockdown_06_words_spanish"),
    verbose = TRUE) {
    
    participants_tmp <- get("participants", parent.frame()) %>%
        select(-.data$version)
    
    # fetch responses
    raw <- download_surveys(surveys, verbose = verbose)
    
    # edit logs dataset
    raw[[1]] <- raw[[1]] %>%
        rename(code = .data$bl_code) %>%
        mutate(
            # fix codes known to be wrong
            code = fix_code(na_if(.data$code, "")),
            created = as_datetime(.data$created)
        ) %>%
        # remove responses with no code
        drop_na(.data$code, .data$ended) %>%
        # fix codes known to be wrong
        fix_code_raw() %>%
        # remove codes not inlcuded in participants
        filter(.data$code %in% participants_tmp$code) %>%
        # get only last response of each code
        arrange(desc(.data$created)) %>%
        distinct(.data$code, .keep_all = TRUE)
    
    items_to_keep <- c(
        "time", "code", "study", "version", "randomisation", "time_stamp", "date_birth",
        "age", "postcode", "sex", "edu_parent1", "edu_parent2",
        "language_doe_catalan", "language_doe_spanish", "language_doe_others",
        "language_doe_catalan_lockdown", "language_doe_spanish_lockdown", "language_doe_others_lockdown"
    )
    
    processed <- raw %>%
        map(drop_na, created) %>%
        map(select, -one_of(c("created", "modified", "ended", "expired"))) %>%
        reduce(inner_join, by = "session") %>%
        left_join(participants_tmp, by = "code") %>%
        left_join(select(raw[[6]], .data$session, created_cat = .data$created, ended_cat = .data$ended), by = "session") %>%
        left_join(select(raw[[7]], .data$session, created_spa = .data$created, ended_spa = .data$ended), by = "session") %>%
        mutate(
            across(c(.data$created_cat, .data$created_spa, .data$ended_cat, .data$ended_spa, .data$date_birth), as_datetime),
            across(starts_with("language_doe"), ~ifelse(is.na(.), 0, .)),
            version = paste0("BL-Lockdown-", .data$version),
            time_stamp = get_time_stamp(., c("ended_cat", "ended_spa"), "last"),
            # calculate age in months
            age = diff_in_months(time_stamp, date_birth),
            language_doe_catalan = get_doe(., languages = .env$languages_lockdown1[grep("catalan", .env$languages_lockdown1)]),
            language_doe_spanish = get_doe(., languages = .env$languages_lockdown1[grep("spanish", .env$languages_lockdown1)]),
            language_doe_catalan_lockdown = get_doe(., languages = .env$languages_lockdown2[grep("catalan", .env$languages_lockdown2)]),
            language_doe_spanish_lockdown = get_doe(., languages = .env$languages_lockdown2[grep("spanish", .env$languages_lockdown2)]),
            language_doe_others = 100 - rowSums(across(c(.data$language_doe_catalan, .data$language_doe_spanish)), na.rm = TRUE),
            language_doe_others_lockdown = 100 - rowSums(across(c(.data$language_doe_catalan_lockdown, .data$language_doe_spanish_lockdown)), na.rm = TRUE)
        ) %>%
        arrange(desc(.data$time_stamp)) %>%
        distinct(.data$session, .keep_all = TRUE) %>%
        rename(
            postcode = .data$demo_postcode,
            edu_parent1 = .data$demo_parent1,
            edu_parent2 = .data$demo_parent2
        ) %>%
        drop_na(.data$age) %>%
        select(starts_with("id"), one_of(items_to_keep), starts_with("cat_"), starts_with("spa_")) %>%
        pivot_longer(
            cols = matches("cat_|spa_"),
            names_to = "item",
            values_to = "response"
        ) %>%
        rename_with(~ gsub("language_", "", .), everything()) %>%
        mutate(
            language = ifelse(grepl("cat_", .data$item), "Catalan", "Spanish"),
            sex = ifelse(.data$sex == 1, "Male", "Female"),
            postcode = as.integer(na_if(.data$postcode, "")),
            edu_parent1 = na_if(.data$edu_parent1, ""),
            edu_parent2 = na_if(.data$edu_parent2, ""),
            sex = NA_character_
        ) %>%
        arrange(desc(.data$time_stamp))
    
    if (verbose){
        cli_alert_success(paste0("BL-Lockdown updated: ", nrow(distinct(processed, code)), " responses retrieved"))
    }
    
    return(processed)
}


#' Import short
#' @import dplyr
#' @importFrom formr formr_raw_results
#' @importFrom purrr map
#' @importFrom purrr set_names
#' @importFrom purrr reduce
#' @importFrom lubridate as_datetime
#' @importFrom tidyr drop_na
#' @importFrom tidyr pivot_longer
#' @importFrom janitor clean_names
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @importFrom cli cli_alert_success
#' @param surveys Name of formr surveys from the bilexicon_short run
#' @param verbose Should progress messages and warnings be printed in the console
import_formr_short <- function(surveys = c(
    "bilexicon_short_01_log",
    "bilexicon_short_02_welcome",
    "bilexicon_short_03_consent",
    "bilexicon_short_04_demo",
    "bilexicon_short_05_language",
    "bilexicon_short_06_words_catalan",
    "bilexicon_short_06_words_spanish"),
    verbose = TRUE) {
    
    participants_tmp <- get("participants", parent.frame()) %>%
        filter(version %in% "BL-Short") %>%
        select(-version)
    
    # fetch responses
    raw <- download_surveys(surveys, verbose = verbose)
    
    # edit Spanish inventory
    raw[[7]] <- rename_all(raw[[7]], ~ gsub("cat_", "spa_", .))
    
    # edit logs dataset
    raw[[1]] <- raw[[1]] %>%
        mutate(
            # fix codes known to be wrong
            code = fix_code(na_if(.data$code, "")),
            created = as_datetime(.data$created)
        ) %>%
        # remove codes not inlcuded in participants
        filter(.data$code %in% participants_tmp$code) %>%
        # get only last response of each code
        arrange(desc(.data$created)) %>%
        distinct(.data$code, .keep_all = TRUE) %>%
        # remove responses with no code
        drop_na(.data$code, .data$ended) %>%
        # fix codes known to be wrong
        fix_code_raw()
    
    items_to_keep <- c(
        "time", "code", "study", "version", "randomisation",
        "time_stamp", "date_birth", "age",
        "sex", "postcode", "edu_parent1", "edu_parent2",
        "language_doe_catalan", "language_doe_spanish", "language_doe_others"
    )
    
    # process data
    processed <- raw %>%
        map(select, -one_of(c("created", "modified", "ended", "expired"))) %>%
        reduce(left_join, by = "session") %>%
        mutate(code = fix_code(.data$code)) %>%
        left_join(participants_tmp, by = "code") %>%
        filter(code %in% participants_tmp$code) %>%
        left_join(select(raw$bilexicon_short_06_words_cat, .data$session, created_cat = .data$created, ended_cat = .data$ended), by = "session") %>%
        left_join(select(raw$bilexicon_short_06_words_spa, .data$session, created_spa = .data$created, ended_spa = .data$ended), by = "session") %>%
        filter(.data$code %in% participants_tmp$code) %>%
        drop_na(.data$created_cat, .data$created_spa) %>%
        mutate(
            across(c(.data$created_cat, .data$created_spa, .data$ended_cat, .data$ended_spa, .data$date_birth), as_datetime),
            across(starts_with("language_doe_"), ~ ifelse(is.na(.), 0, .)),
            version = paste0("BL-Short-", .data$version),
            time_stamp = get_time_stamp(., c("ended_cat", "ended_spa"), "last"),
            age = diff_in_months(time_stamp, date_birth),
            language_doe_catalan = get_doe(., languages = .env$languages_short[grep("catalan", languages_short)]),
            language_doe_spanish = get_doe(., languages = .env$languages_short[grep("spanish", languages_short)]),
            language_doe_others = 100 - rowSums(across(c(.data$language_doe_catalan, .data$language_doe_spanish)), na.rm = TRUE)
        ) %>%
        arrange(desc(.data$time_stamp)) %>%
        distinct(.data$session, .keep_all = TRUE) %>%
        rename(postcode = .data$demo_postcode, edu_parent1 = .data$demo_parent1, edu_parent2 = .data$demo_parent2) %>%
        drop_na(.data$age) %>%
        select(starts_with("id"), one_of(items_to_keep), starts_with("cat_"), starts_with("spa_")) %>%
        pivot_longer(cols = matches("cat_|spa_"), names_to = "item", values_to = "response") %>%
        rename_all(function(x) gsub("language_", "", x)) %>%
        mutate(
            language = ifelse(grepl("cat_", .data$item), "Catalan", "Spanish"),
            sex = ifelse(.data$sex %in% 1, "Male", "Female"),
            postcode = as.integer(ifelse(.data$postcode %in% "", NA_character_, .data$postcode)),
            edu_parent1 = ifelse(.data$edu_parent1 %in% "", NA_character_, .data$edu_parent1),
            edu_parent2 = ifelse(.data$edu_parent2 %in% "", NA_character_, .data$edu_parent2)
        ) %>%
        arrange(desc(.data$time_stamp)) %>%
        distinct(.data$id, .data$code, .data$item, .keep_all = TRUE)
    
    if (verbose) cli_alert_success(paste0("BL-Short updated: ", nrow(distinct(processed, code)), " responses retrieved"))
    
    return(processed)
}

#' Import formr 2
#' @import dplyr
#' @importFrom formr formr_raw_results
#' @importFrom purrr map
#' @importFrom purrr set_names
#' @importFrom purrr reduce
#' @importFrom lubridate as_datetime
#' @importFrom tidyr drop_na
#' @importFrom tidyr pivot_longer
#' @importFrom janitor clean_names
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @importFrom cli cli_alert_success
#' @param surveys Name of formr surveys from the bilexicon_long2 run
#' @param verbose Should progress messages and warnings be printed in the console
import_formr2 <- function(surveys = c(
    "bilexicon_01_log",
    "bilexicon_02_welcome",
    "bilexicon_03_consent",
    "bilexicon_04_demo",
    "bilexicon_05_language",
    "bilexicon_06_words_cat",
    "bilexicon_06_words_spa"),
    verbose = TRUE) {
    
    participants_tmp <- get("participants", parent.frame()) %>%
        filter(
            .data$version %in% "BL-Long",
            .data$randomisation %in% "2"
        ) %>%
        select(-.data$version)
    
    items_to_keep <- c(
        "time", "code", "study", "version", "randomisation",
        "time_stamp", "date_birth", "age", "sex", "postcode",
        "edu_parent1", "edu_parent2",
        "language_doe_catalan", "language_doe_spanish", "language_doe_others"
    )
    
    # fetch responses
    raw <- download_surveys(surveys, verbose = verbose)
    
    # edit Spanish checklist
    raw[[7]] <- rename_with(raw[[7]], ~ gsub("cat_", "spa_", .), everything())
    
    # edit logs dataset
    raw[[1]] <- raw[[1]] %>%
        mutate(
            # fix codes known to be wrong
            code = fix_code(na_if(.data$code, "")),
            created = as_datetime(.data$created)
        ) %>%
        # remove codes not inlcuded in participants
        filter(.data$code %in% participants_tmp$code) %>%
        # get only last response of each code
        arrange(desc(.data$created)) %>%
        distinct(.data$code, .keep_all = TRUE) %>%
        # remove responses with no code
        drop_na(.data$code, .data$ended) %>%
        # fix codes known to be wrong
        fix_code_raw()
    
    # process data
    processed <- map(raw, select, -any_of(c("created", "modified", "ended", "expired"))) %>%
        reduce(left_join, by = "session") %>%
        mutate(code = fix_code(.data$code)) %>%
        left_join(select(participants_tmp, -.data$comments), by = "code") %>%
        left_join(select(raw$bilexicon_06_words_cat, .data$session, created_cat = .data$created, ended_cat = .data$ended), by = "session") %>%
        left_join(select(raw$bilexicon_06_words_spa, .data$session, created_spa = .data$created, ended_spa = .data$ended), by = "session") %>%
        filter(.data$code %in% participants_tmp$code) %>%
        drop_na(.data$created_cat, .data$created_spa, .data$ended_cat, .data$ended_spa) %>%
        mutate(
            across(c(.data$created_cat, .data$created_spa, .data$ended_cat, .data$ended_spa, .data$date_birth), as_datetime),
            across(starts_with("language_doe"), ~ifelse(is.na(.), 0, .)),
            version = "BL-Long-2",
            time_stamp = get_time_stamp(., c("ended_cat", "ended_spa"), "last"),
            age = diff_in_months(.data$time_stamp, .data$date_birth),
            language_doe_catalan = get_doe(., languages = .env$languages2[grep("catalan", .env$languages2)]),
            language_doe_spanish = get_doe(., languages = .env$languages2[grep("spanish", .env$languages2)]),
            language_doe_others = 100 - rowSums(across(c(.data$language_doe_catalan, .data$language_doe_spanish)), na.rm = TRUE)
        ) %>%
        arrange(desc(.data$time_stamp)) %>%
        distinct(.data$session, .keep_all = TRUE) %>%
        rename(postcode = .data$demo_postcode, edu_parent1 = .data$demo_parent1, edu_parent2 = .data$demo_parent2) %>%
        drop_na(.data$age) %>%
        select(starts_with("id"), one_of(items_to_keep), starts_with("cat_"), starts_with("spa_")) %>%
        pivot_longer(cols = matches("cat_|spa_"), names_to = "item", values_to = "response") %>%
        rename_all(~ gsub("language_", "", .)) %>%
        mutate(
            language = ifelse(grepl("cat_", .data$item), "Catalan", "Spanish"),
            sex = ifelse(.data$sex %in% 1, "Male", "Female"),
            postcode = as.integer(ifelse(.data$postcode %in% "", NA_character_, .data$postcode)),
            across(starts_with("edu_"), na_if, "")
        ) %>%
        arrange(desc(.data$time_stamp)) %>%
        distinct(.data$id, .data$code, .data$item, .keep_all = TRUE)
    
    if (verbose) cli_alert_success(paste0("BL-Long-2 updated: ", nrow(distinct(processed, code)), " responses retrieved"))
    
    return(processed)
}
