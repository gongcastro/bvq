#' Download formR surveys
#' 
#' @import cli
#'
#' @param surveys Name of the surveys in the formr run.
#' @param ... Unused.
#' @param author Gonzalo Garcia-Castro
#' @md
download_surveys <- function(surveys, ...) {
    n <- length(surveys)
    i <- 0
    raw <- vector(mode = "list", length = n)
    if (interactive()) {
        cli_progress_step(msg = "Downloaded {i}/{n} {qty(i)}survey{?s}")
    }
    for (i in 1:length(surveys)) {
        raw[[i]] <- formr_raw_results(surveys[i])
        if (interactive()) cli_progress_update()
    }
    
    if (interactive()) {
        cli_progress_done(result = "done")
    }
    raw <- lapply(raw, select, -any_of("language"))
    names(raw) <- surveys
    return(raw)
}

#' Import lockdown data
#' 
#' @import dplyr
#' @importFrom formr formr_raw_results
#' @importFrom lubridate as_datetime
#' @importFrom lubridate time_length
#' @importFrom tidyr drop_na
#' @importFrom tidyr pivot_longer
#' @importFrom janitor clean_names
#' @importFrom rlang .env
#' @importFrom cli cli_alert_success
#'
#' @param surveys Name of formr surveys from the bilexicon_lockdown run.
#' @param ... Unused.
#' @author Gonzalo Garcia-Castro
#' @md
import_formr_lockdown <- function(
        surveys = c(
            "bilexicon_lockdown_01_log",
            "bilexicon_lockdown_02_welcome",
            "bilexicon_lockdown_03_consent",
            "bilexicon_lockdown_04_demo",
            "bilexicon_lockdown_05_language",
            "bilexicon_lockdown_06_words_catalan",
            "bilexicon_lockdown_06_words_spanish"
        ),
        ...) {
    
    participants_tmp <- get("participants", parent.frame()) %>% 
        select(-version)
    
    # fetch responses
    raw <- download_surveys(surveys)
    
    # edit logs dataset
    raw[[1]] <- raw[[1]] %>%
        # fix codes known to be wrong
        rename(code = bl_code) %>%
        mutate(code = fix_code(na_if(code, "")),
               created = as_datetime(created)) %>%
        # remove responses with no code
        drop_na(code, ended) %>%
        # fix codes known to be wrong
        fix_code_raw() %>%
        # remove codes not inlcuded in participants
        filter(code %in% participants_tmp$code) %>%
        # get only last response of each code
        arrange(desc(created)) %>%
        distinct(code, .keep_all = TRUE)
    
    items_to_keep <- c("time",
                       "code",
                       "study",
                       "version",
                       "randomisation",
                       "date_started",
                       "date_finished",
                       "date_birth",
                       "age",
                       "postcode",
                       "sex",
                       "edu_parent1",
                       "edu_parent2",
                       "language_doe_catalan",
                       "language_doe_spanish",
                       "language_doe_others")
    
    # language of interest
    lang_cols <- list(catalan = languages_lockdown1[grep("catalan", languages_lockdown1)],
                      spanish = languages_lockdown1[grep("spanish", languages_lockdown1)]) %>% 
        map(function(x) paste0("language_doe_", x))
    
    processed <- raw %>%
        map(drop_na, created) %>%
        map(select, -one_of(c("created", "modified", "ended", "expired"))) %>%
        # if a single session ID has multiple entries, select most recent
        reduce(inner_join, 
               by = join_by(session),
               multiple = "all") %>%
        left_join(participants_tmp,
                  by = join_by(code)) %>%
        left_join(select(raw[[6]], session, created_cat = created, ended_cat = ended), 
                  by = join_by(session),
                  multiple = "all") %>%
        left_join(select(raw[[7]], session, created_spa = created, ended_spa = ended),
                  by = join_by(session),
                  multiple = "all") %>%
        mutate(across(c(created_cat, created_spa, ended_cat, ended_spa, date_birth), as_datetime),
               across(starts_with("language_doe"), ~ ifelse(is.na(.), 0, .)),
               version = paste0("BL-Lockdown-", version),
               date_started = get_time_stamp(., c("ended_cat", "ended_spa"), "first"),
               date_finished = get_time_stamp(., c("ended_cat", "ended_spa"), "last"),
               # calculate age in months
               age = diff_in_months(date_finished, date_birth),
               time_stamp = get_time_stamp(., c("ended_cat", "ended_spa"), "last"),
               age = diff_in_months(time_stamp, date_birth),
               language_doe_catalan = get_doe(lang_cols$catalan),
               language_doe_spanish = get_doe(lang_cols$spanish),
               language_doe_others = 100 - rowSums(across(c(language_doe_catalan,
                                                            language_doe_spanish)),
                                                   na.rm = TRUE)
        ) %>% 
        arrange(desc(time_stamp)) %>%
        distinct(session, .keep_all = TRUE) %>%
        rename(postcode = demo_postcode,
               edu_parent1 = demo_parent1,
               edu_parent2 = demo_parent2) %>%
        drop_na(age) %>%
        select(starts_with("id"),
               one_of(items_to_keep),
               starts_with("cat_"),
               starts_with("spa_")) %>% 
        pivot_longer(cols = matches("cat_|spa_"),
                     names_to = "item",
                     values_to = "response") %>%
        rename_with(~ gsub("language_", "", .), everything()) %>%
        mutate(
            language = ifelse(grepl("cat_", item), "Catalan", "Spanish"),
            sex = ifelse(sex == 1, "Male", "Female"),
            postcode = na_if(as.character(postcode), ""),
            edu_parent1 = na_if(edu_parent1, ""),
            edu_parent2 = na_if(edu_parent2, ""),
            sex = NA_character_
        ) %>%
        arrange(desc(date_finished))
    
    if (interactive()) {
        cli_alert_success(paste0(
            "BL-Lockdown updated: ",
            nrow(distinct(processed, code)),
            " responses retrieved"
        ))
    }
    
    return(processed)
}


#' Import short
#' 
#' @import dplyr
#' @importFrom formr formr_raw_results
#' @importFrom lubridate as_datetime
#' @importFrom tidyr drop_na
#' @importFrom tidyr pivot_longer
#' @importFrom janitor clean_names
#' @importFrom rlang .env
#' @importFrom cli cli_alert_success
#'
#' @param surveys Name of formr surveys from the bilexicon_short run
#' @param ... Unused.
#' @author Gonzalo Garcia-Castro
#' @md
import_formr_short <- function(
        surveys = c(
            "bilexicon_short_01_log",
            "bilexicon_short_02_welcome",
            "bilexicon_short_03_consent",
            "bilexicon_short_04_demo",
            "bilexicon_short_05_language",
            "bilexicon_short_06_words_catalan",
            "bilexicon_short_06_words_spanish"
        ), ...) {
    
    participants_tmp <- get("participants", parent.frame()) %>%
        filter(version %in% "BL-Short") %>%
        select(-version)
    
    # fetch responses
    raw <- download_surveys(surveys)
    
    # edit Spanish inventory
    raw[[7]] <- rename_all(raw[[7]], ~ gsub("cat_", "spa_", .))
    
    # edit logs dataset
    raw[[1]] <- raw[[1]] %>%
        # fix codes known to be wrong
        mutate(code = fix_code(na_if(code, "")),
               created = as_datetime(created)) %>%
        # remove codes not inlcuded in participants
        filter(code %in% participants_tmp$code) %>%
        # get only last response of each code
        arrange(desc(created)) %>%
        distinct(code, .keep_all = TRUE) %>%
        # remove responses with no code
        drop_na(code, ended) %>%
        # fix codes known to be wrong
        fix_code_raw()
    
    items_to_keep <- c("time",
                       "code",
                       "study",
                       "version",
                       "randomisation",
                       "time_stamp",
                       "date_birth",
                       "age",
                       "sex",
                       "postcode",
                       "edu_parent1",
                       "edu_parent2",
                       "language_doe_catalan",
                       "language_doe_spanish",
                       "language_doe_others")
    
    # language of interest
    lang_cols <- list(catalan = languages_short[grep("catalan", languages_short)],
                      spanish = languages_short[grep("spanish", languages_short)]) %>% 
        map(function(x) paste0("language_doe_", x))
    
    # process data
    processed <- raw %>%
        map(select, -one_of(c("created", "modified", "ended", "expired"))) %>%
        # if a single session ID has multiple entries, select all
        reduce(inner_join, 
               by = join_by(session),
               multiple = "all") %>%   
        mutate(code = fix_code(code)) %>%
        left_join(participants_tmp, by = join_by(code)) %>%
        filter(code %in% participants_tmp$code) %>%
        left_join(select(raw$bilexicon_short_06_words_cat, session,
                         created_cat = created, ended_cat = ended), 
                  by = join_by(session),
                  multiple = "all") %>%
        left_join(select(raw$bilexicon_short_06_words_spa, session,
                         created_spa = created, ended_spa = ended), 
                  by = join_by(session),
                  multiple = "all") %>%
        filter(code %in% participants_tmp$code) %>%
        drop_na(created_cat, created_spa) %>%
        mutate(
            across(c(matches("created_|ended_"), date_birth), as_datetime),
            across(starts_with("language_doe_"), function(x) ifelse(is.na(x), 0, x)),
            version = paste0("BL-Short-", version),
            time_stamp = get_time_stamp(., c("ended_cat", "ended_spa"), "last"),
            age = diff_in_months(time_stamp, date_birth),
            language_doe_catalan = get_doe(lang_cols$catalan),
            language_doe_spanish = get_doe(lang_cols$spanish),
            language_doe_others = 100 - rowSums(across(c(language_doe_catalan,
                                                         language_doe_spanish)),
                                                na.rm = TRUE)
        ) %>% 
        arrange(desc(time_stamp)) %>%
        distinct(session, .keep_all = TRUE) %>%
        rename(postcode = demo_postcode,
               edu_parent1 = demo_parent1,
               edu_parent2 = demo_parent2) %>%
        drop_na(age) %>%
        select(starts_with("id"),
               one_of(items_to_keep),
               starts_with("cat_"),
               starts_with("spa_")) %>% 
        pivot_longer(cols = matches("cat_|spa_"),
                     names_to = "item",
                     values_to = "response") %>%
        rename_all(function(x) gsub("language_", "", x)) %>%
        mutate(
            language = ifelse(grepl("cat_", item), "Catalan", "Spanish"),
            sex = ifelse(sex %in% 1, "Male", "Female"),
            postcode = na_if(as.character(postcode), ""),
            edu_parent1 = ifelse(edu_parent1 %in% "", NA_character_, edu_parent1),
            edu_parent2 = ifelse(edu_parent2 %in% "", NA_character_, edu_parent2)
        ) %>%
        arrange(desc(time_stamp)) %>%
        distinct(id, code, item, .keep_all = TRUE)
    
    if (interactive()) {
        cli_alert_success(paste0(
            "BL-Short updated: ",
            nrow(distinct(processed, code)),
            " responses retrieved"
        ))
    }
    
    return(processed)
}

#' Import formr 2
#' 
#' @import dplyr
#' @importFrom formr formr_raw_results
#' @importFrom lubridate as_datetime
#' @importFrom tidyr drop_na
#' @importFrom tidyr pivot_longer
#' @importFrom janitor clean_names
#' @importFrom rlang .env
#' @importFrom cli cli_alert_success
#' 
#' @param surveys Name of formr surveys from the bilexicon_long2 run.
#' @param ... Unused.
#' @author Gonzalo Garcia-Castro
#' @md
import_formr2 <- function(
        surveys = c(
            "bilexicon_01_log",
            "bilexicon_02_welcome",
            "bilexicon_03_consent",
            "bilexicon_04_demo",
            "bilexicon_05_language",
            "bilexicon_06_words_cat",
            "bilexicon_06_words_spa"
        ), ...) {
    
    participants_tmp <- get("participants", parent.frame()) %>%
        filter(version %in% "BL-Long",
               randomisation %in% "2") %>%
        select(-version)
    
    items_to_keep <- c(
        "time",
        "code",
        "study",
        "version",
        "randomisation",
        "time_stamp",
        "date_birth",
        "age",
        "sex",
        "postcode",
        "edu_parent1",
        "edu_parent2",
        "language_doe_catalan",
        "language_doe_spanish",
        "language_doe_others"
    )
    
    # language of interest
    lang_cols <- list(catalan = languages2[grep("catalan", languages2)],
                      spanish = languages2[grep("spanish", languages2)]) %>% 
        map(function(x) paste0("language_doe_", x))
    
    # fetch responses
    raw <- download_surveys(surveys)
    
    # edit Spanish checklist
    raw[[7]] <-
        rename_with(raw[[7]], ~ gsub("cat_", "spa_", .), everything())
    
    # edit logs dataset
    raw[[1]] <- raw[[1]] %>%
        # fix codes known to be wrong
        mutate(code = fix_code(na_if(code, "")),
               created = as_datetime(created)) %>%
        # remove codes not included in participants
        filter(code %in% participants_tmp$code) %>%
        # get only last response of each code
        arrange(desc(created)) %>%
        distinct(code, .keep_all = TRUE) %>%
        # remove responses with no code
        drop_na(code, ended) %>%
        # fix codes known to be wrong
        fix_code_raw()
    
    # process data
    processed <- map(raw, select, -any_of(c("created", "modified", "ended", "expired"))) %>%
        # if a single session ID has multiple entries, select most recent
        reduce(inner_join, 
               by = join_by(session),
               multiple = "all") %>%
        mutate(code = fix_code(code)) %>%
        left_join(select(participants_tmp, -comments),
                  by = join_by(code)) %>%
        left_join(select(raw$bilexicon_06_words_cat, session, 
                         created_cat = created, ended_cat = ended),
                  by = join_by(session),
                  multiple = "all") %>%
        left_join(select(raw$bilexicon_06_words_spa, session,
                         created_spa = created, ended_spa = ended), 
                  by = join_by(session),
                  multiple = "all") %>%
        filter(code %in% participants_tmp$code) %>%
        drop_na(created_cat, created_spa, ended_cat, ended_spa) %>%
        mutate(
            across(
                c(created_cat, created_spa, ended_cat, ended_spa, date_birth),
                as_datetime
            ),
            across(starts_with("language_doe"), ~ ifelse(is.na(.), 0, .)),
            version = "BL-Long-2",
            time_stamp = get_time_stamp(., c("ended_cat", "ended_spa"), "last"),
            age = diff_in_months(time_stamp, date_birth),
            language_doe_catalan = get_doe(lang_cols$catalan),
            language_doe_spanish = get_doe(lang_cols$spanish),
            language_doe_others = 100 - rowSums(across(c(language_doe_catalan,
                                                         language_doe_spanish)),
                                                na.rm = TRUE)
        ) %>% 
        arrange(desc(time_stamp)) %>%
        distinct(session, .keep_all = TRUE) %>%
        rename(postcode = demo_postcode, 
               edu_parent1 = demo_parent1,
               edu_parent2 = demo_parent2) %>%
        drop_na(age) %>%
        select(starts_with("id"),
               one_of(items_to_keep), 
               starts_with("cat_"), 
               starts_with("spa_")) %>%
        pivot_longer(cols = matches("cat_|spa_"), 
                     names_to = "item",
                     values_to = "response") %>%
        rename_all(~ gsub("language_", "", .)) %>%
        mutate(language = ifelse(grepl("cat_", item), 
                                 "Catalan",
                                 "Spanish"),
               sex = ifelse(sex %in% 1, "Male", "Female"),
               postcode = na_if(as.character(postcode), ""),
               across(starts_with("edu_"), function(x) na_if(x, ""))
        ) %>%
        arrange(desc(time_stamp)) %>%
        distinct(id, code, item, .keep_all = TRUE)
    
    if (interactive()) {
        cli_alert_success(paste0(
            "BL-Short updated: ",
            nrow(distinct(processed, code)),
            " responses retrieved"
        ))
    }
    
    return(processed)
}
