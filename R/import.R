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
#' @param surveys Name of formr surveys from the bilexicon_lockdown run
import_formr_lockdown <- function(surveys = c(
                                    "bilexicon_lockdown_01_log",
                                    "bilexicon_lockdown_02_welcome",
                                    "bilexicon_lockdown_03_consent",
                                    "bilexicon_lockdown_04_demo",
                                    "bilexicon_lockdown_05_language",
                                    "bilexicon_lockdown_06_words_catalan",
                                    "bilexicon_lockdown_06_words_spanish"
                                  )) {
  suppressMessages({
    participants <- get("participants", parent.frame()) %>%
      select(-.data$version)

    items_to_keep <- c("code", "bl_code", "consent_mail", "demo_parent1", "demo_parent2", "demo_postcode", "sex", "language_doe_spanish", "language_doe_spanish_american", "language_doe_catalan_barcelona", "language_doe_catalan_majorca", "language_doe_catalan_other")

    raw <- map(surveys, formr_raw_results) %>%
      set_names(surveys) %>%
      map(select, -any_of("language"))

    raw$bilexicon_lockdown_01_log <- raw$bilexicon_lockdown_01_log %>%
      mutate(created = as_datetime(.data$created)) %>%
      arrange(desc(.data$created), .data$bl_code) %>%
      drop_na(.data$bl_code, .data$ended) %>%
      filter(.data$bl_code != "") %>%
      rename(code = .data$bl_code) %>%
      fix_code_raw() %>%
      distinct(.data$code, .keep_all = TRUE)

    processed <- raw %>%
      map(select, -one_of(c("created", "modified", "ended", "expired"))) %>%
      reduce(left_join, by = "session") %>%
      mutate(code = fix_code(.data$code)) %>%
      left_join(participants, by = "code") %>%
      filter(.data$code %in% participants$code) %>%
      left_join(
        select(
          raw$bilexicon_lockdown_06_words_cat,
          .data$session,
          created_cat = .data$created,
          ended_cat = .data$ended
        ),
        by = "session"
      ) %>%
      left_join(
        select(
          raw$bilexicon_lockdown_06_words_spa,
          .data$session,
          created_spa = .data$created,
          ended_spa = .data$ended
        ),
        by = "session"
      ) %>%
      filter(.data$code %in% participants$code) %>%
      drop_na(.data$created_cat, .data$created_spa) %>%
      mutate_at(
        vars("created_cat", "created_spa", "ended_cat", "ended_spa", "date_birth"),
        as_datetime
      ) %>%
      mutate_at(
        vars(starts_with("language_doe")),
        function(x) ifelse(is.na(x), 0, x)
      )

    # breaking pipe to use custom functions (not pipe-friendly at the moment)
    processed <- processed %>%
      mutate(
        version = paste0("BL-Lockdown-", .data$version),
        time_stamp = get_time_stamp(processed, c("ended_cat", "ended_spa"), "last"),
        age = time_length(difftime(time_stamp, date_birth), "months"),
        age = ifelse(.data$age %in% c(-Inf, Inf), NA_real_, .data$age),
        language_doe_catalan = get_doe(processed, languages = .env$languages_lockdown1[grep("catalan", .env$languages_lockdown1)]),
        language_doe_spanish = get_doe(processed, languages = .env$languages_lockdown1[grep("spanish", .env$languages_lockdown1)]),
        language_doe_catalan_lockdown = get_doe(processed, languages = .env$languages_lockdown2[grep("catalan", .env$languages_lockdown2)]),
        language_doe_spanish_lockdown = get_doe(processed, languages = .env$languages_lockdown2[grep("spanish", .env$languages_lockdown2)]),
      ) %>%
      rowwise() %>%
      mutate(
        language_doe_others = 100 - sum(.data$language_doe_catalan, .data$language_doe_spanish, na.rm = TRUE),
        language_doe_others_lockdown = 100 - sum(.data$language_doe_catalan_lockdown, .data$language_doe_spanish_lockdown, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      clean_names() %>%
      arrange(desc(.data$time_stamp)) %>%
      distinct(.data$session, .keep_all = TRUE) %>%
      rename(
        postcode = .data$demo_postcode,
        edu_parent1 = .data$demo_parent1,
        edu_parent2 = .data$demo_parent2
      ) %>%
      rename_all(function(x) gsub("language_", "", x)) %>%
      drop_na(.data$age) %>%
      select(
        starts_with("id"), .data$time, .data$code, .data$study, .data$version,
        .data$randomisation, .data$time_stamp, .data$date_birth, .data$age,
        .data$postcode, .data$sex, starts_with("edu_"), .data$doe_catalan,
        .data$doe_spanish, .data$doe_others, .data$doe_catalan_lockdown,
        .data$doe_spanish_lockdown, .data$doe_others_lockdown, matches("cat_|spa_")
      ) %>%
      pivot_longer(
        cols = matches("cat_|spa_"),
        names_to = "item",
        values_to = "response"
      ) %>%
      mutate(
        language = ifelse(grepl("cat_", .data$item), "Catalan", "Spanish"),
        sex = ifelse(.data$sex == 1, "Male", "Female"),
        postcode = as.integer(ifelse(.data$postcode == "", NA_character_, .data$postcode)),
        edu_parent1 = ifelse(.data$edu_parent1 %in% "", NA_character_, .data$edu_parent1),
        edu_parent2 = ifelse(.data$edu_parent2 %in% "", NA_character_, .data$edu_parent2),
        sex = NA_character_
      ) %>%
      arrange(desc(.data$time_stamp)) %>%
      distinct(.data$id, .data$code, .data$item, .keep_all = TRUE)
  })

  message("BL-Lockdown updated")

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
#' @param surveys Name of formr surveys from the bilexicon_short run
import_formr_short <- function(surveys = c(
                                 "bilexicon_short_01_log",
                                 "bilexicon_short_02_welcome",
                                 "bilexicon_short_03_consent",
                                 "bilexicon_short_04_demo",
                                 "bilexicon_short_05_language",
                                 "bilexicon_short_06_words_catalan",
                                 "bilexicon_short_06_words_spanish"
                               )) {
  suppressMessages({
    participants <- get("participants", parent.frame())
    participants <- filter(participants, version %in% "BL-Short") %>%
      select(-version)

    items_to_keep <- c("code", "bl_code", "consent_mail", "demo_parent1", "demo_parent2", "demo_postcode", "sex", "language_doe_spanish", "language_doe_spanish_american", "language_doe_catalan_barcelona", "language_doe_catalan_majorca", "language_doe_catalan_other")

    # import data
    raw <- map(surveys, formr_raw_results) %>%
      set_names(surveys) %>%
      map(select, -any_of("language"))

    raw$bilexicon_short_06_words_spanish <- rename_all(
      raw$bilexicon_short_06_words_spanish,
      function(x) gsub("cat_", "spa_", x)
    )

    raw$bilexicon_short_01_log <- raw$bilexicon_short_01_log %>%
      mutate(created = as_datetime(.data$created)) %>%
      arrange(desc(.data$created), .data$code) %>%
      fix_code_raw() %>%
      distinct(.data$code, .keep_all = TRUE) %>%
      drop_na(.data$code)

    # process data
    processed <- raw %>%
      map(select, -one_of(c("created", "modified", "ended", "expired"))) %>%
      reduce(left_join, by = "session") %>%
      mutate(code = fix_code(.data$code)) %>%
      left_join(participants, by = "code") %>%
      filter(code %in% participants$code) %>%
      left_join(select(raw$bilexicon_short_06_words_cat, .data$session, created_cat = .data$created, ended_cat = .data$ended), by = "session") %>%
      left_join(select(raw$bilexicon_short_06_words_spa, .data$session, created_spa = .data$created, ended_spa = .data$ended), by = "session") %>%
      filter(.data$code %in% participants$code) %>%
      drop_na(.data$created_cat, .data$created_spa) %>%
      mutate_at(vars("created_cat", "created_spa", "ended_cat", "ended_spa", "date_birth"), as_datetime) %>%
      mutate_at(vars(starts_with("language_doe_")), function(x) ifelse(is.na(x), 0, x))

    # breaking pipe to use custom functions (not pipe-friendly at the moment)
    processed <- processed %>%
      mutate(
        version = paste0("BL-Short-", .data$version),
        time_stamp = get_time_stamp(processed, c("ended_cat", "ended_spa"), "last"),
        age = time_length(difftime(time_stamp, date_birth), "months"),
        age = ifelse(.data$age %in% c(-Inf, Inf), NA_real_, .data$age),
        language_doe_catalan = get_doe(processed, languages = .env$languages_short[grep("catalan", languages_short)]),
        language_doe_spanish = get_doe(processed, languages = .env$languages_short[grep("spanish", languages_short)])
      ) %>%
      rowwise() %>%
      mutate(language_doe_others = 100 - sum(.data$language_doe_catalan, .data$language_doe_spanish, na.rm = TRUE)) %>%
      ungroup() %>%
      clean_names() %>%
      arrange(desc(.data$time_stamp)) %>%
      distinct(.data$session, .keep_all = TRUE) %>%
      rename(postcode = .data$demo_postcode, edu_parent1 = .data$demo_parent1, edu_parent2 = .data$demo_parent2) %>%
      rename_all(function(x) gsub("language_", "", x)) %>%
      drop_na(.data$age) %>%
      select(
        starts_with("id"), .data$time, .data$code, .data$study, .data$version,
        .data$randomisation, .data$time_stamp, .data$date_birth, .data$age,
        .data$sex, .data$postcode, starts_with("edu_"), .data$doe_catalan, .data$doe_spanish,
        matches("cat_|spa_")
      ) %>%
      # group_by(id, time, code) %>%
      # summarise_all(coalesce_by_column) %>%
      # ungroup() %>%
      pivot_longer(cols = matches("cat_|spa_"), names_to = "item", values_to = "response") %>%
      mutate(
        language = ifelse(grepl("cat_", .data$item), "Catalan", "Spanish"),
        sex = ifelse(.data$sex %in% 1, "Male", "Female"),
        postcode = as.integer(ifelse(.data$postcode %in% "", NA_character_, .data$postcode)),
        edu_parent1 = ifelse(.data$edu_parent1 %in% "", NA_character_, .data$edu_parent1),
        edu_parent2 = ifelse(.data$edu_parent2 %in% "", NA_character_, .data$edu_parent2)
      ) %>%
      arrange(desc(.data$time_stamp)) %>%
      distinct(.data$id, .data$code, .data$item, .keep_all = TRUE)
  })

  message("BL-Short updated")

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
#' @param surveys Name of formr surveys from the bilexicon_long2 run
import_formr2 <- function(surveys = c(
                            "bilexicon_01_log",
                            "bilexicon_02_welcome",
                            "bilexicon_03_consent",
                            "bilexicon_04_demo",
                            "bilexicon_05_language",
                            "bilexicon_06_words_cat",
                            "bilexicon_06_words_spa"
                          )) {
  suppressMessages({
    participants <- get("participants", parent.frame())

    participants <- filter(
      participants,
      .data$version %in% "BL-Long",
      .data$randomisation %in% "2"
    ) %>%
      select(-.data$version)

    items_to_keep <- c("code", "bl_code", "consent_mail", "demo_parent1", "demo_parent2", "demo_postcode", "sex", "language_doe_spanish", "language_doe_spanish_american", "language_doe_catalan_barcelona", "language_doe_catalan_majorca", "language_doe_catalan_other")

    # import data
    raw <- map(surveys, formr_raw_results) %>%
      set_names(surveys) %>%
      map(select, -any_of("language"))

    raw$bilexicon_06_words_spa <- rename_all(
      raw$bilexicon_06_words_spa,
      function(x) gsub("cat_", "spa_", x)
    )

    raw$bilexicon_01_log <- raw$bilexicon_01_log %>%
      mutate(created = as_datetime(.data$created)) %>%
      arrange(desc(.data$created), .data$code) %>%
      drop_na(.data$code, .data$ended) %>%
      filter(.data$code != "") %>%
      fix_code_raw() %>%
      distinct(.data$code, .keep_all = TRUE)

    # process data
    processed <- map(raw, select, -any_of(c("created", "modified", "ended", "expired"))) %>%
      reduce(left_join, by = "session") %>%
      mutate(code = fix_code(.data$code)) %>%
      left_join(select(participants, -.data$comments), by = "code") %>%
      left_join(select(raw$bilexicon_06_words_cat, .data$session, created_cat = .data$created, ended_cat = .data$ended), by = "session") %>%
      left_join(select(raw$bilexicon_06_words_spa, .data$session, created_spa = .data$created, ended_spa = .data$ended), by = "session") %>%
      filter(.data$code %in% participants$code) %>%
      drop_na(.data$created_cat, .data$created_spa, .data$ended_cat, .data$ended_spa) %>%
      mutate_at(
        vars("created_cat", "created_spa", "ended_cat", "ended_spa", "date_birth"),
        as_datetime
      ) %>%
      mutate_at(vars(starts_with("language_doe")), function(x) ifelse(is.na(x), 0, x))

    # breaking pipe to use custom functions (not pipe-friendly at the moment)
    processed <- processed %>%
      mutate(
        version = "BL-Long-2",
        time_stamp = get_time_stamp(processed, c("ended_cat", "ended_spa"), "last"),
        age = time_length(difftime(time_stamp, date_birth), "months"),
        age = ifelse(.data$age %in% c(-Inf, Inf), NA_real_, .data$age),
        language_doe_catalan = get_doe(processed, languages = .env$languages2[grep("catalan", .env$languages2)]),
        language_doe_spanish = get_doe(processed, languages = .env$languages2[grep("spanish", .env$languages2)])
      ) %>%
      rowwise() %>%
      mutate(language_doe_others = 100 - sum(.data$language_doe_catalan, .data$language_doe_spanish, na.rm = TRUE)) %>%
      ungroup() %>%
      clean_names() %>%
      arrange(desc(.data$time_stamp)) %>%
      distinct(.data$session, .keep_all = TRUE) %>%
      rename(
        postcode = .data$demo_postcode,
        edu_parent1 = .data$demo_parent1,
        edu_parent2 = .data$demo_parent2
      ) %>%
      rename_all(function(x) gsub("language_", "", x)) %>%
      drop_na(.data$age) %>%
      select(
        starts_with("id"), .data$time, .data$code, .data$study, .data$version,
        .data$randomisation, .data$time_stamp, .data$date_birth, .data$age, .data$sex,
        .data$postcode, starts_with("edu_"), .data$doe_catalan, .data$doe_spanish,
        .data$doe_others, matches("cat_|spa_")
      ) %>%
      pivot_longer(cols = matches("cat_|spa_"), names_to = "item", values_to = "response") %>%
      mutate(
        language = ifelse(grepl("cat_", .data$item), "Catalan", "Spanish"),
        sex = ifelse(.data$sex %in% 1, "Male", "Female"),
        postcode = as.integer(ifelse(.data$postcode %in% "", NA_character_, .data$postcode)),
        edu_parent1 = ifelse(.data$edu_parent1 %in% "", NA_character_, .data$edu_parent1),
        edu_parent2 = ifelse(.data$edu_parent2 %in% "", NA_character_, .data$edu_parent2)
      ) %>%
      arrange(desc(.data$time_stamp)) %>%
      distinct(.data$id, .data$code, .data$item, .keep_all = TRUE)
  })

  message("BL-Long-2 updated")

  return(processed)
}
