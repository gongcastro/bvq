#' Age difference in months
#'
#' @param x Most recent date
#' @param y Least recent date
#' @importFrom lubridate time_length
diff_in_months <- function(x, y) {
    diff <- difftime(x, y)
    diff <- time_length(diff, "months") 
    diff <- ifelse(diff %in% c(-Inf, Inf), NA_real_, diff)
    return(diff)
}

#' Get timestamps
#'
#' @param .data Data frame containing a column for the first time stamp and the
#'   last time stamp of participants' responses the word inventory in each
#'   language (Spanish and Catalan)
#' @param cols Character string vector indicating the name of the columns
#'   containing the first and the last time stamps (in that order) of
#'   participants' responses to a given language inventory.
#' @param which Which time stamp to consider: `'first'` (by default) or
#'   `'last'`?
#' @importFrom lubridate as_datetime
#' @export get_time_stamp
get_time_stamp <- function(.data, cols, which = "first") {
    d <- .data[c(cols[1], cols[2])]
    if (which %in% "first") {
        x <- apply(d, 1, min, na.rm = TRUE)
    } else if (which %in% "last") {
        x <- apply(d, 1, max, na.rm = TRUE)
    }
    x <- as_datetime(x)
    suppressMessages(return(x))
}

#' Summarise language profile
#'
#' @importFrom rlang .data
#' @param .data Data frame that contains each degree of exposure as columns,
#'   named `language_doe_*`.
#' @param languages Character vector of languages to compute degree of exposure
#'   for (all others will be considered as `doe_others`).
get_doe <- function(.data, languages) {
    apply(.data[paste0("language_doe_", languages)], 1, sum, na.rm = TRUE)
}

#' Fix variable version
#' 
#' @param x Vector of \code{version} whose values should be fixed
fix_version <- function(x) {
    trimws(x)
}

#' Fix codes
#' 
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_remove_all
#' @param x Vector of \code{code} whose values should be fixed
fix_code <- function(x) {
    x <- toupper(trimws(x))
    x <- x %>%
        str_remove_all(".*BL") %>%
        str_replace_all(
            c("O" = "0",
              "l" = "L",
              "I" = "L",
              "BLBL" = "BL")
        )
    x <- ifelse(!grepl("BL", x), paste0("BL", x), x)
    return(x)
}


#' Fix raw codes
#'
#' @param x Vector of \code{code} whose values should be fixed, based on
#'   \code{session}
fix_code_raw <- function(x) {
    x[x$session == "-OYU0wA9FPQ9-ugKUpyrz1A0usJZIuM5hb-cbV2yMgGBal5S9q3ReRgphBDDxFEY", "code"] <- "BL1674"
    x[x$session == "ZZiRT3JN4AdKnXMxjEMtU3CzRkniH0hOSZzS-0kzquRt_Ls9PJzmKsY3qm8tQ7Z2", "code"] <- "BL1671"
    x[x$session == "TW8vSEn7YTtbZoe9BaEtRgwNvryWTwSv49dLKb5W0_6bFL306Eiw0Ehg72Q9nqLx", "code"] <- "BL1672"
    x[x$session == "DDjiYrPl-WD951rocaSKH9grkn2T4ZJKjhdCAPDzqNBWyhc8E8wwjOY0CcruNn1m", "code"] <- "BL1673"
    x[x$session == "c9fPw4Lbm5WS0AbBRppv4NVHh4eETxvEitH8lUC1pkt2ulxxHCvXgAYopCGRQSa_", "code"] <- "BL1569"
    x[x$session == "I8ldNrILmQd7Vhtwqk99Y9YaKWrZzlExKeXsRv9_toIEi43BtlfuLI-PmdU4iY7G", "code"] <- "BL1788"
    x[x$session == "dU5CZLLkvmY7SDpe8d0jFQO3xzpmeS0lCOFF_ErjETC1tyyYbv3ZhyaDmlfdJwHc", "code"] <- "BL1876"
    x[x$session == "L4F1jd13H4wyFf6QYGy8hfSURneFr-zfzMn1YFFeBTbTZWWjxYPRbC-rPY6U1qdr", "code"] <- "remove"
    return(x)
}

#' Fix DOEs
#' 
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @param x Vector of \code{doe} whose values should be fixed
fix_doe <- function(x) {
    x %>%
        mutate(
            doe_catalan = case_when(
                id_db == "54469" & time == 2 ~ 0,
                id_db == "57157" & time == 1 ~ 80,
                id_db == "57046" & time == 1 ~ 50,
                code == "BL1582" ~ 30,
                code == "BL1295" ~ 10,
                code == "BL1252" ~ 90,
                .default = doe_catalan
            ),
            doe_spanish = case_when(
                id_db == "57046" & time == 1 ~ 50,
                code == "BL896" ~ 75,
                .default = doe_spanish
            ),
            doe_others = case_when(
                code == "BL1252" ~ 0,
                code == "BL1208" ~ 0,
                code == "BL896" ~ 0,
                code == "BL1582" ~ 0,
                code == "BL1295" ~ 0,
                .default = doe_others
            )
        )
}

#' Fix sex (missing in first responses to BL-Lockdown)
#' 
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr ungroup
#' @param x Vector of \code{sex} whose values should be fixed
fix_sex <- function(x) {
    x$sex <- ifelse(x$id %in% c("bilexicon_1097", 
                                "bilexicon_1441", 
                                "bilexicon_1124",
                                "bilexicon_1448"),
                    "Female",
                    x$sex)
    
    x$sex <- ifelse(x$id %in% c("bilexicon_1447"),
                    "Male",
                    x$sex)
    
    return(x)
}

#' Fix postcode
#' 
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @param x Vector of \code{postcode} whose values should be fixed
fix_postcode <- function(x) {
    mutate(
        x,
        postcode = ifelse(nchar(postcode) < 5, paste0("0", postcode), postcode),
        postcode = ifelse(nchar(postcode) < 5, NA_character_, postcode)
    )
}

#' Fix item
#' 
#' @importFrom dplyr case_when
#' @param x Vector of \code{item} whose values should be fixed
fix_item <- function(x) {
    mutate(
        x,
        item = case_when(
            item == "cat_parc" ~ "cat_parc1",
            item == "cat_eciam" ~ "cat_enciam",
            item == "cat_voler3" ~ "cat_voler2",
            item == "cat_voler" ~ "cat_voler1",
            item == "cat_despres1" ~ "cat_despres",
            item == "cat_peix" ~ "cat_peix1",
            item == "cat_estar" ~ "cat_estar1",
            item == "cat_querer" ~ "cat_querer1",
            item == "cat_estiguestequiet" ~ "cat_estiguesquiet",
            item == "spa_nibla" ~ "spa_niebla",
            item == "spa_ir" ~ "spa_ir1",
            item == "spa_querer" ~ "spa_querer1",
            item == "cat_anar" ~ "cat_anar1",
            .default = item
        )
    )
}


#' Fix study
#' 
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @param x Vector of \code{study} whose values should be fixed
fix_study <- function(x) {
    mutate(
        x,
        study = ifelse(
            is.na(study),
            "BiLexicon",
            study
        )
    )
}

#' Fix id_exp
#' 
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @param x Vector of \code{id_exp} whose values should be fixed
fix_id_exp <- function(x) {
    x$id_exp <- ifelse(x$code %in% "BL547", "bilexicon_189", x$id_exp)
    return(x)
}

#' Proportion, adjusted for zero- and one- inflation
#' 
#' @export prop_adj
#' @param x Number of successes
#' @param n Number of tries
prop_adj <- function(x, n) {
    e <- (x + 2) / (n + 4)
    return(e)
}

#' Standard error of proportion, adjusted for zero- and one-inflation
#' 
#' @export prop_adj_se
#' @param x Number of successes
#' @param n Number of tries
prop_adj_se <- function(x, n) {
    e <- (x + 2) / (n + 4)
    se <- sqrt(e * (1 - e) / (n + 4))
    return(se)
}

#' Confidence interval of proportion, adjusted for zero- and one-inflation
#'
#' @importFrom stats qnorm
#' @export prop_adj_ci
#' @param x Number of successes
#' @param n Number of tries
#' @param .width Confidence level (defaults to .95)
prop_adj_ci <- function(x, n, .width = 0.95) {
    e <- (x + 2) / (n + 4)
    se <- sqrt(e * (1 - e) / (n + 4))
    ci <- e + qnorm(c((1 - .width) / 2, (1 - (1 - .width) / 2))) * se
    ci[1] <- ifelse(ci[1] < 0, 0, ci[1]) # truncate at 0
    ci[2] <- ifelse(ci[2] > 1, 1, ci[2]) # truncate at 1
    return(ci)
}

#' Deal with repeated measures
#'
#' @export get_longitudinal
#'
#' @param x A data frame containing a column for participants (each participant
#'   gets a unique ID), and a column for times (a numeric value indicating how
#'   many times each participant appears in the data frame counting this one).
#'   One participant may appear several times in the data frame, with each time
#'   with a unique value of `time`.
#' @param longitudinal A character string indicating what subset of the
#'   participants should be returned:
#'   * `"all"` (default) returns all participants.
#'   * `"no"` remove all participants with more than one response.
#'   * `"only"` returns only participants with more than one response in the
#'   dataset (i.e., longitudinal participants).
#'   * `"first"` returns the first response of each participant (participants with only one appearance are
#'   included).
#'   * `"last"` returns the last response from each participant (participants with only one response are included).
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr distinct
#' @importFrom dplyr n
#' @importFrom dplyr filter
#' @importFrom dplyr ungroup
#' 
#' @returns A subset of the data frame `x` with only the selected cases,
#'   according to `longitudinal`.
get_longitudinal <- function(x, longitudinal = "all") {
    
    longitudinal_opts <- c("all", "no", "first", "last", "only")
    
    if (!(longitudinal %in% longitudinal_opts)) {
        cli_abort(paste0("longitudinal must be one of: ", 
                         paste0(longitudinal_opts, collapse = ", ")))
    }
    
    repeated <- distinct(x, id, time) %>%
        group_by(id) %>%
        filter(n() > 1) %>%
        ungroup()
    
    if (longitudinal == "no") {
        y <- filter(x, !(id %in% repeated$id))
    } else if (longitudinal == "first") {
        y <- group_by(x, id) %>%
            filter(time == min(time, na.rm = TRUE)) %>%
            ungroup()
    } else if (longitudinal == "last") {
        y <- group_by(x, id) %>%
            filter(time == max(time, na.rm = TRUE)) %>%
            ungroup()
    } else if (longitudinal == "only") {
        y <- filter(x, id %in% repeated$id)
    } else {
        y <- x
    }
    return(y)
}




