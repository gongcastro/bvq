#' Age difference in time
#'
#' Returns the absolute difference in time elapsed between two dates, as indicated by [lubridate::time_length()].
#'
#' @param x Most recent date
#' @param y Least recent date
#' @param units Time units in which the time difference should be returned. Passed to [lubridate::time_length].
#' 
#' @importFrom lubridate time_length
#' 
#' @returns Absolute difference in months elapsed between `x` and `y`.
#' 
#' @author Gonzalo Garcia-Castro
#' 
#' @noRd
#' @keywords internal
#' 
#' @examples
#' diff_in_time(as.Date("2023-02-01"), as.Date("2022-02-01"))
diff_in_time <- function(x, y, units = "months") {
    diff <- difftime(x, y)
    diff <- abs(time_length(diff, units))
    diff <- ifelse(diff %in% c(-Inf, Inf), NA_real_, diff)
    return(diff)
}

#' Get timestamps
#'
#' @param ... Columns containing the first and the last time stamps (in that order) of
#'   participants' responses to a given language inventory.
#' @param which Which time stamp to consider:
#' * `'first'` (by default)
#' * `'last'`
#'
#' @returns A datetime vector with the first or last of each time stamp
#'
#' @author Gonzalo Garcia-Castro
#' @importFrom lubridate as_datetime
#' @importFrom dplyr across
#' 
#' @noRd
#' @keywords internal
#'
#' @examples
#' library(dplyr)
#' x <- data.frame(
#'   start = as.Date(c("2023-02-01", "2023-02-20")),
#'   finish = as.Date(c("2023-03-15", "2023-02-21"))
#' )
#'
#' y <- mutate(x,
#'   time_stamp_default = get_time_stamp(start, finish),
#'   time_stamp_first = get_time_stamp(start, finish, which = "first"),
#'   time_stamp_last = get_time_stamp(start, finish, which = "last")
#' )
#' (y)
#'
get_time_stamp <- function(..., which = "first") {
    if (!(which == "first" || which == "last")) {
        cli_abort("`which` must be 'first' or 'last'")
    }
    
    if (which == "first") fun <- min else fun <- max
    x <- lubridate::as_datetime(apply(across(c(...)), 1, fun, na.rm = TRUE))
    
    return(x)
}

#' Summarise language profile
#'
#' @param ... Columns with the degree of exposures to be summed up
#'   for (all others will be considered as `doe_others`).
#'   
#' @author Gonzalo Garcia-Castro
#'
#' @returns A numeric vector with the row-wise sums of the columns specified in `...`.
#'
#' @importFrom dplyr across
#' 
#' @export
#' 
#' @examples
#' library(dplyr)
#' x <- data.frame(
#'   doe_cat_1 = seq(0, 1, 0.1),
#'   doe_cat_2 = c(0, rep(c(0.1, 0), each = 5)),
#'   doe_spa_1 = c(0, rep(c(0.1, 0), each = 5)),
#'   doe_spa_2 = c(1, 0.7, 0.6, 0.5, 0.3, 0.1, 0.4, 0.3, 0.2, 0.1, 0)
#' )
#'
#' y <- mutate(x,
#'   doe_other = 1 - get_doe(matches("cat|spa")),
#'   doe_cat = get_doe(doe_cat_1, doe_cat_2),
#'   doe_spa = get_doe(matches("spa"))
#' )
#'
#' (y)
get_doe <- function(...) {
    rowSums(across(c(...)), dims = 1, na.rm = TRUE)
}

#' Fix response IDs
#'
#' @param x Vector of `response_id` whose values should be fixed.
#' 
#' @author Gonzalo Garcia-Castro
#' 
#' @noRd
#' @keywords internal
#' 
fix_response_id <- function(x) { # nocov start
    x <- toupper(trimws(x))
    x <- gsub("O", "0", x)
    x <- gsub("I", "L", x)
    x <- gsub("[^\\d]+", "", x, perl = TRUE)
    x <- as.character(x)
    return(x)
} # nocov end


#' Fix raw response IDs
#'
#' @param x Vector of `response_id` whose values should be fixed, based on `session`.
#' 
#' @author Gonzalo Garcia-Castro
#' 
#' @noRd
#' @keywords internal
#' 
fix_code_raw <- function(x) { # nocov start
    x[x$session == "-OYU0wA9FPQ9-ugKUpyrz1A0usJZIuM5hb-cbV2yMgGBal5S9q3ReRgphBDDxFEY", "response_id"] <- "BL1674"
    x[x$session == "ZZiRT3JN4AdKnXMxjEMtU3CzRkniH0hOSZzS-0kzquRt_Ls9PJzmKsY3qm8tQ7Z2", "response_id"] <- "BL1671"
    x[x$session == "TW8vSEn7YTtbZoe9BaEtRgwNvryWTwSv49dLKb5W0_6bFL306Eiw0Ehg72Q9nqLx", "response_id"] <- "BL1672"
    x[x$session == "DDjiYrPl-WD951rocaSKH9grkn2T4ZJKjhdCAPDzqNBWyhc8E8wwjOY0CcruNn1m", "response_id"] <- "BL1673"
    x[x$session == "c9fPw4Lbm5WS0AbBRppv4NVHh4eETxvEitH8lUC1pkt2ulxxHCvXgAYopCGRQSa_", "response_id"] <- "BL1569"
    x[x$session == "I8ldNrILmQd7Vhtwqk99Y9YaKWrZzlExKeXsRv9_toIEi43BtlfuLI-PmdU4iY7G", "response_id"] <- "BL1788"
    x[x$session == "dU5CZLLkvmY7SDpe8d0jFQO3xzpmeS0lCOFF_ErjETC1tyyYbv3ZhyaDmlfdJwHc", "response_id"] <- "BL1876"
    x[x$session == "L4F1jd13H4wyFf6QYGy8hfSURneFr-zfzMn1YFFeBTbTZWWjxYPRbC-rPY6U1qdr", "response_id"] <- "remove"
    return(x)
} # nocov end

#' Fix DOEs
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' 
#' @param x Vector of `doe` whose values should be fixed
#' 
#' @author Gonzalo Garcia-Castro
#' 
#' @noRd
#' @keywords internal
#' 
fix_doe <- function(x) { # nocov start
    
    x$doe_catalan[x$child_id=="54469" & x$time == 2] <- 0
    x$doe_catalan[x$child_id=="57157" & x$time == 1] <- 80
    x$doe_catalan[x$child_id=="57046" & x$time == 1] <- 50
    x$doe_spanish[x$child_id=="57046" & x$time == 2] <- 50
    x$doe_catalan[x$child_id=="54469" & x$time == 2] <- 30
    x$doe_catalan[x$response_id=="BL1295" & x$time == 2] <- 10
    x$doe_catalan[x$response_id=="BL1252" & x$time == 2] <- 90
    x$doe_spanish[x$response_id=="BL896"] <- 75
    x$doe_others[x$response_id=="BL1252"] <- 0
    x$doe_others[x$response_id=="BL1208"] <- 0
    x$doe_others[x$response_id=="BL896"] <- 0
    x$doe_others[x$response_id=="BL1582"] <- 0
    x$doe_others[x$response_id=="BL1295"] <- 0
    
    return(x)
    
} # nocov end

#' Fix sex (missing in first responses to BL-Lockdown)
#'
#' @param x Vector of `sex` whose values should be fixed.
#' 
#' @author Gonzalo Garcia-Castro
#' 
#' @noRd
#' @keywords internal
#' 
fix_sex <- function(x) { # nocov start
    
    child_id_female <- c("54917",
                         "58294",
                         "54977",
                         "54925")
    x$sex[x$child_id %in% child_id_female] <- "Female"
    x$sex[x$child_id %in% "58276"] <- "Male"
    
    return(x)
} # nocov end


#' Fix item
#'
#' @importFrom dplyr case_when
#' 
#' @param x Vector of `item` whose values should be fixed
#' 
#' @noRd
#' @keywords internal
#' 
fix_item <- function(x) { # nocov start
    
    x$item[x$item == "cat_parc"] <- "cat_parc1"
    x$item[x$item == "cat_eciam"] <- "cat_enciam"
    x$item[x$item == "cat_voler3"] <- "cat_voler2"
    x$item[x$item == "cat_voler"] <- "cat_voler1"
    x$item[x$item == "cat_despres1"] <- "cat_despres"
    x$item[x$item == "cat_peix"] <- "cat_peix1"
    x$item[x$item == "cat_estar"] <- "cat_estar1"
    x$item[x$item == "cat_querer"] <- "cat_querer1"
    x$item[x$item == "cat_estiguestequiet"] <- "cat_estiguesquiet"
    x$item[x$item == "spa_nibla"] <- "spa_niebla"
    x$item[x$item == "spa_ir"] <- "spa_ir1"
    x$item[x$item == "spa_querer"] <- "spa_querer1"
    x$item[x$item == "cat_anar"] <- "cat_anar1"
    
    return(x)
} # nocov end


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
#' @importFrom dplyr distinct
#' @importFrom dplyr n
#' @importFrom dplyr filter
#' 
#' @returns A subset of the data frame `x` with only the selected cases,
#'   according to `longitudinal`.
#'   
#' @author Gonzalo Garcia-Castro
#' 
#' @examples
#' child_id <- c(1, 1, 1, 2, 2, 3, 4, 4, 4, 4, 5, 6, 7, 7, 8, 9, 10, 10)
#' sums <- rle(sort(child_id))[["lengths"]]
#' dat <- data.frame(child_id, time = unlist(sapply(sums, function(x) seq(1, x))))
#'
#' (dat)
#'
#' get_longitudinal(dat, "first")
#' get_longitudinal(dat, "only")
get_longitudinal <- function(x, longitudinal = "all") {
    longitudinal_opts <- c("all", "no", "first", "last", "only")
    
    if (!(longitudinal %in% longitudinal_opts)) {
        long_colapsed <- paste0(longitudinal_opts, collapse = ", ")
        cli_abort(paste0("longitudinal must be one of: ", long_colapsed))
    }
    
    repeated <- filter(distinct(x, child_id, time), n() > 1, .by = child_id)
    
    if (longitudinal == "no") x <- filter(x, !(child_id %in% repeated$child_id))
    if (longitudinal == "first") x <- filter(x, time==min(time), .by = child_id)
    if (longitudinal == "last") x <- filter(x, time==max(time), .by = child_id)
    if (longitudinal == "only") x <- filter(x, child_id %in% repeated$child_id)
    
    return(x)
}

#' Get BVQ formr runs
#'
#' @author Gonzalo Garcia-Castro
#' 
#' @noRd
#' @keywords internal
#' 
#' @md
get_bvq_runs <- function() { # nocov start
    runs <- list(
        "bvq-1.0.0" = c(
            "bvq_01_log",
            "bvq_02_welcome",
            "bvq_03_consent",
            "bvq_04_demo",
            "bvq_05_language",
            "bvq_06_words_catalan",
            "bvq_06_words_spanish"
        ),
        "bvq-lockdown" = c(
            "bilexicon_lockdown_01_log",
            "bilexicon_lockdown_02_welcome",
            "bilexicon_lockdown_03_consent",
            "bilexicon_lockdown_04_demo",
            "bilexicon_lockdown_05_language",
            "bilexicon_lockdown_06_words_catalan",
            "bilexicon_lockdown_06_words_spanish"
        ),
        "bvq-short" = c(
            "bilexicon_short_01_log",
            "bilexicon_short_02_welcome",
            "bilexicon_short_03_consent",
            "bilexicon_short_04_demo",
            "bilexicon_short_05_language",
            "bilexicon_short_06_words_catalan",
            "bilexicon_short_06_words_spanish"
        ),
        "bvq-long" = c(
            "bilexicon_01_log",
            "bilexicon_02_welcome",
            "bilexicon_03_consent",
            "bilexicon_04_demo",
            "bilexicon_05_language",
            "bilexicon_06_words_cat",
            "bilexicon_06_words_spa"
        )
    )
    
    attr(runs, "versions") <- c("bvq-devlex" = "DevLex",
                                "bvq-phocross" = "PhoCross",
                                "bvq-long" = "BL-Long",
                                "bvq-short" = "BL-Short",
                                "bvq-lockdown" = "BL-Lockdown",
                                "bvq-1.0.0" = "1.0.0")
    return(runs)
} # nocov end


#' Proportion, adjusted for zero- and one-inflation
#' 
#' @details
#' It is very common that a large proportion of the participants know or do not know some word.
#' Vocabulary sizes and word prevalence norms in package are calculated using an estimate that
#' adjusts for zero- and one-inflation so that, at the population level such estimates are more
#' likely to be accurate.
#' 
#'
#' @export prop_adj
#'
#' @param x Number of successes
#' @param n Number of tries
#' 
#' @returns A numeric scalar.
#' 
#' @examples prop_adj(4, 60)
#' 
prop_adj <- function(x, n) {
    (x + 2) / (n + 4)
}

#' Launch bvq Shiny App in a browser
#' 
#' @source https://github.com/gongcastro/bvq-app
#' 
#' @importFrom utils browseURL
#' 
#' @details The BVQ Shiny App provides a visual interface to the bvq R package to explore the database. Its [GitHub repository](https://github.com/gongcastro/bvq-app) contains the data, documentation, and R scripts needed to run the BVQ Shiny app.
#' 
launch_app <- function() { # nocov start
    browseURL("https://gongcastro.shinyapps.io/bvq-app/", 
              browser = getOption("browser"),
              encodeIfNeeded = FALSE)
} # nocov end


