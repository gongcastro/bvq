#' Age difference in time
#' 
#' Returns the absolute difference in time elapsed between two dates, as indicated by [lubridate::time_length()].
#'
#' @param x Most recent date
#' @param y Least recent date
#' @param units Time units in which the time difference should be returned. Passed to [lubridate::time_length].
#' @importFrom lubridate time_length
#' @returns Absolute difference in months elapsed between `x` and `y`
#' @author Gonzalo Garcia-Castro
#' @export diff_in_time
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
#' @export
#' 
#' @examples
#' library(dplyr)
#' x <- data.frame(start = as.Date(c("2023-02-01", "2023-02-20")),
#'                 finish = as.Date(c("2023-03-15", "2023-02-21")))
#' 
#' y <- mutate(x, 
#'             time_stamp_default = get_time_stamp(start, finish), 
#'             time_stamp_first = get_time_stamp(start, finish, which = "first"), 
#'             time_stamp_last = get_time_stamp(start, finish, which = "last"))
#' (y)
#' 
get_time_stamp <- function(..., which = "first") {
    
    if (!(which=="first" || which=="last")) {
        cli_abort("`which` must be 'first' or 'last'")
    }
    
    if (which=="first") fun <- min else fun <- max
    x <- as_datetime(apply(across(c(...)), 1, fun, na.rm = TRUE))
    
    return(x)
}

#' Summarise language profile
#'
#' @param ... Columns with the degree of exposures to be summed up
#'   for (all others will be considered as `doe_others`).
#' @author Gonzalo Garcia-Castro
#' 
#' @returns A numeric vector with the row-wise sums of the columns specified in `...`. 
#' 
#' @importFrom dplyr across
#' @export
#' 
#' @examples
#' library(dplyr)
#' x <- data.frame(doe_cat_1 = seq(0, 1, 0.1),
#'                 doe_cat_2 = c(0, rep(c(0.1, 0), each = 5)),
#'                 doe_spa_1 = c(0, rep(c(0.1, 0), each = 5)),
#'                 doe_spa_2 = c(1, 0.7, 0.6, 0.5, 0.3, 0.1, 0.4, 0.3, 0.2, 0.1, 0))
#'                 
#' y <- mutate(x,
#'             doe_other = 1-get_doe(matches("cat|spa")),
#'             doe_cat = get_doe(doe_cat_1, doe_cat_2),
#'             doe_spa = get_doe(matches("spa")))
#' 
#' (y)
get_doe <- function(...) {
    rowSums(across(c(...)), dims = 1, na.rm = TRUE)
}

#' Fix codes
#' 
#' @param x Vector of `code` whose values should be fixed.
#' @author Gonzalo Garcia-Castro
fix_code <- function(x) { # nocov start
    x <- toupper(trimws(x))
    x <- gsub("O", "0", x)
    x <- gsub("I", "L", x)
    x <- gsub("[^\\d]+", "", x, perl=TRUE)
    x <- paste0("BL", x)
    
    return(x)
} # nocov end


#' Fix raw codes
#'
#' @param x Vector of `code` whose values should be fixed, based on `session`.
#' @author Gonzalo Garcia-Castro
fix_code_raw <- function(x) { # nocov start
    x[x$session == "-OYU0wA9FPQ9-ugKUpyrz1A0usJZIuM5hb-cbV2yMgGBal5S9q3ReRgphBDDxFEY", "code"] <- "BL1674"
    x[x$session == "ZZiRT3JN4AdKnXMxjEMtU3CzRkniH0hOSZzS-0kzquRt_Ls9PJzmKsY3qm8tQ7Z2", "code"] <- "BL1671"
    x[x$session == "TW8vSEn7YTtbZoe9BaEtRgwNvryWTwSv49dLKb5W0_6bFL306Eiw0Ehg72Q9nqLx", "code"] <- "BL1672"
    x[x$session == "DDjiYrPl-WD951rocaSKH9grkn2T4ZJKjhdCAPDzqNBWyhc8E8wwjOY0CcruNn1m", "code"] <- "BL1673"
    x[x$session == "c9fPw4Lbm5WS0AbBRppv4NVHh4eETxvEitH8lUC1pkt2ulxxHCvXgAYopCGRQSa_", "code"] <- "BL1569"
    x[x$session == "I8ldNrILmQd7Vhtwqk99Y9YaKWrZzlExKeXsRv9_toIEi43BtlfuLI-PmdU4iY7G", "code"] <- "BL1788"
    x[x$session == "dU5CZLLkvmY7SDpe8d0jFQO3xzpmeS0lCOFF_ErjETC1tyyYbv3ZhyaDmlfdJwHc", "code"] <- "BL1876"
    x[x$session == "L4F1jd13H4wyFf6QYGy8hfSURneFr-zfzMn1YFFeBTbTZWWjxYPRbC-rPY6U1qdr", "code"] <- "remove"
    return(x)
} # nocov end

#' Fix DOEs
#' 
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @param x Vector of `doe` whose values should be fixed
#' @author Gonzalo Garcia-Castro
fix_doe <- function(x) { # nocov start
    
    x %>%
        mutate(
            doe_catalan = case_when(
                id == "54469" & time == 2 ~ 0,
                id == "57157" & time == 1 ~ 80,
                id == "57046" & time == 1 ~ 50,
                code == "BL1582" ~ 30,
                code == "BL1295" ~ 10,
                code == "BL1252" ~ 90,
                .default = doe_catalan
            ),
            doe_spanish = case_when(
                id == "57046" & time == 1 ~ 50,
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
} # nocov end

#' Fix sex (missing in first responses to BL-Lockdown)
#' 
#' @param x Vector of `sex` whose values should be fixed.
#' @author Gonzalo Garcia-Castro
fix_sex <- function(x) { # nocov start
    
    x$sex <- ifelse(x$id_bvq %in% c("bilexicon_1097", 
                                    "bilexicon_1441", 
                                    "bilexicon_1124",
                                    "bilexicon_1448"),
                    "Female",
                    x$sex)
    
    x$sex <- ifelse(x$id_bvq=="bilexicon_1447", "Male", x$sex)
    
    return(x)
} # nocov end


#' Fix item
#' 
#' @importFrom dplyr case_when
#' @param x Vector of `item` whose values should be fixed
fix_item <- function(x) { # nocov start
    
    x$item[x$item=="cat_parc"]            <- "cat_parc1"
    x$item[x$item=="cat_eciam"]           <- "cat_enciam"
    x$item[x$item=="cat_voler3"]          <- "cat_voler2"
    x$item[x$item=="cat_voler"]           <- "cat_voler1"
    x$item[x$item=="cat_despres1"]        <- "cat_despres"
    x$item[x$item=="cat_peix"]            <- "cat_peix1"
    x$item[x$item=="cat_estar"]           <- "cat_estar1"
    x$item[x$item=="cat_querer"]          <- "cat_querer1"
    x$item[x$item=="cat_estiguestequiet"] <- "cat_estiguesquiet"
    x$item[x$item=="spa_nibla"]           <- "spa_niebla"
    x$item[x$item=="spa_ir"]              <- "spa_ir1"
    x$item[x$item=="spa_querer"]          <- "spa_querer1"
    x$item[x$item=="cat_anar"]            <- "cat_anar1"
    
    return(x)
} # nocov end


#' Fix id_exp
#' 
#' @param x Vector of `id_exp` whose values should be fixed
#' @author Gonzalo Garcia-Castro
fix_id_exp <- function(x) 
{ # nocov start
    x$id_exp <- ifelse(x$code %in% "BL547", "bilexicon_189", x$id_exp)
    return(x)
} # nocov end

#' Deal with repeated measures
#'
#' @export get_longitudinal
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
#' @importFrom dplyr distinct
#' @importFrom dplyr n
#' @importFrom dplyr filter
#' @returns A subset of the data frame `x` with only the selected cases,
#'   according to `longitudinal`.
#' @author Gonzalo Garcia-Castro
#' @examples 
#' id <- c(1, 1, 1, 2, 2, 3, 4, 4, 4, 4, 5, 6, 7, 7, 8, 9, 10, 10)
#' sums <- rle(sort(id))[["lengths"]]
#' dat <- data.frame(id, time = unlist(sapply(sums, function(x) seq(1, x))))
#' 
#' (dat)
#' 
#' get_longitudinal(dat, "first")
#' get_longitudinal(dat, "only")
get_longitudinal <- function(x, longitudinal = "all")
{
    
    longitudinal_opts <- c("all", "no", "first", "last", "only")
    
    if (!(longitudinal %in% longitudinal_opts) && interactive()) {
        long_colapsed <- paste0(longitudinal_opts, collapse = ", ")
        cli_abort(paste0("longitudinal must be one of: ", long_colapsed))
    }
    
    repeated <- filter(distinct(x, id, time), n() > 1, .by = id) 
    
    if (longitudinal == "no") x <- filter(x, !(id %in% repeated$id))
    if (longitudinal == "first") x <- filter(x, time==min(time), .by = id)
    if (longitudinal == "last") x <- filter(x, time==max(time), .by = id) 
    if (longitudinal == "only") x <- filter(x, id %in% repeated$id)
    
    return(x)
}




