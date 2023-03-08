#' Remove punctuation and fix non-ASCII characters from IPA transcriptions
#'
#' @details Note that this function will effectively remove information about
#'   syllabification and stress from the phonological representations.
#'   
#' @export flatten_ipa
#' 
#' @param x A character vector with at least one element that contains
#'   phonological transcriptions in International Phonology Association (IPA)
#'   format. These character strings may contain non-ASCII characters that make
#'   certain operations daunting, such as computing edit distances between
#'   transcriptions.
#'   
#' @return A character vector of the same length in which punctuation characters
#'   have been removed and non-ASCII characters have been replaced by
#'   computer-friendly ones.
flatten_ipa <- function(x) {
    unique_phonemes <- unique(unlist(strsplit(paste(x, collapse = ""), "")))
    shortlisted_phonemes <- paste0("\\", unique_phonemes[c(3, 6, 37, 39, 44, 50)] , collapse = "|")
    gsub(shortlisted_phonemes, "", x)
}

#' Remove punctuation from SAMPA transcriptions
#'
#' @details Note that this function will effectively remove information about
#'   phoneme clustering.
#'   
#' @export flatten_sampa
#' 
#' @param x A character vector with at least one element that contains
#'   phonological transcriptions in Speech Assessment Methods Phonetic Alphabet
#'   (X-SAMPA) format.
#'   
#' @return A character vector of the same length in which punctuation characters
#'   have been removed.
flatten_sampa <- function(x) {
    gsub("[[:punct:]]", "", x)
}
