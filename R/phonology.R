#' Remove punctuation from X-SAMPA transcriptions
#'
#' @details Note that this function will effectively remove information about
#'   syllabification and stress from the phonological representations.
#'   
#' @export flatten_xsampa
#' 
#' @param x A character string with a phonological transcription in X-SAMPA format.
#' 
#' @return A character string containing a phonological transcription in X-SAMPA format in which punctuation characters
#'   have been removed.
#'   
#' @author Gonzalo Garcia-Castro
#' 
#' @md
flatten_xsampa <- function(x) {
  str_rm <- c("\\.", "\\\\", ",", "/", "?", "'", '"')
  str <- gsub(paste0(str_rm, collapse = "|"), "", x)
  str <- gsub("\\{", "\\\\{", str)
  return(str)
}

#' Syllabify phonological transcriptions in X-SAMPA formats
#'
#' @export syllabify_xsampa
#' 
#' @param x A character string with a phonological transcription in X-SAMPA.
#' @param .sep A vector of character strings indicating the characters that will be used to separate syllables. Takes `"\\."` and `"\\\""` by default.
#' 
#' @return A vector of characters in which each element is a syllable.
#' 
#' @author Gonzalo Garcia-Castro
#' 
#' @md
syllabify_xsampa <- function(x, .sep = c("\\.", "\\\"")) {
  syll <- strsplit(x, split = paste0(.sep, collapse = "|"))
  syll <- lapply(syll, function(x) x[x != ""])
  return(syll)
}

#' Get syllable structure from X-SAMPA phonological transcription
#'
#' @export syllable_str_xsampa
#' 
#' @param x A character string with a phonological transcription in X-SAMPA format.
#' @param .sep Character separating syllables in the input transcriptions.
#' 
#' @return A vector of characters in which each element is a syllable, in which vowels have been replaced with `"V"` and each consonants has been replaced with `"C"`.
#' 
#' @author Gonzalo Garcia-Castro
#' 
#' @md
syllable_str_xsampa <- function(x, .sep = c("\\.", "\\\"")) {
  syll <- syllabify_xsampa(x)

  syll <- lapply(syll, function(x) {
    phon <- strsplit(x, split = "")
    vapply(phon, function(x) {
      type <- ifelse(x %in% vowels$xsampa, "V", "C")
      paste0(unlist(type), collapse = "")
    }, character(1))
  })
  return(syll)
}
