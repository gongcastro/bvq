#' Remove punctuation and fix non-ASCII characters from IPA transcriptions
#'
#' @details Note that this function will effectively remove information about
#'   syllabification and stress from the phonological representations.
#' @export flatten_ipa
#' @param x A character string with a phonological transcription in IPA format.
#' @return A character string containing a phonological transcription in IPA format in which punctuation characters
#'   have been removed.
#' @author Gonzalo Garcia-Castro
#' @seealso [bvqdev::flatten_xsampa()], [ipa::ipa()]
flatten_ipa <- function(x) {
    unique_phonemes <- unique(unlist(strsplit(paste(x, collapse = ""), "")))
    shortlisted_phonemes <- paste0("\\", 
                                   unique_phonemes[c(3, 6, 37, 39, 44, 50)] ,
                                   collapse = "|")
    gsub(shortlisted_phonemes, "", x)
}

#' Remove punctuation from SAMPA transcriptions
#'
#' @details Note that this function will effectively remove information about
#'   syllabification and stress from the phonological representations.
#' @export flatten_xsampa
#' @param x A character string with a phonological transcription in X-SAMPA format.
#' @return A character string containing a phonological transcription in X-SAMPA format in which punctuation characters
#'   have been removed.
#' @author Gonzalo Garcia-Castro
#' @seealso [bvqdev::flatten_ipa()], [ipa::xsampa()]
#' @md
flatten_xsampa <- function(x) {
    str_rm <- c("\\.", "\\\\", ",", "/", "?", "¿", "'", '"', "ˈ")
    str <- gsub(paste0(str_rm, collapse = "|"), "", x)
    str <- gsub("\\{", "\\\\{", str)
    return(str)
}

#' Syllabify phonological transcriptions in X-SAMPA formats
#'
#' @export syllabify_xsampa
#' @param x A character string with a phonological transcription in X-SAMPA.
#' @param .sep A vector of character strings indicating the characters that will be used to separate syllables. Takes `"\\."` and `"\\\""` by default.
#' @return A vector of characters in which each element is a syllable.
#' @author Gonzalo Garcia-Castro
#' @seealso [bvqdev::syllabify_ipa()], [ipa::ipa()], [ipa::xsampa()], [bvqdev::flatten_xsampa()]
#' @md
syllabify_xsampa <- function(x, .sep = c("\\.", "\\\"")) {
    syll <- strsplit(x, split = paste0(.sep, collapse = "|"))
    syll <- lapply(syll, function(x) x[x != ""]) 
    return(syll)
}

#' Syllabify phonological transcriptions in IPA or X-SAMPA formats
#'
#' @export syllabify_ipa
#' @param x A character string with a phonological transcription in IPA format.
#' @param .sep A vector of character strings indicating the characters that will be used to separate syllables. Takes `"\\."`, `"'"` and `"ˈ"` by default.
#' @return A vector of characters in which each element is a syllable.
#' @seealso [bvqdev::syllabify_xsampa()], [ipa::xsampa()], [ipa::ipa()], [bvqdev::flatten_ipa()]
#' @author Gonzalo Garcia-Castro
#' @md
syllabify_ipa <- function(x, .sep = c("\\.", "'", "ˈ")) {
    syll <- strsplit(x, split = paste0(.sep, collapse = "|"))
    syll <- lapply(syll, function(x) x[x != ""]) 
    return(syll)
}

#' Get syllable structure from X-SAMPA phonological transcription
#'
#' @export syllable_str_xsampa
#' @param x A character string with a phonological transcription in X-SAMPA format.
#' @param .sep Character separating syllables in the input transcriptions.
#' @return A vector of characters in which each element is a syllable, in which vowels have been replaced with `"V"` and each consonants has been replaced with `"C"`. 
#' @seealso [bvqdev::syllabify_xsampa()], [ipa::xsampa()], [ipa::ipa()]
#' @author Gonzalo Garcia-Castro
#' @md
syllable_str_xsampa <- function(x, .sep = c("\\.", "\\\""))
{
    syll <- syllabify_xsampa(x)
    syll <- map(syll, function(x) {
        phon <- strsplit(x, split = "")
        map_chr(phon, function(x) {
            type <- ifelse(x %in% vowels$xsampa, "V", "C")
            paste0(unlist(type), collapse = "")
        })
    })
    return(syll)
}

#' Get syllable structure from IPA phonological transcription
#'
#' @export syllable_str_ipa
#' @param x A character string with a phonological transcription in X-SAMPA format.
#' @param .sep Character separating syllables in the input transcriptions.
#' @return A vector of characters in which each element is a syllable, in which vowels have been replaced with `"V"` and each consonants has been replaced with `"C"`. 
#' @seealso [bvqdev::syllabify_ipa()], [ipa::ipa()], [ipa::xsampa()]
#' @author Gonzalo Garcia-Castro
#' @md
syllable_str_ipa <- function(x, .sep = c("\\.", "\\\""))
{
    syll <- syllabify_ipa(x)
    syll <- map(syll, function(x) {
        phon <- strsplit(x, split = "")
        map_chr(phon, function(x) {
            type <- ifelse(x %in% vowels$ipa, "V", "C")
            paste0(unlist(type), collapse = "")
        })
    })
    return(syll)
}

#' Check that all characters included in X-SAMPA phonological transcriptions are part of the X-SAMPA alphabet
#'
#' @export check_xsampa
#' @importFrom cli cli_abort
#' @import ipa
#' @param x A vector of character strings with at least one element that contains
#'   phonological transcriptions in X-SAMPA format.
#' @return A logical (invisible) value indicating whether all symbols in `x` are part of the X-SAMPA alphabet.
#' @seealso [bvqdev::syllabify_xsampa()], [ipa::xsampa()], [flatten_xsampa()]
#' @author Gonzalo Garcia-Castro
#' @md
check_xsampa <- function(x) {
    
    str <- flatten_xsampa(x)
    str <- unique(unlist(strsplit(str, "")))
    
    phonemes_df <- get("phonemes",
                       envir = asNamespace("ipa"),
                       inherits = FALSE)
    
    is_xsampa <- str %in% phonemes_df$xsampa
    if (!all(is_xsampa)) {
        which_not <- str[which(!is_xsampa)]
        cli_abort("Character{?s} {which_not} {?is/are} not a X-SAMPA symbol{?s}")
    } else {
        return(invisible(TRUE))
    }
    
}


