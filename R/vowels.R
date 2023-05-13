#' Inventory and classification of vowels in X-SAMPA format.
#'
#' A dataset containing most vowel phonemes identified by the [International Phonetic Association](https://en.wikipedia.org/wiki/International_Phonetic_Association) (IPA). Phonemes are classified across three dimensions: place of articulation, manner of articulation, and voicing. Each phoneme is assigned a symbol in X-SAMPA format.
#' @source https://en.wikipedia.org/wiki/X-SAMPA
#' @format A data frame with 34 rows and 4 variables:
#' * xsampa: phoneme symbol in [X-SAMPA](https://en.wikipedia.org/wiki/X-SAMPA) format
#' * openness: `"Close"`, `"Near-close"`, `"Near-mid"`, `"Close-mid"`, `"Mid"`, `"Open-mid"`, `"Near-open"`, or `"Open"`. "An open vowel is a vowel sound in which the tongue is positioned as far as possible from the roof of the mouth. Open vowels are sometimes also called low vowels (in U.S. terminology) in reference to the low position of the tongue.
#' * frontness: `"Back`, `"Central`, or `"Front`. "A front vowel is a class of vowel sounds used in some spoken languages, its defining characteristic being that the highest point of the tongue is positioned as far forward as possible in the mouth without creating a constriction that would otherwise make it a consonant. Front vowels are sometimes also called bright vowels because they are perceived as sounding brighter than the back vowels.
"vowels"
