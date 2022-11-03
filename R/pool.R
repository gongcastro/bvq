#' Pool of words
#'
#' A dataset containing candidate words to be included in the questionnaires
#' with some lexical properties. Transcriptions were (a) generated manually, (b) retrieved from \href{https://www.wiktionary.org/}{Wiktionary}. All transcriptions have been manually double-checked and fixed if necessary.
#'
#' @format A data frame with 1601 rows and 20 variables:
#' \describe{
#'   \item{item}{item label, as indicated in the formr survey spreadsheets, items are unique within and across questionnaires}
#'   \item{te}{index associated to translation equivalents across languages}
#'   \item{language}{language the item belongs to}
#'   \item{category}{semantic/functional category the items belongs to}
#'   \item{class}{Funcional category (verb, nouns, adjective, etc.)}
#'   \item{label}{item label, as presented to participants in the front-end of the questionnaire, some labels are not unique within or across quesitonnaires}
#'   \item{ipa}{phonological transcription in IPA format, extracted from \href{https://www.wiktionary.org/}{Wiktionary} or manually coded if not available}
#'   \item{ipa}{phonological transcription in C-SAMPA format, transcribed from \code{ipa} using the \href{https://clearpond.northwestern.edu/ipa_cpsampa.html}{CLEARPOND transcriber}}
#'   \item{subtlex_lemma}{word label, as included in the corresponding version of SUBTLEX}
#'   \item{wordbank_lemma}{word label, as indexed in Worbank}
#'   \item{include}{should this item be included in analyses?}
#'   \item{version}{what short version of the questionnaire does this item appear on?}
#'   \item{comments}{additional comments to the item}
#' }
"pool"
