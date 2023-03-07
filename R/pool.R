#' Pool of words
#'
#' A dataset containing candidate words to be included in the questionnaires
#' with some lexical properties. Transcriptions were (a) generated manually, (b)
#' retrieved from [Wiktionary](https://www.wiktionary.org/). All transcriptions
#' have been manually double-checked and fixed if necessary.
#'
#' @format A data frame with 1601 rows and 20 variables:
#' * item: item label, as indicated in the formr survey spreadsheets, items are unique within and across questionnaires.
#' * language: language the item belongs to.
#' * te: index associated to translation equivalents across languages.
#' * label: item label, as presented to participants in the front-end of the questionnaire, some labels are not unique within or across questionnaires.
#' * ipa: phonological transcription in IPA format, extracted from [Wiktionary](https://www.wiktionary.org/) or manually coded if not available.
#' * sampa: phonological transcription in C-SAMPA format, transcribed from `ipa` using the [CLEARPOND transcriber](https://clearpond.northwestern.edu/ipa_cpsampa.html).
#' * n_lemmas: an integer indicating the number of different lemmas showed in the item label to participants. for instance, the Spanish item `"spa_hierba"` was shown to in the questionnaire as `"hierba / césped"`. Lemma with similar roots were considered as one, such as the Spanish item `"spa_tonto"`, presented as `"tonto / tonta"` in the questionnaire.
#' * is_multiword: an logical indicating  whether the item included a multi-word phrase as presented in the questionnaire. For instance the Spanish item `"spa_cepillodientes"` was shown as `"cepillo de dientes"` in the questionnaire, which includes three words.
#' * subtlex_lemma: word label, as included in the corresponding version.
#'   of SUBTLEX.
#' * wordbank_lemma: word label, as indexed in Worbank.
#' * childes_lemma: word label, as it appears in the CHILDES English
#'   corpora (based on `wordbank_lemma`).
#' * semantic_category: semantic/functional category the items belongs to.
#' * class: Functional category (verb, nouns, adjective, etc.).
#' * version: what short version of the questionnaire does this item appear on?
#' * include: should this item be included in analyses?
"pool"
