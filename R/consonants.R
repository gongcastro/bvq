#' Inventory and classification of consonants in X-SAMPA and IPA formats.
#'
#' A dataset containing most consonant phonemes identified by the [International Phonetic Association](https://en.wikipedia.org/wiki/International_Phonetic_Association) (IPA). Phonemes are classified across three dimensions: place of articulation, manner of articulation, and voicing. Each phoneme is assigned a symbol in X-SAMPA and IPA format.
#' @source https://en.wikipedia.org/wiki/X-SAMPA
#' @format A data frame with 65 rows and 5 variables:
#' * xsampa: phoneme symbol in [X-SAMPA](https://en.wikipedia.org/wiki/X-SAMPA) format
#' * ipa: phoneme symbol in [IPA](https://en.wikipedia.org/wiki/International_Phonetic_Alphabet) format
#' * place: place of articulation (broad classification): `"Coronal"`, `"Dorsal"`, `"Labial"`, or `"Pharyngeal"`. "Location along the vocal tract where its production occurs.[1]: 10  It is a point where a constriction is made between an active and a passive articulator."^[https://en.wikipedia.org/wiki/Place_of_articulation#:~:text=In%20articulatory%20phonetics%2C%20the%20place,active%20and%20a%20passive%20articulator.]
#' * place_2: place of articulation (fine classification): `"Nasal"`, `"Plosive"`, `"Fricative"`, `"Approximant"`, `"Trill"`, `"Flap"`, `"Lateral Fricative"`, `"Lateral Approximant"`, or `"Lateral Flat"`
#' * manner: manner of articulation: `"Bilabial"`, `"Labio-dental"`, `"Dental"`, `"Alveolar"`, `"Post-alveolar"`, `"Retroflex"`, `"Palatal"`, `"Velar"`, `"Uvular"`, `"Epiglotal"`, `"Glotal"`. "Configuration and interaction of the articulators (speech organs such as the tongue, lips, and palate) when making a speech sound."^[https://en.wikipedia.org/wiki/Manner_of_articulation]
#' * voicing: `"Voiced"`, `"Voiceless"`. "Classification of speech sounds that tend to be associated with vocal cord vibration but may not actually be voiced at the articulatory level."^[https://en.wikipedia.org/wiki/Voice_(phonetics)]
"consonants"
