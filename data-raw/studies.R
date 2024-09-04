## code to prepare `studies` dataset goes here

# uni lemmas
studies <- read.delim(
  system.file(file.path("extdata", "studies.csv"),
    package = "bvq"
  ),
  sep = ",",
  quote = "",
  na.strings = c("", "NA")
)

# export pool database
usethis::use_data(studies, languages_lockdown1, languages_lockdown2, languages1, bvq_000, overwrite = TRUE, internal = TRUE)
