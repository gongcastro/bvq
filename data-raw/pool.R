library(dplyr)
library(readxl)

# uni lemmas
worbank_lemmas <- read.delim("inst/extdata/lemmas.txt", sep = "\t") %>% 
    select(te, item, ends_with("lemma")) 

# import pool
pool <- read_xlsx("inst/extdata/pool.xlsx") %>% 
    left_join(worbank_lemmas) %>% 
    mutate(language = ifelse(grepl("cat_", item), "Catalan", "Spanish")) %>% 
    select(item, language, te, label, ipa, sampa, subtlex_lemma, wordbank_lemma, childes_lemma, category, class, version, include) %>% 
    mutate(
        te = as.integer(te),
        include = as.logical(include),
        version = strsplit(version, split = ",")
    )

# export pool database
usethis::use_data(pool, overwrite = TRUE)
