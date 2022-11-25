library(dplyr)
library(readxl)

# uni lemmas
worbank_lemmas <- read.delim("inst/extdata/lemmas.txt", sep = "\t") %>% 
    select(te, item, ends_with("lemma")) 

# import pool
pool <- read_xlsx("inst/extdata/pool.xlsx") %>% 
    left_join(worbank_lemmas) %>% 
    mutate(language = ifelse(grepl("cat_", item), "Catalan", "Spanish")) %>% 
    select(item, language, te, label, ipa, sampa, 
           n_lemmas, is_multiword, subtlex_lemma, wordbank_lemma, 
           childes_lemma, semantic_category, class, version, include) %>% 
    drop_na(version) %>% 
    mutate(
        across(c(te, n_lemmas), as.integer),
        across(c(is_multiword, include),as.logical),
        version = strsplit(version, split = ",")
    ) 

# export pool database
usethis::use_data(pool, overwrite = TRUE)
