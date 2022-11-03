library(dplyr)
library(readxl)

# uni lemmas
worbank_lemmas <- read.delim("inst/extdata/lemmas.txt", sep = "\t") %>% 
    select(te, item, wordbank_lemma) 

# import pool
pool <- read_xlsx("inst/extdata/pool.xlsx") %>% 
    left_join(worbank_lemmas) %>% 
    select(item, te, label, ipa, subtlex_lemma, wordbank_lemma, version, include) 

# export pool database
usethis::use_data(pool, overwrite = TRUE)
