## code to prepare `phonemes` dataset goes here
library(dplyr)
library(ipa)
library(readxl)

# consonants
consonants <- read_xlsx(system.file("extdata/consonants.xlsx", 
                                    package = "bvqdev")) %>% 
    rename(xsampa = phoneme) %>% 
    mutate(across(-xsampa, as.factor),
           across(c(place, place_2, manner, voicing), 
                  function(x) strsplit(as.character(x), ", ")),
           ipa = xsampa(xsampa)) %>% 
    relocate(xsampa, ipa, place, place_2, manner, voicing)

# export pool database
usethis::use_data(consonants, overwrite = TRUE)
