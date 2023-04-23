## code to prepare `phonemes` dataset goes here
library(dplyr)
library(readxl)

# consonants
consonants <- read_xlsx(system.file("extdata/consonants.xlsx", 
                                    package = "bvq")) %>% 
    rename(xsampa = phoneme) %>% 
    mutate(across(-xsampa, as.factor),
           across(c(place, place_2, manner, voicing), 
                  function(x) strsplit(as.character(x), ", "))) %>% 
    relocate(xsampa, place, place_2, manner, voicing)

# export for future testing
saveRDS(consonants, test_path("fixtures", "consonants.rds"))

# export database
usethis::use_data(consonants, overwrite = TRUE, internal = FALSE)
