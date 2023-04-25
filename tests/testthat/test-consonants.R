test_that("consonants columns are the right classes", {
    pool <- readRDS(test_path("fixtures", "consonants.rds"))
    
    expect_type(consonants$xsampa, "character")
    expect_type(consonants$place, "list")
    expect_type(consonants$place_2, "list")
    expect_type(consonants$manner, "list")
    expect_type(consonants$voicing, "list")
    
    
})

test_that("consonants column values are right", {
    consonants <- readRDS(test_path("fixtures", "consonants.rds"))
    
    consonants_vct <- c("m", "F", "n", "n`", "J", "N", "N\\", "p", "b", "p_d",
                        "b_d", "t", "d", "t`", "d`", "c", "J\\", "k", "g", "q",   
                        "G\\", ">\\", "?", "p\\", "B", "f", "v", "T", "D", "s",
                        "z", "S", "Z", "s`", "z`", "C", "j\\", "x", "G", "X",
                        "R", "X\\", "?\\", "H\\", "<\\", "h", "h\\", "B_o", "v\\", "r\\", 
                        "r\\`", "j", "M\\", "B\\", "r", "R\\", "4", "r`", "K", "K\\", 
                        "l", "l`", "L", "L\\", "l\\")
    
    place_vct <- c("Labial", "Coronal", "Dorsal", "Laryngeal")
    
    place_2_vct <- c("Nasal", "Plosive", "Fricative",          
                     "Approximant", "Trill", "Flap",               
                     "Lateral Fricative", "Lateral Approximant", "Lateral Flap")
    
    manner_vct <- c("Bilabial", "Labio-dental", "Dental", "Alveolar",     
                    "Post-alveolar", "Retroflex", "Palatal", "Velar",        
                    "Uvular", "Epiglotal", "Glotal", "Pharyngeal" )
    
    voicing_vct <- c("Voiced", "Voiceless", NA)
    
    expect_true(all(unique(unlist(consonants$xsampa)) %in% consonants_vct))
    expect_true(all(unique(unlist(consonants$place)) %in% place_vct))
    expect_true(all(unique(unlist(consonants$place_2)) %in% place_2_vct))
    expect_true(all(unique(unlist(consonants$manner)) %in% manner_vct))
    expect_true(all(unique(unlist(consonants$voicing)) %in% voicing_vct))
    
})

test_that("consonants are not duplicated", {
    pool <- readRDS(test_path("fixtures", "consonants.rds"))
    
    expect_false(any(duplicated(consonants$xsampa)))
    
})