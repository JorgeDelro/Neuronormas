# Stroop color-word interference test (word)

# Reference
# Peña-Casanova, J., Quiñones-Ubeda, S., Gramunt-Fombuena,N., et al., 2009. Neuronorma
# study team. Spanish multicenter normative studies (neuronorma project): norms for the stroop color-word 
# interference test and the tower of london-drexel. 
# Arch. Clin. Neuropsychol. 24 (4), 413-29.
# 

word <- function(score, age, education_years){
  
  word_db <- data.frame(score = score, age = age, education_years = education_years)
  word_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(word_db)) {
    res <- word_scale_score(score = word_db[i, "score"], 
                              age = word_db[i, "age"],
                              education_years = word_db[i, "education_years"])
    word_new <- rbind(word_new, res)
  }
  
  return(word_new[,c("word_scale_score", "word_percentil_range", "word_NSSae")])
}

word_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    db$word_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 131, 18, ifelse (
        
        db$score >= 127, 16, ifelse (
          db$score >= 124, 15, ifelse (
            db$score >= 120, 14, ifelse (
              db$score >= 113, 13, ifelse (
                db$score >= 108, 12, ifelse (
                  db$score >= 101, 11, ifelse (
                    db$score >= 97, 10, ifelse (
                      db$score >= 89, 9, ifelse (
                        db$score >= 80, 8, ifelse (
                          db$score >= 72, 7, ifelse (
                            db$score >= 61, 6, ifelse (
                              db$score >= 56, 5, ifelse (
                                db$score >= 44, 4, ifelse (
                                  db$score >= 25, 3, ifelse (
                                    db$score <= 24, 2, NA ))))))))))))))))) )

# percentile score

db$word_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >= 131, "> 99" , ifelse (
    
    db$score >= 127, "98" , ifelse (
      db$score >= 124, "95-97" , ifelse (
        db$score >= 120, "90-94" , ifelse (
          db$score >= 113, "82-89" , ifelse (
            db$score >= 108, "72-81" , ifelse (
              db$score >= 101, "60-71" , ifelse (
                db$score >= 97, "41-59" , ifelse (
                  db$score >= 89, "29-40" , ifelse (
                    db$score >= 80, "19-28" , ifelse (
                      db$score >= 72, "11-18" , ifelse (
                        db$score >= 61, "6-10" , ifelse (
                          db$score >= 56, "3-5" , ifelse (
                            db$score >= 44, "2" , ifelse (
                              db$score >= 25, "1" , ifelse (
                                db$score <= 24, "<1" , NA )))))))))))))))))  )
    
    
    
    
    
  }
  
  
  ##################################TABLE 4##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    
    db$word_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 131, 18, ifelse (
        
        db$score >= 127, 16, ifelse (
          db$score >= 124, 15, ifelse (
            db$score >= 120, 14, ifelse (
              db$score >= 111, 13, ifelse (
                db$score >= 106, 12, ifelse (
                  db$score >= 100, 11, ifelse (
                    db$score >= 94, 10, ifelse (
                      db$score >= 87, 9, ifelse (
                        db$score >= 80, 8, ifelse (
                          db$score >= 69, 7, ifelse (
                            db$score >= 65, 6, ifelse (
                              db$score >= 47, 5, ifelse (
                                db$score >= 44, 4, ifelse (
                                  
                                  db$score <= 43, 2, NA )))))))))))))))) )

# percentile score

db$word_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >= 131, "> 99" , ifelse (
    
    db$score >= 127, "98" , ifelse (
      db$score >= 124, "95-97" , ifelse (
        db$score >= 120, "90-94" , ifelse (
          db$score >= 111, "82-89" , ifelse (
            db$score >= 106, "72-81" , ifelse (
              db$score >= 100, "60-71" , ifelse (
                db$score >= 94, "41-59" , ifelse (
                  db$score >= 87, "29-40" , ifelse (
                    db$score >= 80, "19-28" , ifelse (
                      db$score >= 69, "11-18" , ifelse (
                        db$score >= 65, "6-10" , ifelse (
                          db$score >= 47, "3-5" , ifelse (
                            db$score >= 44, "2" , ifelse (
                              
                              db$score <= 43, "<1" , NA )))))))))))))))) )
    
    
    
    
  }
  
  
  #############################TABLE 5###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    
    db$word_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 131, 18, ifelse (
        
        db$score >= 127, 16, ifelse (
          db$score >= 124, 15, ifelse (
            db$score >= 120, 14, ifelse (
              db$score >= 111, 13, ifelse (
                db$score >= 105, 12, ifelse (
                  db$score >= 100, 11, ifelse (
                    db$score >= 91, 10, ifelse (
                      db$score >= 81, 9, ifelse (
                        db$score >= 74, 8, ifelse (
                          db$score >= 67, 7, ifelse (
                            db$score >= 56, 6, ifelse (
                              db$score >= 44, 5, ifelse (
                                
                                db$score >= 43, 3, ifelse (
                                  db$score <= 42, 2, NA )))))))))))))))) )

# percentile score

db$word_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >= 131, "> 99" , ifelse (
    
    db$score >= 127, "98" , ifelse (
      db$score >= 124, "95-97" , ifelse (
        db$score >= 120, "90-94" , ifelse (
          db$score >= 111, "82-89" , ifelse (
            db$score >= 105, "72-81" , ifelse (
              db$score >= 100, "60-71" , ifelse (
                db$score >= 91, "41-59" , ifelse (
                  db$score >= 81, "29-40" , ifelse (
                    db$score >= 74, "19-28" , ifelse (
                      db$score >= 67, "11-18" , ifelse (
                        db$score >= 56, "6-10" , ifelse (
                          db$score >= 44, "3-5" , ifelse (
                            
                            db$score >= 43, "1" , ifelse (
                              db$score <= 42, "<1" , NA )))))))))))))))) )
    
    
  }
  
  
  ###########################TABLE 6#####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    
    db$word_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 131, 18, ifelse (
        
        db$score >= 125, 16, ifelse (
          db$score >= 121, 15, ifelse (
            db$score >= 115, 14, ifelse (
              db$score >= 104, 13, ifelse (
                db$score >= 100, 12, ifelse (
                  db$score >= 98, 11, ifelse (
                    db$score >= 85, 10, ifelse (
                      db$score >= 77, 9, ifelse (
                        db$score >= 71, 8, ifelse (
                          db$score >= 61, 7, ifelse (
                            db$score >= 54, 6, ifelse (
                              db$score >= 46, 5, ifelse (
                                db$score >= 44, 4, ifelse (
                                  db$score >= 43, 3, ifelse (
                                    db$score <= 42, 2, NA ))))))))))))))))) )

# percentile score

db$word_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >= 131, "> 99" , ifelse (
    
    db$score >= 125, "98" , ifelse (
      db$score >= 121, "95-97" , ifelse (
        db$score >= 115, "90-94" , ifelse (
          db$score >= 104, "82-89" , ifelse (
            db$score >= 100, "72-81" , ifelse (
              db$score >= 98, "60-71" , ifelse (
                db$score >= 85, "41-59" , ifelse (
                  db$score >= 77, "29-40" , ifelse (
                    db$score >= 71, "19-28" , ifelse (
                      db$score >= 61, "11-18" , ifelse (
                        db$score >= 54, "6-10" , ifelse (
                          db$score >= 46, "3-5" , ifelse (
                            db$score >= 44, "2" , ifelse (
                              db$score >= 43, "1" , ifelse (
                                db$score <= 42, "<1" , NA ))))))))))))))))) )
    
  }
  
  
  ############################TABLE 7####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$word_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 131, 18, ifelse (
        
        db$score >= 125, 16, ifelse (
          db$score >= 121, 15, ifelse (
            db$score >= 116, 14, ifelse (
              db$score >= 109, 13, ifelse (
                db$score >= 99, 12, ifelse (
                  db$score >= 92, 11, ifelse (
                    db$score >= 83, 10, ifelse (
                      db$score >= 78, 9, ifelse (
                        db$score >= 72, 8, ifelse (
                          db$score >= 66, 7, ifelse (
                            db$score >= 58, 6, ifelse (
                              db$score >= 46, 5, ifelse (
                                db$score >= 43, 4, ifelse (
                                  db$score >= 31, 3, ifelse (
                                    db$score <= 30, 2, NA ))))))))))))))))) )

# percentile score

db$word_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >= 131, "> 99" , ifelse (
    
    db$score >= 125, "98" , ifelse (
      db$score >= 121, "95-97" , ifelse (
        db$score >= 116, "90-94" , ifelse (
          db$score >= 109, "82-89" , ifelse (
            db$score >= 99, "72-81" , ifelse (
              db$score >= 92, "60-71" , ifelse (
                db$score >= 83, "41-59" , ifelse (
                  db$score >= 78, "29-40" , ifelse (
                    db$score >= 72, "19-28" , ifelse (
                      db$score >= 66, "11-18" , ifelse (
                        db$score >= 58, "6-10" , ifelse (
                          db$score >= 46, "3-5" , ifelse (
                            db$score >= 43, "2" , ifelse (
                              db$score >= 31, "1" , ifelse (
                                db$score <= 30, "<1" , NA ))))))))))))))))) )
    
  }
  
  
  ##############################TABLE 8##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    db$word_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 131, 18, ifelse (
        db$score >= 121, 17, ifelse (
          db$score >= 116, 16, ifelse (
            db$score >= 113, 15, ifelse (
              db$score >= 107, 14, ifelse (
                db$score >= 99, 13, ifelse (
                  db$score >= 97, 12, ifelse (
                    db$score >= 91, 11, ifelse (
                      db$score >= 80, 10, ifelse (
                        db$score >= 74, 9, ifelse (
                          db$score >= 71, 8, ifelse (
                            db$score >= 60, 7, ifelse (
                              db$score >= 54, 6, ifelse (
                                db$score >= 33, 5, ifelse (
                                  db$score >= 31, 4, ifelse (
                                    db$score >= 24, 3, ifelse (
                                      db$score <= 23, 2, NA )))))))))))))))))) )
    
    # percentile score
    
    db$word_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 131, "> 99" , ifelse (
        db$score >= 121, "99" , ifelse (
          db$score >= 116, "98" , ifelse (
            db$score >= 113, "95-97" , ifelse (
              db$score >= 107, "90-94" , ifelse (
                db$score >= 99, "82-89" , ifelse (
                  db$score >= 97, "72-81" , ifelse (
                    db$score >= 91, "60-71" , ifelse (
                      db$score >= 80, "41-59" , ifelse (
                        db$score >= 74, "29-40" , ifelse (
                          db$score >= 71, "19-28" , ifelse (
                            db$score >= 60, "11-18" , ifelse (
                              db$score >= 54, "6-10" , ifelse (
                                db$score >= 33, "3-5" , ifelse (
                                  db$score >= 31, "2" , ifelse (
                                    db$score >= 24, "1" , ifelse (
                                      db$score <= 23, "<1" , NA )))))))))))))))))) )
    
    
  }
  
  
  ##############################TABLE 9##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    
    db$word_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 124, 18, ifelse (
        db$score >= 121 , 17, ifelse (
          db$score >= 115 , 16, ifelse (
            db$score >= 112 , 15, ifelse (
              db$score >= 105 , 14, ifelse (
                db$score >= 99 , 13, ifelse (
                  db$score >= 96 , 12, ifelse (
                    db$score >= 90 , 11, ifelse (
                      db$score >= 82 , 10, ifelse (
                        db$score >= 76 , 9, ifelse (
                          db$score >= 68 , 8, ifelse (
                            db$score >= 60 , 7, ifelse (
                              db$score >= 48 , 6, ifelse (
                                db$score >= 33 , 5, ifelse (
                                  db$score >= 31 , 4, ifelse (
                                    db$score >= 24 , 3, ifelse (
                                      db$score <=  23, 2, NA )))))))))))))))))) )
    
    # percentile score
    
    db$word_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 124, "> 99" , ifelse (
        db$score >= 121 , "99" , ifelse (
          db$score >= 115 , "98" , ifelse (
            db$score >= 112 , "95-97" , ifelse (
              db$score >= 105 , "90-94" , ifelse (
                db$score >= 99 , "82-89" , ifelse (
                  db$score >= 96 , "72-81" , ifelse (
                    db$score >= 90 , "60-71" , ifelse (
                      db$score >= 82 , "41-59" , ifelse (
                        db$score >= 76 , "29-40" , ifelse (
                          db$score >= 68 , "19-28" , ifelse (
                            db$score >= 60 , "11-18" , ifelse (
                              db$score >= 48 , "6-10" , ifelse (
                                db$score >= 33 , "3-5" , ifelse (
                                  db$score >= 31 , "2" , ifelse (
                                    db$score >= 24 , "1" , ifelse (
                                      db$score <=  23, "<1" , NA )))))))))))))))))) )
    
  }
  
  
  
  ##############################TABLE 10##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    
    db$word_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 121, 18, ifelse (
        db$score >= 120, 17, ifelse (
          db$score >= 115, 16, ifelse (
            db$score >= 110, 15, ifelse (
              db$score >= 101, 14, ifelse (
                db$score >= 97, 13, ifelse (
                  db$score >= 92, 12, ifelse (
                    db$score >= 86, 11, ifelse (
                      db$score >= 78, 10, ifelse (
                        db$score >= 71, 9, ifelse (
                          db$score >= 62, 8, ifelse (
                            db$score >= 55, 7, ifelse (
                              db$score >= 42, 6, ifelse (
                                db$score >= 31, 5, ifelse (
                                  db$score >= 24, 4, ifelse (
                                    db$score >= 23, 3, ifelse (
                                      db$score <= 22, 2, NA )))))))))))))))))) )
    
    # percentile score
    
    db$word_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 121, "> 99" , ifelse (
        db$score >= 120, "99" , ifelse (
          db$score >= 115, "98" , ifelse (
            db$score >= 110, "95-97" , ifelse (
              db$score >= 101, "90-94" , ifelse (
                db$score >= 97, "82-89" , ifelse (
                  db$score >= 92, "72-81" , ifelse (
                    db$score >= 86, "60-71" , ifelse (
                      db$score >= 78, "41-59" , ifelse (
                        db$score >= 71, "29-40" , ifelse (
                          db$score >= 62, "19-28" , ifelse (
                            db$score >= 55, "11-18" , ifelse (
                              db$score >= 42, "6-10" , ifelse (
                                db$score >= 31, "3-5" , ifelse (
                                  db$score >= 24, "2" , ifelse (
                                    db$score >= 23, "1" , ifelse (
                                      db$score <= 22, "<1" , NA )))))))))))))))))) )
    
    
  }
  
  
  ##############################TABLE 11##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    
    db$word_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 120, 18, ifelse (
        
        db$score >= 115, 16, ifelse (
          db$score >= 105, 15, ifelse (
            db$score >= 101, 14, ifelse (
              db$score >= 98, 13, ifelse (
                db$score >= 90, 12, ifelse (
                  db$score >= 84, 11, ifelse (
                    db$score >= 76, 10, ifelse (
                      db$score >= 68, 9, ifelse (
                        db$score >= 56, 8, ifelse (
                          db$score >= 51, 7, ifelse (
                            db$score >= 42, 6, ifelse (
                              db$score >= 33, 5, ifelse (
                                
                                db$score >= 32, 3, ifelse (
                                  db$score <= 31, 2, NA )))))))))))))))) )

# percentile score

db$word_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >= 120, "> 99" , ifelse (
    
    db$score >= 115, "98" , ifelse (
      db$score >= 105, "95-97" , ifelse (
        db$score >= 101, "90-94" , ifelse (
          db$score >= 98, "82-89" , ifelse (
            db$score >= 90, "72-81" , ifelse (
              db$score >= 84, "60-71" , ifelse (
                db$score >= 76, "41-59" , ifelse (
                  db$score >= 68, "29-40" , ifelse (
                    db$score >= 56, "19-28" , ifelse (
                      db$score >= 51, "11-18" , ifelse (
                        db$score >= 42, "6-10" , ifelse (
                          db$score >= 33, "3-5" , ifelse (
                            
                            db$score >= 32, "1" , ifelse (
                              db$score <= 31, "<1" , NA )))))))))))))))) )
    
  }
  
  
  #############################TABLE 12###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    
    db$word_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 120, 18, ifelse (
        
        db$score >= 115, 16, ifelse (
          db$score >= 105, 15, ifelse (
            db$score >= 100, 14, ifelse (
              db$score >= 96, 13, ifelse (
                db$score >= 89, 12, ifelse (
                  db$score >= 84, 11, ifelse (
                    db$score >= 76, 10, ifelse (
                      db$score >= 66, 9, ifelse (
                        db$score >= 55, 8, ifelse (
                          db$score >= 51, 7, ifelse (
                            db$score >= 42, 6, ifelse (
                              db$score >= 22, 5, ifelse (
                                db$score >= 21, 4, ifelse (
                                  
                                  db$score <= 20, 2, NA )))))))))))))))) )

# percentile score

db$word_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >= 120, "> 99" , ifelse (
    
    db$score >= 115, "98" , ifelse (
      db$score >= 105, "95-97" , ifelse (
        db$score >= 100, "90-94" , ifelse (
          db$score >= 96, "82-89" , ifelse (
            db$score >= 89, "72-81" , ifelse (
              db$score >= 84, "60-71" , ifelse (
                db$score >= 76, "41-59" , ifelse (
                  db$score >= 66, "29-40" , ifelse (
                    db$score >= 55, "19-28" , ifelse (
                      db$score >= 51, "11-18" , ifelse (
                        db$score >= 42, "6-10" , ifelse (
                          db$score >= 22, "3-5" , ifelse (
                            db$score >= 21, "2" , ifelse (
                              
                              db$score <= 20, "<1" , NA )))))))))))))))) )
    
  }
  
  
  # Educational level adjust 
  db$word_education_years_adj <- with(db, ifelse(
    is.na(db$word_scale_score), NA, ifelse (
    db$education_years >= 0  & db$education_years < 1, db$word_scale_score + 3, ifelse(
      db$education_years >= 1  & db$education_years <= 4, db$word_scale_score + 2, ifelse(
        db$education_years >= 5  & db$education_years <= 8, db$word_scale_score + 1, ifelse(
          db$education_years >= 9  & db$education_years <= 12, db$word_scale_score, ifelse(
            db$education_years >= 13  & db$education_years <= 15, db$word_scale_score - 1, ifelse(
              db$education_years >= 16  & db$education_years <= 19, db$word_scale_score - 2, ifelse(
                db$education_years > 19  & db$education_years <= 20, db$word_scale_score- 3, ifelse(
            ))))))))) )
  
  
  # NSSae
  db$word_NSSae <- with(db, ifelse(
    is.na(db$word_education_years_adj), NA, ifelse (
      !is.na(db$word_education_years_adj), db$word_scale_score - (0.25663*(db$word_education_years_adj-12)) )))
  
  return(db)
}



