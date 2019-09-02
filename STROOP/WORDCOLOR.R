# Stroop color-word interference test (interference)

# Reference
# Peña-Casanova, J., Quiñones-Ubeda, S., Gramunt-Fombuena,N., et al., 2009. Neuronorma
# study team. Spanish multicenter normative studies (neuronorma project): norms for the stroop color-word 
# interference test and the tower of london-drexel. 
# Arch. Clin. Neuropsychol. 24 (4), 413-29.
# 

wordcolor <- function(score, age, education_years){
  
  wordcolor_db <- data.frame(score = score, age = age, education_years = education_years)
  wordcolor_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(wordcolor_db)) {
    res <- wordcolor_scale_score(score = wordcolor_db[i, "score"], 
                              age = wordcolor_db[i, "age"],
                              education_years = wordcolor_db[i, "education_years"])
    wordcolor_new <- rbind(wordcolor_new, res)
  }
  
  return(wordcolor_new)
}

wordcolor_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    db$wordcolor_scale_score <- with ( db, ifelse (
      db$score >= 73, 18, ifelse (
        db$score >= 67, 17, ifelse (
          db$score >= 57, 16, ifelse (
            db$score >= 55, 15, ifelse (
              db$score >= 51, 14, ifelse (
                db$score >= 47, 13, ifelse (
                  db$score >= 42, 12, ifelse (
                    db$score >= 40, 11, ifelse (
                      db$score >= 34, 10, ifelse (
                        db$score >= 32, 9, ifelse (
                          db$score >= 26, 8, ifelse (
                            db$score >= 22, 7, ifelse (
                              db$score >= 20, 6, ifelse (
                                db$score >= 12, 5, ifelse (
                                  db$score >= 8, 4, ifelse (
                                    db$score >= 7, 3, ifelse (
                                      db$score <= 6, 2, NA ))))))))))))))))))
    
    # percentile score
    
    db$wordcolor_percentil_range <- with (db, ifelse (
      db$score >= 73, "> 99" , ifelse (
        db$score >= 67, "99" , ifelse (
          db$score >= 57, "98" , ifelse (
            db$score >= 55, "95-97" , ifelse (
              db$score >= 51, "90-94" , ifelse (
                db$score >= 47, "82-89" , ifelse (
                  db$score >= 42, "72-81" , ifelse (
                    db$score >= 40, "60-71" , ifelse (
                      db$score >= 34, "41-59" , ifelse (
                        db$score >= 32, "29-40" , ifelse (
                          db$score >= 26, "19-28" , ifelse (
                            db$score >= 22, "11-18" , ifelse (
                              db$score >= 20, "6-10" , ifelse (
                                db$score >= 12, "3-5" , ifelse (
                                  db$score >= 8, "2" , ifelse (
                                    db$score >= 7, "1" , ifelse (
                                      db$score <= 6, "<1" , NA ))))))))))))))))))
    
    
    
    
    
  }
  
  
  ##################################TABLE 4##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    
    db$wordcolor_scale_score <- with ( db, ifelse (
      db$score >= 73, 18, ifelse (
        db$score >= 67, 17, ifelse (
          db$score >= 57, 16, ifelse (
            db$score >= 55, 15, ifelse (
              db$score >= 50, 14, ifelse (
                db$score >= 45, 13, ifelse (
                  db$score >= 42, 12, ifelse (
                    db$score >= 39, 11, ifelse (
                      db$score >= 33, 10, ifelse (
                        db$score >= 30, 9, ifelse (
                          db$score >= 26, 8, ifelse (
                            db$score >= 21, 7, ifelse (
                              db$score >= 19, 6, ifelse (
                                db$score >= 12, 5, ifelse (
                                  db$score >= 8, 4, ifelse (
                                    db$score >= 7, 3, ifelse (
                                      db$score <= 6, 2, NA ))))))))))))))))))
    
    # percentile score
    
    db$wordcolor_percentil_range <- with (db, ifelse (
      db$score >= 73, "> 99" , ifelse (
        db$score >= 67, "99" , ifelse (
          db$score >= 57, "98" , ifelse (
            db$score >= 55, "95-97" , ifelse (
              db$score >= 50, "90-94" , ifelse (
                db$score >= 45, "82-89" , ifelse (
                  db$score >= 42, "72-81" , ifelse (
                    db$score >= 39, "60-71" , ifelse (
                      db$score >= 33, "41-59" , ifelse (
                        db$score >= 30, "29-40" , ifelse (
                          db$score >= 26, "19-28" , ifelse (
                            db$score >= 21, "11-18" , ifelse (
                              db$score >= 19, "6-10" , ifelse (
                                db$score >= 12, "3-5" , ifelse (
                                  db$score >= 8, "2" , ifelse (
                                    db$score >= 7, "1" , ifelse (
                                      db$score <= 6, "<1" , NA ))))))))))))))))))
    
    
    
    
    
  }
  
  
  #############################TABLE 5###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    db$wordcolor_scale_score <- with ( db, ifelse (
      db$score >= 57, 18, ifelse (
        db$score >= 56, 17, ifelse (
          
          db$score >= 51, 15, ifelse (
            db$score >= 48, 14, ifelse (
              db$score >= 44, 13, ifelse (
                db$score >= 42, 12, ifelse (
                  db$score >= 37, 11, ifelse (
                    db$score >= 31, 10, ifelse (
                      db$score >= 27, 9, ifelse (
                        db$score >= 25, 8, ifelse (
                          db$score >= 21, 7, ifelse (
                            db$score >= 19, 6, ifelse (
                              db$score >= 10, 5, ifelse (
                                db$score >= 8, 4, ifelse (
                                  db$score >= 7, 3, ifelse (
                                    db$score <= 6, 2, NA )))))))))))))))))

# percentile score

db$wordcolor_percentil_range <- with (db, ifelse (
  db$score >= 57, "> 99" , ifelse (
    db$score >= 56, "99" , ifelse (
      
      db$score >= 51, "95-97" , ifelse (
        db$score >= 48, "90-94" , ifelse (
          db$score >= 44, "82-89" , ifelse (
            db$score >= 42, "72-81" , ifelse (
              db$score >= 37, "60-71" , ifelse (
                db$score >= 31, "41-59" , ifelse (
                  db$score >= 27, "29-40" , ifelse (
                    db$score >= 25, "19-28" , ifelse (
                      db$score >= 21, "11-18" , ifelse (
                        db$score >= 19, "6-10" , ifelse (
                          db$score >= 10, "3-5" , ifelse (
                            db$score >= 8, "2" , ifelse (
                              db$score >= 7, "1" , ifelse (
                                db$score <= 6, "<1" , NA )))))))))))))))))
    
    
    
  }
  
  
  ###########################TABLE 6#####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    
    db$wordcolor_scale_score <- with ( db, ifelse (
      db$score >= 56, 18, ifelse (
        db$score >= 55, 17, ifelse (
          db$score >= 54, 16, ifelse (
            db$score >= 50, 15, ifelse (
              db$score >= 46, 14, ifelse (
                db$score >= 42, 13, ifelse (
                  db$score >= 39, 12, ifelse (
                    db$score >= 35, 11, ifelse (
                      db$score >= 29, 10, ifelse (
                        db$score >= 25, 9, ifelse (
                          db$score >= 22, 8, ifelse (
                            db$score >= 16, 7, ifelse (
                              db$score >= 14, 6, ifelse (
                                db$score >= 10, 5, ifelse (
                                  db$score >= 8, 4, ifelse (
                                    db$score >= 7, 3, ifelse (
                                      db$score <= 6, 2, NA ))))))))))))))))))
    
    # percentile score
    
    db$wordcolor_percentil_range <- with (db, ifelse (
      db$score >= 56, "> 99" , ifelse (
        db$score >= 55, "99" , ifelse (
          db$score >= 54, "98" , ifelse (
            db$score >= 50, "95-97" , ifelse (
              db$score >= 46, "90-94" , ifelse (
                db$score >= 42, "82-89" , ifelse (
                  db$score >= 39, "72-81" , ifelse (
                    db$score >= 35, "60-71" , ifelse (
                      db$score >= 29, "41-59" , ifelse (
                        db$score >= 25, "29-40" , ifelse (
                          db$score >= 22, "19-28" , ifelse (
                            db$score >= 16, "11-18" , ifelse (
                              db$score >= 14, "6-10" , ifelse (
                                db$score >= 10, "3-5" , ifelse (
                                  db$score >= 8, "2" , ifelse (
                                    db$score >= 7, "1" , ifelse (
                                      db$score <= 6, "<1" , NA ))))))))))))))))))
    
  }
  
  
  ############################TABLE 7####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$wordcolor_scale_score <- with ( db, ifelse (
      db$score >= 55, 18, ifelse (
        db$score >= 52, 17, ifelse (
          db$score >= 51, 16, ifelse (
            db$score >= 48, 15, ifelse (
              db$score >= 44, 14, ifelse (
                db$score >= 40, 13, ifelse (
                  db$score >= 36, 12, ifelse (
                    db$score >= 31, 11, ifelse (
                      db$score >= 27, 10, ifelse (
                        db$score >= 25, 9, ifelse (
                          db$score >= 22, 8, ifelse (
                            db$score >= 18, 7, ifelse (
                              db$score >= 14, 6, ifelse (
                                db$score >= 10, 5, ifelse (
                                  db$score >= 6, 4, ifelse (
                                    
                                    db$score <= 5, 2, NA )))))))))))))))))

# percentile score

db$wordcolor_percentil_range <- with (db, ifelse (
  db$score >= 55, "> 99" , ifelse (
    db$score >= 52, "99" , ifelse (
      db$score >= 51, "98" , ifelse (
        db$score >= 48, "95-97" , ifelse (
          db$score >= 44, "90-94" , ifelse (
            db$score >= 40, "82-89" , ifelse (
              db$score >= 36, "72-81" , ifelse (
                db$score >= 31, "60-71" , ifelse (
                  db$score >= 27, "41-59" , ifelse (
                    db$score >= 25, "29-40" , ifelse (
                      db$score >= 22, "19-28" , ifelse (
                        db$score >= 18, "11-18" , ifelse (
                          db$score >= 14, "6-10" , ifelse (
                            db$score >= 10, "3-5" , ifelse (
                              db$score >= 6, "2" , ifelse (
                                
                                db$score <= 5, "<1" , NA )))))))))))))))))
    
  }
  
  
  ##############################TABLE 8##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    db$wordcolor_scale_score <- with ( db, ifelse (
      db$score >= 52, 18, ifelse (
        db$score >= 51, 17, ifelse (
          db$score >= 47, 16, ifelse (
            db$score >= 45, 15, ifelse (
              db$score >= 43, 14, ifelse (
                db$score >= 37, 13, ifelse (
                  db$score >= 34, 12, ifelse (
                    db$score >= 30, 11, ifelse (
                      db$score >= 26, 10, ifelse (
                        db$score >= 24, 9, ifelse (
                          db$score >= 20, 8, ifelse (
                            db$score >= 15, 7, ifelse (
                              db$score >= 12, 6, ifelse (
                                db$score >= 10, 5, ifelse (
                                  db$score >= 6, 4, ifelse (
                                    
                                    db$score <= 5, 2, NA )))))))))))))))))

# percentile score

db$wordcolor_percentil_range <- with (db, ifelse (
  db$score >= 52, "> 99" , ifelse (
    db$score >= 51, "99" , ifelse (
      db$score >= 47, "98" , ifelse (
        db$score >= 45, "95-97" , ifelse (
          db$score >= 43, "90-94" , ifelse (
            db$score >= 37, "82-89" , ifelse (
              db$score >= 34, "72-81" , ifelse (
                db$score >= 30, "60-71" , ifelse (
                  db$score >= 26, "41-59" , ifelse (
                    db$score >= 24, "29-40" , ifelse (
                      db$score >= 20, "19-28" , ifelse (
                        db$score >= 15, "11-18" , ifelse (
                          db$score >= 12, "6-10" , ifelse (
                            db$score >= 10, "3-5" , ifelse (
                              db$score >= 6, "2" , ifelse (
                                
                                db$score <= 5, "<1" , NA )))))))))))))))))
    
    
  }
  
  
  ##############################TABLE 9##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    
    db$wordcolor_scale_score <- with ( db, ifelse (
      db$score >=  51, 18, ifelse (
        db$score >= 47 , 17, ifelse (
          
          db$score >= 45 , 15, ifelse (
            db$score >= 41 , 14, ifelse (
              db$score >= 37 , 13, ifelse (
                db$score >= 31 , 12, ifelse (
                  db$score >= 28 , 11, ifelse (
                    db$score >= 25 , 10, ifelse (
                      db$score >= 21 , 9, ifelse (
                        db$score >= 18 , 8, ifelse (
                          db$score >= 14 , 7, ifelse (
                            db$score >= 11 , 6, ifelse (
                              db$score >= 9 , 5, ifelse (
                                
                                db$score >= 6 , 3, ifelse (
                                  db$score <=  5, 2, NA ))))))))))))))))

# percentile score

db$wordcolor_percentil_range <- with (db, ifelse (
  db$score >=  51, "> 99" , ifelse (
    db$score >= 47 , "99" , ifelse (
      
      db$score >= 45 , "95-97" , ifelse (
        db$score >= 41 , "90-94" , ifelse (
          db$score >= 37 , "82-89" , ifelse (
            db$score >= 31 , "72-81" , ifelse (
              db$score >= 28 , "60-71" , ifelse (
                db$score >= 25 , "41-59" , ifelse (
                  db$score >= 21 , "29-40" , ifelse (
                    db$score >= 18 , "19-28" , ifelse (
                      db$score >= 14 , "11-18" , ifelse (
                        db$score >= 11 , "6-10" , ifelse (
                          db$score >= 9 , "3-5" , ifelse (
                            
                            db$score >= 6 , "1" , ifelse (
                              db$score <=  5, "<1" , NA ))))))))))))))))
    
  }
  
  
  
  ##############################TABLE 10##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    
    db$wordcolor_scale_score <- with ( db, ifelse (
      db$score >= 46, 18, ifelse (
        
        db$score >= 45, 16, ifelse (
          db$score >= 44, 15, ifelse (
            db$score >= 39, 14, ifelse (
              db$score >= 35, 13, ifelse (
                db$score >= 29, 12, ifelse (
                  db$score >= 27, 11, ifelse (
                    db$score >= 22, 10, ifelse (
                      db$score >= 19, 9, ifelse (
                        db$score >= 16, 8, ifelse (
                          db$score >= 12, 7, ifelse (
                            db$score >= 10, 6, ifelse (
                              db$score >= 9, 5, ifelse (
                                db$score >= 6, 4, ifelse (
                                  db$score >= 5, 3, ifelse (
                                    db$score <= 4, 2, NA )))))))))))))))))

# percentile score

db$wordcolor_percentil_range <- with (db, ifelse (
  db$score >= 46, "> 99" , ifelse (
    
    db$score >= 45, "98" , ifelse (
      db$score >= 44, "95-97" , ifelse (
        db$score >= 39, "90-94" , ifelse (
          db$score >= 35, "82-89" , ifelse (
            db$score >= 29, "72-81" , ifelse (
              db$score >= 27, "60-71" , ifelse (
                db$score >= 22, "41-59" , ifelse (
                  db$score >= 19, "29-40" , ifelse (
                    db$score >= 16, "19-28" , ifelse (
                      db$score >= 12, "11-18" , ifelse (
                        db$score >= 10, "6-10" , ifelse (
                          db$score >= 9, "3-5" , ifelse (
                            db$score >= 6, "2" , ifelse (
                              db$score >= 5, "1" , ifelse (
                                db$score <= 4, "<1" , NA )))))))))))))))))
    
    
  }
  
  
  ##############################TABLE 11##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    
    db$wordcolor_scale_score <- with ( db, ifelse (
      db$score >= 43, 18, ifelse (
        
        db$score >= 41, 16, ifelse (
          db$score >= 39, 15, ifelse (
            db$score >= 37, 14, ifelse (
              db$score >= 32, 13, ifelse (
                db$score >= 27, 12, ifelse (
                  db$score >= 24, 11, ifelse (
                    db$score >= 20, 10, ifelse (
                      db$score >= 18, 9, ifelse (
                        db$score >= 15, 8, ifelse (
                          db$score >= 12, 7, ifelse (
                            db$score >= 10, 6, ifelse (
                              db$score >= 9, 5, ifelse (
                                
                                db$score >= 8, 3, ifelse (
                                  db$score <= 7, 2, NA ))))))))))))))))

# percentile score

db$wordcolor_percentil_range <- with (db, ifelse (
  db$score >= 43, "> 99" , ifelse (
    
    db$score >= 41, "98" , ifelse (
      db$score >= 39, "95-97" , ifelse (
        db$score >= 37, "90-94" , ifelse (
          db$score >= 32, "82-89" , ifelse (
            db$score >= 27, "72-81" , ifelse (
              db$score >= 24, "60-71" , ifelse (
                db$score >= 20, "41-59" , ifelse (
                  db$score >= 18, "29-40" , ifelse (
                    db$score >= 15, "19-28" , ifelse (
                      db$score >= 12, "11-18" , ifelse (
                        db$score >= 10, "6-10" , ifelse (
                          db$score >= 9, "3-5" , ifelse (
                            
                            db$score >= 8, "1" , ifelse (
                              db$score <= 7, "<1" , NA ))))))))))))))))
    
  }
  
  
  #############################TABLE 12###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    db$wordcolor_scale_score <- with ( db, ifelse (
      db$score >= 41, 18, ifelse (
        
        db$score >= 38, 16, ifelse (
          db$score >= 37, 15, ifelse (
            db$score >= 35, 14, ifelse (
              db$score >= 28, 13, ifelse (
                db$score >= 26, 12, ifelse (
                  db$score >= 23, 11, ifelse (
                    db$score >= 19, 10, ifelse (
                      db$score >= 17, 9, ifelse (
                        db$score >= 15, 8, ifelse (
                          db$score >= 11, 7, ifelse (
                            db$score >= 10, 6, ifelse (
                              db$score >= 9, 5, ifelse (
                                db$score >= 8, 4, ifelse (
                                  
                                  db$score <= 7, 2, NA ))))))))))))))))

# percentile score

db$wordcolor_percentil_range <- with (db, ifelse (
  db$score >= 41, "> 99" , ifelse (
    
    db$score >= 38, "98" , ifelse (
      db$score >= 37, "95-97" , ifelse (
        db$score >= 35, "90-94" , ifelse (
          db$score >= 28, "82-89" , ifelse (
            db$score >= 26, "72-81" , ifelse (
              db$score >= 23, "60-71" , ifelse (
                db$score >= 19, "41-59" , ifelse (
                  db$score >= 17, "29-40" , ifelse (
                    db$score >= 15, "19-28" , ifelse (
                      db$score >= 11, "11-18" , ifelse (
                        db$score >= 10, "6-10" , ifelse (
                          db$score >= 9, "3-5" , ifelse (
                            db$score >= 8, "2" , ifelse (
                              
                              db$score <= 7, "<1" , NA ))))))))))))))))
    
    
  }
  
  
  # Educational level adjust 
  
  db$education_years_adj <- with(db, ifelse(
    db$education_years >= 0  & db$education_years < 1, db$wordcolor_scale_score + 2, ifelse(
      db$education_years >= 1  & db$education_years <= 6, db$wordcolor_scale_score + 1, ifelse(
        db$education_years >= 7  & db$education_years <= 12, db$wordcolor_scale_score, ifelse(
          db$education_years >= 13  & db$education_years <= 17, db$wordcolor_scale_score - 1, ifelse(
            db$education_years >= 18  & db$education_years <= 20, db$wordcolor_scale_score - 2, ifelse(
              
                )))))))
  
  
  # NSSae
  db$NSSae_wordcolor <- db$wordcolor_scale_score - (0.17826*(db$education_years_adj-12)) 
  
  return(db)
}
