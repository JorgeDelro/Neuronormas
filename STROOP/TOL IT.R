# Tower of London -  Total initation time

# Reference
# Peña-Casanova, J., Quiñones-Ubeda, S., Gramunt-Fombuena,N., et al., 2009. Neuronorma
# study team. Spanish multicenter normative studies (neuronorma project): norms for the stroop color-word 
# interference test and the tower of london-drexel. 
# Arch. Clin. Neuropsychol. 24 (4), 413-29.
# 

# 
TOL_IT <- function(score, age, education_years){
  
  TOL_IT_db <- data.frame(score = score, age = age, education_years = education_years)
  TOL_IT_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(TOL_IT_db)) {
    res <- TOL_IT_scale_score(score = TOL_IT_db[i, "score"], 
                              age = TOL_IT_db[i, "age"],
                              education_years = TOL_IT_db[i, "education_years"])
    TOL_IT_new <- rbind(TOL_IT_new, res)
  }
  
  return(TOL_IT_new)
}

TOL_IT_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    
    db$TOL_IT_scale_score <- with ( db, ifelse (
      db$score <= 9, 18, ifelse (
        db$score <= 10, 17, ifelse (
          db$score <= 12, 16, ifelse (
            db$score <= 14, 15, ifelse (
              db$score <= 20, 14, ifelse (
                db$score <= 29, 13, ifelse (
                  db$score <= 34, 12, ifelse (
                    db$score <= 41, 11, ifelse (
                      db$score <= 53, 10, ifelse (
                        db$score <= 70, 9, ifelse (
                          db$score <= 82, 8, ifelse (
                            db$score <= 103, 7, ifelse (
                              db$score <= 133, 6, ifelse (
                                db$score <= 173, 5, ifelse (
                                  db$score <= 272, 4, ifelse (
                                    db$score <= 307, 3, ifelse (
                                      db$score >= 308, 2, NA ))))))))))))))))))
    
    # percentile score
    
    db$TOL_IT_percentil_range <- with (db, ifelse (
      db$score <= 9, "> 99" , ifelse (
        db$score <= 10, "99" , ifelse (
          db$score <= 12, "98" , ifelse (
            db$score <= 14, "95-97" , ifelse (
              db$score <= 20, "90-94" , ifelse (
                db$score <= 29, "82-89" , ifelse (
                  db$score <= 34, "72-81" , ifelse (
                    db$score <= 41, "60-71" , ifelse (
                      db$score <= 53, "41-59" , ifelse (
                        db$score <= 70, "29-40" , ifelse (
                          db$score <= 82, "19-28" , ifelse (
                            db$score <= 103, "11-18" , ifelse (
                              db$score <= 133, "6-10" , ifelse (
                                db$score <= 173, "3-5" , ifelse (
                                  db$score <= 272, "2" , ifelse (
                                    db$score <= 307, "1" , ifelse (
                                      db$score >= 308, "<1" , NA ))))))))))))))))))
    
    
    
    
  }
  
  
  ##################################TABLE 4##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    
    db$TOL_IT_scale_score <- with ( db, ifelse (
      db$score <= 8, 18, ifelse (
        db$score <= 11, 17, ifelse (
          db$score <= 13, 16, ifelse (
            db$score <= 16, 15, ifelse (
              db$score <= 25, 14, ifelse (
                db$score <= 31, 13, ifelse (
                  db$score <= 34, 12, ifelse (
                    db$score <= 41, 11, ifelse (
                      db$score <= 54, 10, ifelse (
                        db$score <= 65, 9, ifelse (
                          db$score <= 80, 8, ifelse (
                            db$score <= 102, 7, ifelse (
                              db$score <= 126, 6, ifelse (
                                db$score <= 222, 5, ifelse (
                                  db$score <= 272, 4, ifelse (
                                    db$score <= 307, 3, ifelse (
                                      db$score >= 308, 2, NA ))))))))))))))))))
    
    # percentile score
    
    db$TOL_IT_percentil_range <- with (db, ifelse (
      db$score <= 8, "> 99" , ifelse (
        db$score <= 11, "99" , ifelse (
          db$score <= 13, "98" , ifelse (
            db$score <= 16, "95-97" , ifelse (
              db$score <= 25, "90-94" , ifelse (
                db$score <= 31, "82-89" , ifelse (
                  db$score <= 34, "72-81" , ifelse (
                    db$score <= 41, "60-71" , ifelse (
                      db$score <= 54, "41-59" , ifelse (
                        db$score <= 65, "29-40" , ifelse (
                          db$score <= 80, "19-28" , ifelse (
                            db$score <= 102, "11-18" , ifelse (
                              db$score <= 126, "6-10" , ifelse (
                                db$score <= 222, "3-5" , ifelse (
                                  db$score <= 272, "2" , ifelse (
                                    db$score <= 307, "1" , ifelse (
                                      db$score >= 308, "<1" , NA ))))))))))))))))))
    
    
    
    
  }
  
  
  #############################TABLE 5###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    
    db$TOL_IT_scale_score <- with ( db, ifelse (
      db$score <= 11, 18, ifelse (
        db$score <= 13, 17, ifelse (
          db$score <= 14, 16, ifelse (
            db$score <= 19, 15, ifelse (
              db$score <= 25, 14, ifelse (
                db$score <= 30, 13, ifelse (
                  db$score <= 34, 12, ifelse (
                    db$score <= 40, 11, ifelse (
                      db$score <= 53, 10, ifelse (
                        db$score <= 65, 9, ifelse (
                          db$score <= 83, 8, ifelse (
                            db$score <= 108, 7, ifelse (
                              db$score <= 126, 6, ifelse (
                                db$score <= 222, 5, ifelse (
                                  db$score <= 307, 4, ifelse (
                                    db$score <= 414, 3, ifelse (
                                      db$score >= 415, 2, NA ))))))))))))))))))
    
    # percentile score
    
    db$TOL_IT_percentil_range <- with (db, ifelse (
      db$score <= 11, "> 99" , ifelse (
        db$score <= 13, "99" , ifelse (
          db$score <= 14, "98" , ifelse (
            db$score <= 19, "95-97" , ifelse (
              db$score <= 25, "90-94" , ifelse (
                db$score <= 30, "82-89" , ifelse (
                  db$score <= 34, "72-81" , ifelse (
                    db$score <= 40, "60-71" , ifelse (
                      db$score <= 53, "41-59" , ifelse (
                        db$score <= 65, "29-40" , ifelse (
                          db$score <= 83, "19-28" , ifelse (
                            db$score <= 108, "11-18" , ifelse (
                              db$score <= 126, "6-10" , ifelse (
                                db$score <= 222, "3-5" , ifelse (
                                  db$score <= 307, "2" , ifelse (
                                    db$score <= 414, "1" , ifelse (
                                      db$score >= 415, "<1" , NA ))))))))))))))))))
    
    
  }
  
  
  ###########################TABLE 6#####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    
    db$TOL_IT_scale_score <- with ( db, ifelse (
      db$score <= 15, 18, ifelse (
        db$score <= 17, 17, ifelse (
          
          db$score <= 21, 15, ifelse (
            db$score <= 27, 14, ifelse (
              db$score <= 31, 13, ifelse (
                db$score <= 36, 12, ifelse (
                  db$score <= 43, 11, ifelse (
                    db$score <= 53, 10, ifelse (
                      db$score <= 65, 9, ifelse (
                        db$score <= 80, 8, ifelse (
                          db$score <= 108, 7, ifelse (
                            db$score <= 122, 6, ifelse (
                              db$score <= 192, 5, ifelse (
                                db$score <= 222, 4, ifelse (
                                  db$score <= 414, 3, ifelse (
                                    db$score >= 415, 2, NA )))))))))))))))))

# percentile score

db$TOL_IT_percentil_range <- with (db, ifelse (
  db$score <= 15, "> 99" , ifelse (
    db$score <= 17, "99" , ifelse (
      
      db$score <= 21, "95-97" , ifelse (
        db$score <= 27, "90-94" , ifelse (
          db$score <= 31, "82-89" , ifelse (
            db$score <= 36, "72-81" , ifelse (
              db$score <= 43, "60-71" , ifelse (
                db$score <= 53, "41-59" , ifelse (
                  db$score <= 65, "29-40" , ifelse (
                    db$score <= 80, "19-28" , ifelse (
                      db$score <= 108, "11-18" , ifelse (
                        db$score <= 122, "6-10" , ifelse (
                          db$score <= 192, "3-5" , ifelse (
                            db$score <= 222, "2" , ifelse (
                              db$score <= 414, "1" , ifelse (
                                db$score >= 415, "<1" , NA )))))))))))))))))
    
  }
  
  
  ############################TABLE 7####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$TOL_IT_scale_score <- with ( db, ifelse (
      db$score <= 15, 18, ifelse (
        db$score <= 17, 17, ifelse (
          
          db$score <= 23, 15, ifelse (
            db$score <= 27, 14, ifelse (
              db$score <= 31, 13, ifelse (
                db$score <= 41, 12, ifelse (
                  db$score <= 47, 11, ifelse (
                    db$score <= 65, 10, ifelse (
                      db$score <= 79, 9, ifelse (
                        db$score <= 95, 8, ifelse (
                          db$score <= 114, 7, ifelse (
                            db$score <= 183, 6, ifelse (
                              db$score <= 259, 5, ifelse (
                                db$score <= 269, 4, ifelse (
                                  db$score <= 414, 3, ifelse (
                                    db$score >= 415, 2, NA )))))))))))))))))

# percentile score

db$TOL_IT_percentil_range <- with (db, ifelse (
  db$score <= 15, "> 99" , ifelse (
    db$score <= 17, "99" , ifelse (
      
      db$score <= 23, "95-97" , ifelse (
        db$score <= 27, "90-94" , ifelse (
          db$score <= 31, "82-89" , ifelse (
            db$score <= 41, "72-81" , ifelse (
              db$score <= 47, "60-71" , ifelse (
                db$score <= 65, "41-59" , ifelse (
                  db$score <= 79, "29-40" , ifelse (
                    db$score <= 95, "19-28" , ifelse (
                      db$score <= 114, "11-18" , ifelse (
                        db$score <= 183, "6-10" , ifelse (
                          db$score <= 259, "3-5" , ifelse (
                            db$score <= 269, "2" , ifelse (
                              db$score <= 414, "1" , ifelse (
                                db$score >= 415, "<1" , NA )))))))))))))))))
    
  }
  
  
  ##############################TABLE 8##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    db$TOL_IT_scale_score <- with ( db, ifelse (
      db$score <= 17, 18, ifelse (
        
        db$score <= 21, 16, ifelse (
          db$score <= 23, 15, ifelse (
            db$score <= 29, 14, ifelse (
              db$score <= 36, 13, ifelse (
                db$score <= 42, 12, ifelse (
                  db$score <= 48, 11, ifelse (
                    db$score <= 65, 10, ifelse (
                      db$score <= 79, 9, ifelse (
                        db$score <= 93, 8, ifelse (
                          db$score <= 109, 7, ifelse (
                            db$score <= 183, 6, ifelse (
                              db$score <= 225, 5, ifelse (
                                db$score <= 259, 4, ifelse (
                                  db$score <= 269, 3, ifelse (
                                    db$score >= 270, 2, NA )))))))))))))))))

# percentile score

db$TOL_IT_percentil_range <- with (db, ifelse (
  db$score <= 17, "> 99" , ifelse (
    
    db$score <= 21, "98" , ifelse (
      db$score <= 23, "95-97" , ifelse (
        db$score <= 29, "90-94" , ifelse (
          db$score <= 36, "82-89" , ifelse (
            db$score <= 42, "72-81" , ifelse (
              db$score <= 48, "60-71" , ifelse (
                db$score <= 65, "41-59" , ifelse (
                  db$score <= 79, "29-40" , ifelse (
                    db$score <= 93, "19-28" , ifelse (
                      db$score <= 109, "11-18" , ifelse (
                        db$score <= 183, "6-10" , ifelse (
                          db$score <= 225, "3-5" , ifelse (
                            db$score <= 259, "2" , ifelse (
                              db$score <= 269, "1" , ifelse (
                                db$score >= 270, "<1" , NA )))))))))))))))))
    
    
  }
  
  
  ##############################TABLE 9##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    db$TOL_IT_scale_score <- with ( db, ifelse (
      db$score <=  17, 18, ifelse (
        db$score <= 21 , 17, ifelse (
          
          db$score <= 24 , 15, ifelse (
            db$score <= 30 , 14, ifelse (
              db$score <= 39 , 13, ifelse (
                db$score <= 44 , 12, ifelse (
                  db$score <= 50 , 11, ifelse (
                    db$score <= 65 , 10, ifelse (
                      db$score <= 78 , 9, ifelse (
                        db$score <= 93 , 8, ifelse (
                          db$score <= 115, 7, ifelse (
                            db$score <= 183, 6, ifelse (
                              db$score <= 225, 5, ifelse (
                                db$score <= 259, 4, ifelse (
                                  db$score <= 269, 3, ifelse (
                                    db$score >=  270, 2, NA )))))))))))))))))

# percentile score

db$TOL_IT_percentil_range <- with (db, ifelse (
  db$score <=  17, "> 99" , ifelse (
    db$score <= 21 , "99" , ifelse (
      
      db$score <= 24 , "95-97" , ifelse (
        db$score <= 30 , "90-94" , ifelse (
          db$score <= 39 , "82-89" , ifelse (
            db$score <= 44 , "72-81" , ifelse (
              db$score <= 50 , "60-71" , ifelse (
                db$score <= 65 , "41-59" , ifelse (
                  db$score <= 78 , "29-40" , ifelse (
                    db$score <= 93 , "19-28" , ifelse (
                      db$score <= 115, "11-18" , ifelse (
                        db$score <= 183, "6-10" , ifelse (
                          db$score <= 225, "3-5" , ifelse (
                            db$score <= 259, "2" , ifelse (
                              db$score <= 269, "1" , ifelse (
                                db$score >=  270, "<1" , NA )))))))))))))))))
    
    
  }
  
  
  
  ##############################TABLE 10##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    
    db$TOL_IT_scale_score <- with ( db, ifelse (
      db$score <= 17, 18, ifelse (
        db$score <= 21, 17, ifelse (
          db$score <= 23, 16, ifelse (
            db$score <= 28, 15, ifelse (
              db$score <= 32, 14, ifelse (
                db$score <= 38, 13, ifelse (
                  db$score <= 42, 12, ifelse (
                    db$score <= 52, 11, ifelse (
                      db$score <= 68, 10, ifelse (
                        db$score <= 79, 9, ifelse (
                          db$score <= 93, 8, ifelse (
                            db$score <= 108, 7, ifelse (
                              db$score <= 165, 6, ifelse (
                                db$score <= 220, 5, ifelse (
                                  db$score <= 225, 4, ifelse (
                                    db$score <= 259, 3, ifelse (
                                      db$score >= 260, 2, NA ))))))))))))))))))
    
    # percentile score
    
    db$TOL_IT_percentil_range <- with (db, ifelse (
      db$score <= 17, "> 99" , ifelse (
        db$score <= 21, "99" , ifelse (
          db$score <= 23, "98" , ifelse (
            db$score <= 28, "95-97" , ifelse (
              db$score <= 32, "90-94" , ifelse (
                db$score <= 38, "82-89" , ifelse (
                  db$score <= 42, "72-81" , ifelse (
                    db$score <= 52, "60-71" , ifelse (
                      db$score <= 68, "41-59" , ifelse (
                        db$score <= 79, "29-40" , ifelse (
                          db$score <= 93, "19-28" , ifelse (
                            db$score <= 108, "11-18" , ifelse (
                              db$score <= 165, "6-10" , ifelse (
                                db$score <= 220, "3-5" , ifelse (
                                  db$score <= 225, "2" , ifelse (
                                    db$score <= 259, "1" , ifelse (
                                      db$score >= 260, "<1" , NA )))))))))))))))))) 
    
    
  }
  
  
  ##############################TABLE 11##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    
    db$TOL_IT_scale_score <- with ( db, ifelse (
      db$score <= 25, 18, ifelse (
        
        db$score <= 28, 16, ifelse (
          db$score <= 29, 15, ifelse (
            db$score <= 33, 14, ifelse (
              db$score <= 41, 13, ifelse (
                db$score <= 50, 12, ifelse (
                  db$score <= 59, 11, ifelse (
                    db$score <= 72, 10, ifelse (
                      db$score <= 80, 9, ifelse (
                        db$score <= 97, 8, ifelse (
                          db$score <= 115, 7, ifelse (
                            db$score <= 165, 6, ifelse (
                              db$score <= 225, 5, ifelse (
                                
                                db$score <= 226, 3, ifelse (
                                  db$score >= 227, 2, NA ))))))))))))))))

# percentile score

db$TOL_IT_percentil_range <- with (db, ifelse (
  db$score <= 25, "> 99" , ifelse (
    
    db$score <= 28, "98" , ifelse (
      db$score <= 29, "95-97" , ifelse (
        db$score <= 33, "90-94" , ifelse (
          db$score <= 41, "82-89" , ifelse (
            db$score <= 50, "72-81" , ifelse (
              db$score <= 59, "60-71" , ifelse (
                db$score <= 72, "41-59" , ifelse (
                  db$score <= 80, "29-40" , ifelse (
                    db$score <= 97, "19-28" , ifelse (
                      db$score <= 115, "11-18" , ifelse (
                        db$score <= 165, "6-10" , ifelse (
                          db$score <= 225, "3-5" , ifelse (
                            
                            db$score <= 226, "1" , ifelse (
                              db$score >= 227, "<1" , NA ))))))))))))))))
    
  }
  
  
  #############################TABLE 12###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    
    db$TOL_IT_scale_score <- with ( db, ifelse (
      db$score <= 29, 18, ifelse (
        
        db$score <= 30, 16, ifelse (
          
          db$score <= 34, 14, ifelse (
            db$score <= 39, 13, ifelse (
              db$score <= 48, 12, ifelse (
                db$score <= 61, 11, ifelse (
                  db$score <= 72, 10, ifelse (
                    db$score <= 88, 9, ifelse (
                      db$score <= 105, 8, ifelse (
                        db$score <= 117, 7, ifelse (
                          db$score <= 139, 6, ifelse (
                            db$score <= 220, 5, ifelse (
                              db$score <= 221, 4, ifelse (
                                
                                db$score >= 222, 2, NA )))))))))))))))

# percentile score

db$TOL_IT_percentil_range <- with (db, ifelse (
  db$score <= 29, "> 99" , ifelse (
    
    db$score <= 30, "98" , ifelse (
      
      db$score <= 34, "90-94" , ifelse (
        db$score <= 39, "82-89" , ifelse (
          db$score <= 48, "72-81" , ifelse (
            db$score <= 61, "60-71" , ifelse (
              db$score <= 72, "41-59" , ifelse (
                db$score <= 88, "29-40" , ifelse (
                  db$score <= 105, "19-28" , ifelse (
                    db$score <= 117, "11-18" , ifelse (
                      db$score <= 139, "6-10" , ifelse (
                        db$score <= 220, "3-5" , ifelse (
                          db$score <= 221, "2" , ifelse (
                            
                            db$score >= 222, "<1" , NA )))))))))))))))
    
  }
  
  
  # Educational level adjust 
  db$education_years_adj <- with(db, ifelse(
      db$education_years >= 0  & db$education_years <= 8, db$TOL_IT_scale_score, ifelse(
        db$education_years >= 9  & db$education_years <= 20, db$TOL_IT_scale_score - 1, ifelse(
          
            ))))
  
  
  # NSSae
  db$NSSae_TOL_IT <- db$TOL_IT_scale_score - (-0.06256*(db$education_years_adj-12)) 
  
  return(db)
}
