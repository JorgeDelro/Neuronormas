# S

# Casals-Coll M., Sánchez-Benavides G., Quintana M., Manerob R.M., Rognonia T., 
# Calvo L., Palomo R., Aranciva F., Tamayo F. & Peña-Casanova J. (2013) Estudios normativos españoles en población adulta joven
# (proyecto NEURONORMA jóvenes): normas para los test de fluencia verbal. Neurología, 28 (1):33-40.


# 
s <- function(score, age, education_years){
  
  s_db <- data.frame(score = score, age = age, education_years = education_years)
  s_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(s_db)) {
    res <- s_scale_score(score = s_db[i, "score"], 
                              age = s_db[i, "age"],
                              education_years = s_db[i, "education_years"])
    s_new <- rbind(s_new, res)
  }
  
  return(s_new)
}

s_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    db$s_scale_score <- with ( db, ifelse (
      db$score >=  30, 18, ifelse (
        db$score >= 28 , 17, ifelse (
          db$score >= 27, 16, ifelse (
            db$score >= 25 , 15, ifelse (
              db$score >= 24, 14, ifelse (
                db$score >= 21 , 13, ifelse (
                  db$score >= 19 , 12, ifelse (
                    db$score >= 16 , 11, ifelse (
                      db$score >= 13 , 10, ifelse (
                        db$score >= 12, 9, ifelse (
                          db$score >= 10 , 8, ifelse (
                            db$score >= 8, 7, ifelse (
                              db$score >= 7, 6, ifelse (
                                db$score >= 5, 5, ifelse (
                                  db$score >= 3, 4, ifelse (
                                    db$score >= 2, 3, ifelse (
                                      db$score <= 1, 2, NA ))))))))))))))))))
    
    # percentile score
    
    db$s_percentil_range <- with (db, ifelse (
      db$score >=  30, "> 99" , ifelse (
        db$score >= 28 , "99" , ifelse (
          db$score >= 27, "98" , ifelse (
            db$score >= 25 , "95-97" , ifelse (
              db$score >= 24, "90-94" , ifelse (
                db$score >= 21 , "82-89" , ifelse (
                  db$score >= 19 , "72-81" , ifelse (
                    db$score >= 16 , "60-71" , ifelse (
                      db$score >= 13 , "41-59" , ifelse (
                        db$score >= 12, "29-40" , ifelse (
                          db$score >= 10 , "19-28" , ifelse (
                            db$score >= 8, "11-18" , ifelse (
                              db$score >= 7, "6-10" , ifelse (
                                db$score >= 5, "3-5" , ifelse (
                                  db$score >= 3, "2" , ifelse (
                                    db$score >= 2, "1" , ifelse (
                                      db$score <= 1, "<1" , NA )))))))))))))))))) 
    
    
    
    
    
  }
  
  
  ##################################TABLE 4##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    db$s_scale_score <- with ( db, ifelse (
      db$score >=  30, 18, ifelse (
        db$score >= 28 , 17, ifelse (
          db$score >= 26 , 16, ifelse (
            db$score >= 24 , 15, ifelse (
              db$score >= 22 , 14, ifelse (
                db$score >= 21, 13, ifelse (
                  db$score >= 18 , 12, ifelse (
                    db$score >= 15 , 11, ifelse (
                      db$score >= 13 , 10, ifelse (
                        db$score >= 11 , 9, ifelse (
                          db$score >= 9, 8, ifelse (
                            db$score >= 7, 7, ifelse (
                              db$score >= 5, 6, ifelse (
                                db$score >= 2, 5, ifelse (
                                  
                                  db$score >= 1, 3, ifelse (
                                    db$score <= 0, 2, NA )))))))))))))))))

# percentile score

db$s_percentil_range <- with (db, ifelse (
  db$score >=  30, "> 99" , ifelse (
    db$score >= 28 , "99" , ifelse (
      db$score >= 26 , "98" , ifelse (
        db$score >= 24 , "95-97" , ifelse (
          db$score >= 22 , "90-94" , ifelse (
            db$score >= 21, "82-89" , ifelse (
              db$score >= 18 , "72-81" , ifelse (
                db$score >= 15 , "60-71" , ifelse (
                  db$score >= 13 , "41-59" , ifelse (
                    db$score >= 11 , "29-40" , ifelse (
                      db$score >= 9, "19-28" , ifelse (
                        db$score >= 7, "11-18" , ifelse (
                          db$score >= 5, "6-10" , ifelse (
                            db$score >= 2, "3-5" , ifelse (
                              
                              db$score >= 1, "1" , ifelse (
                                db$score <= 0, "<1" , NA ))))))))))))))))) 
    
    
    
    
    
  }
  
  
  #############################TABLE 5###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    db$s_scale_score <- with ( db, ifelse (
      db$score >=  29, 18, ifelse (
        db$score >= 28, 17, ifelse (
          db$score >= 26 , 16, ifelse (
            db$score >= 24 , 15, ifelse (
              db$score >= 21 , 14, ifelse (
                db$score >= 20, 13, ifelse (
                  db$score >= 17 , 12, ifelse (
                    db$score >= 15 , 11, ifelse (
                      db$score >= 12 , 10, ifelse (
                        db$score >= 10 , 9, ifelse (
                          db$score >= 8, 8, ifelse (
                            db$score >= 6, 7, ifelse (
                              db$score >= 5, 6, ifelse (
                                db$score >= 2, 5, ifelse (
                                  
                                  db$score >= 1, 3, ifelse (
                                    db$score <= 0, 2, NA )))))))))))))))))

# percentile score

db$s_percentil_range <- with (db, ifelse (
  db$score >=  29, "> 99" , ifelse (
    db$score >= 28, "99" , ifelse (
      db$score >= 26 , "98" , ifelse (
        db$score >= 24 , "95-97" , ifelse (
          db$score >= 21 , "90-94" , ifelse (
            db$score >= 20, "82-89" , ifelse (
              db$score >= 17 , "72-81" , ifelse (
                db$score >= 15 , "60-71" , ifelse (
                  db$score >= 12 , "41-59" , ifelse (
                    db$score >= 10 , "29-40" , ifelse (
                      db$score >= 8, "19-28" , ifelse (
                        db$score >= 6, "11-18" , ifelse (
                          db$score >= 5, "6-10" , ifelse (
                            db$score >= 2, "3-5" , ifelse (
                              
                              db$score >= 1, "1" , ifelse (
                                db$score <= 0, "<1" , NA )))))))))))))))))
    
    
    
  }
  
  
  ###########################TABLE 6#####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    db$s_scale_score <- with ( db, ifelse (
      db$score >=  29, 18, ifelse (
        db$score >= 28, 17, ifelse (
          db$score >= 27, 16, ifelse (
            db$score >= 24 , 15, ifelse (
              db$score >= 21 , 14, ifelse (
                db$score >= 20, 13, ifelse (
                  db$score >= 17 , 12, ifelse (
                    db$score >= 15 , 11, ifelse (
                      db$score >= 11 , 10, ifelse (
                        db$score >= 9, 9, ifelse (
                          db$score >= 8, 8, ifelse (
                            db$score >= 6, 7, ifelse (
                              db$score >= 5, 6, ifelse (
                                
                                db$score >= 2, 4, ifelse (
                                  
                                  db$score <= 1, 2, NA ))))))))))))))))

# percentile score

db$s_percentil_range <- with (db, ifelse (
  db$score >=  29, "> 99" , ifelse (
    db$score >= 28, "99" , ifelse (
      db$score >= 27, "98" , ifelse (
        db$score >= 24 , "95-97" , ifelse (
          db$score >= 21 , "90-94" , ifelse (
            db$score >= 20, "82-89" , ifelse (
              db$score >= 17 , "72-81" , ifelse (
                db$score >= 15 , "60-71" , ifelse (
                  db$score >= 11 , "41-59" , ifelse (
                    db$score >= 9, "29-40" , ifelse (
                      db$score >= 8, "19-28" , ifelse (
                        db$score >= 6, "11-18" , ifelse (
                          db$score >= 5, "6-10" , ifelse (
                            
                            db$score >= 2, "2" , ifelse (
                              
                              db$score <= 1, "<1" , NA ))))))))))))))))
    
    
  }
  
  
  ############################TABLE 7####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$s_scale_score <- with ( db, ifelse (
      db$score >=  27, 18, ifelse (
        db$score >= 26, 17, ifelse (
          db$score >= 25, 16, ifelse (
            db$score >= 21 , 15, ifelse (
              
              db$score >= 18 , 13, ifelse (
                db$score >= 15 , 12, ifelse (
                  db$score >= 13 , 11, ifelse (
                    db$score >= 11 , 10, ifelse (
                      db$score >= 9, 9, ifelse (
                        db$score >= 8, 8, ifelse (
                          db$score >= 7, 7, ifelse (
                            db$score >= 6, 6, ifelse (
                              db$score >= 5, 5, ifelse (
                                
                                db$score >= 2, 3, ifelse (
                                  db$score <= 1, 2, NA ))))))))))))))))

# percentile score

db$s_percentil_range <- with (db, ifelse (
  db$score >=  27, "> 99" , ifelse (
    db$score >= 26, "99" , ifelse (
      db$score >= 25, "98" , ifelse (
        db$score >= 21 , "95-97" , ifelse (
          
          db$score >= 18 , "82-89" , ifelse (
            db$score >= 15 , "72-81" , ifelse (
              db$score >= 13 , "60-71" , ifelse (
                db$score >= 11 , "41-59" , ifelse (
                  db$score >= 9, "29-40" , ifelse (
                    db$score >= 8, "19-28" , ifelse (
                      db$score >= 7, "11-18" , ifelse (
                        db$score >= 6, "6-10" , ifelse (
                          db$score >= 5, "3-5" , ifelse (
                            
                            db$score >= 2, "1" , ifelse (
                              db$score <= 1, "<1" , NA ))))))))))))))))
    
  }
  
  
  ##############################TABLE 8##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    db$s_scale_score <- with ( db, ifelse (
      db$score >=  27, 18, ifelse (
        db$score >= 26, 17, ifelse (
          db$score >= 25, 16, ifelse (
            db$score >= 22 , 15, ifelse (
              db$score >= 21, 14, ifelse (
                db$score >= 18 , 13, ifelse (
                  db$score >= 15 , 12, ifelse (
                    db$score >= 13 , 11, ifelse (
                      db$score >= 10 , 10, ifelse (
                        db$score >= 9, 9, ifelse (
                          db$score >= 8, 8, ifelse (
                            db$score >= 7, 7, ifelse (
                              db$score >= 6, 6, ifelse (
                                db$score >= 5, 5, ifelse (
                                  
                                  db$score >= 1, 3, ifelse (
                                    db$score <= 0, 2, NA )))))))))))))))))

# percentile score

db$s_percentil_range <- with (db, ifelse (
  db$score >=  27, "> 99" , ifelse (
    db$score >= 26, "99" , ifelse (
      db$score >= 25, "98" , ifelse (
        db$score >= 22 , "95-97" , ifelse (
          db$score >= 21, "90-94" , ifelse (
            db$score >= 18 , "82-89" , ifelse (
              db$score >= 15 , "72-81" , ifelse (
                db$score >= 13 , "60-71" , ifelse (
                  db$score >= 10 , "41-59" , ifelse (
                    db$score >= 9, "29-40" , ifelse (
                      db$score >= 8, "19-28" , ifelse (
                        db$score >= 7, "11-18" , ifelse (
                          db$score >= 6, "6-10" , ifelse (
                            db$score >= 5, "3-5" , ifelse (
                              
                              db$score >= 1, "1" , ifelse (
                                db$score <= 0, "<1" , NA )))))))))))))))))
    
    
  }
  
  
  ##############################TABLE 9##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    db$s_scale_score <- with ( db, ifelse (
      db$score >=  26, 18, ifelse (
        db$score >= 25, 17, ifelse (
          db$score >= 24, 16, ifelse (
            db$score >= 21 , 15, ifelse (
              db$score >= 19 , 14, ifelse (
                db$score >= 16 , 13, ifelse (
                  db$score >= 13 , 12, ifelse (
                    db$score >= 12, 11, ifelse (
                      db$score >= 9, 10, ifelse (
                        db$score >= 8, 9, ifelse (
                          
                          db$score >= 7, 7, ifelse (
                            db$score >= 6, 6, ifelse (
                              db$score >= 5, 5, ifelse (
                                
                                db$score >= 1, 3, ifelse (
                                  db$score <= 0, 2, NA ))))))))))))))))

# percentile score

db$s_percentil_range <- with (db, ifelse (
  db$score >=  26, "> 99" , ifelse (
    db$score >= 25, "99" , ifelse (
      db$score >= 24, "98" , ifelse (
        db$score >= 21 , "95-97" , ifelse (
          db$score >= 19 , "90-94" , ifelse (
            db$score >= 16 , "82-89" , ifelse (
              db$score >= 13 , "72-81" , ifelse (
                db$score >= 12, "60-71" , ifelse (
                  db$score >= 9, "41-59" , ifelse (
                    db$score >= 8, "29-40" , ifelse (
                      
                      db$score >= 7, "11-18" , ifelse (
                        db$score >= 6, "6-10" , ifelse (
                          db$score >= 5, "3-5" , ifelse (
                            
                            db$score >= 1, "1" , ifelse (
                              db$score <= 0, "<1" , NA )))))))))))))))) 
    
    
  }
  
  
  
  ##############################TABLE 10##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    db$s_scale_score <- with ( db, ifelse (
      db$score >=  25, 18, ifelse (
        db$score >= 24, 17, ifelse (
          db$score >= 23, 16, ifelse (
            db$score >= 21 , 15, ifelse (
              db$score >= 17 , 14, ifelse (
                db$score >= 14 , 13, ifelse (
                  db$score >= 12 , 12, ifelse (
                    db$score >= 11, 11, ifelse (
                      db$score >= 9, 10, ifelse (
                        db$score >= 8, 9, ifelse (
                          db$score >= 7, 8, ifelse (
                            db$score >= 6, 7, ifelse (
                              db$score >= 5, 6, ifelse (
                                db$score >= 3, 5, ifelse (
                                  db$score >= 1, 4, ifelse (
                                    
                                    db$score <= 0, 2, NA )))))))))))))))))

# percentile score

db$s_percentil_range <- with (db, ifelse (
  db$score >=  25, "> 99" , ifelse (
    db$score >= 24, "99" , ifelse (
      db$score >= 23, "98" , ifelse (
        db$score >= 21 , "95-97" , ifelse (
          db$score >= 17 , "90-94" , ifelse (
            db$score >= 14 , "82-89" , ifelse (
              db$score >= 12 , "72-81" , ifelse (
                db$score >= 11, "60-71" , ifelse (
                  db$score >= 9, "41-59" , ifelse (
                    db$score >= 8, "29-40" , ifelse (
                      db$score >= 7, "19-28" , ifelse (
                        db$score >= 6, "11-18" , ifelse (
                          db$score >= 5, "6-10" , ifelse (
                            db$score >= 3, "3-5" , ifelse (
                              db$score >= 1, "2" , ifelse (
                                
                                db$score <= 0, "<1" , NA ))))))))))))))))) 
    
    
    
  }
  
  
  ##############################TABLE 11##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    db$s_scale_score <- with ( db, ifelse (
      db$score >=  23, 18, ifelse (
        
        db$score >= 22, 16, ifelse (
          db$score >= 18 , 15, ifelse (
            db$score >= 15 , 14, ifelse (
              db$score >= 13 , 13, ifelse (
                db$score >= 12, 12, ifelse (
                  db$score >= 10 , 11, ifelse (
                    db$score >= 9, 10, ifelse (
                      db$score >= 8, 9, ifelse (
                        db$score >= 6, 8, ifelse (
                          
                          db$score >= 5, 6, ifelse (
                            db$score >= 3, 5, ifelse (
                              
                              db$score >= 1, 3, ifelse (
                                db$score <= 0, 2, NA )))))))))))))))

# percentile score

db$s_percentil_range <- with (db, ifelse (
  db$score >=  23, "> 99" , ifelse (
    
    db$score >= 22, "98" , ifelse (
      db$score >= 18 , "95-97" , ifelse (
        db$score >= 15 , "90-94" , ifelse (
          db$score >= 13 , "82-89" , ifelse (
            db$score >= 12, "72-81" , ifelse (
              db$score >= 10 , "60-71" , ifelse (
                db$score >= 9, "41-59" , ifelse (
                  db$score >= 8, "29-40" , ifelse (
                    db$score >= 6, "19-28" , ifelse (
                      
                      db$score >= 5, "6-10" , ifelse (
                        db$score >= 3, "3-5" , ifelse (
                          
                          db$score >= 1, "1" , ifelse (
                            db$score <= 0, "<1" , NA ))))))))))))))) 
    
    
  }
  
  
  #############################TABLE 12###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    db$s_scale_score <- with ( db, ifelse (
      db$score >=  17, 18, ifelse (
        
        db$score >= 15 , 16, ifelse (
          
          db$score >= 14, 14, ifelse (
            db$score >= 13, 13, ifelse (
              db$score >= 11 , 12, ifelse (
                db$score >= 10, 11, ifelse (
                  db$score >= 8, 10, ifelse (
                    db$score >= 7, 9, ifelse (
                      db$score >= 6, 8, ifelse (
                        db$score >= 5, 7, ifelse (
                          
                          db$score >= 3, 5, ifelse (
                            db$score >= 1, 4, ifelse (
                              
                              db$score <= 0, 2, NA ))))))))))))))

# percentile score

db$s_percentil_range <- with (db, ifelse (
  db$score >=  17, "> 99" , ifelse (
    
    db$score >= 15 , "98" , ifelse (
      
      db$score >= 14, "90-94" , ifelse (
        db$score >= 13, "82-89" , ifelse (
          db$score >= 11 , "72-81" , ifelse (
            db$score >= 10, "60-71" , ifelse (
              db$score >= 8, "41-59" , ifelse (
                db$score >= 7, "29-40" , ifelse (
                  db$score >= 6, "19-28" , ifelse (
                    db$score >= 5, "11-18" , ifelse (
                      
                      db$score >= 3, "3-5" , ifelse (
                        db$score >= 1, "2" , ifelse (
                          
                          db$score <= 0, "<1" , NA ))))))))))))))
    
    
  }
  
  
  # Educational level adjust 
  db$education_years_adj <- with(db, ifelse(
    db$education_years >= 0  & db$education_years < 1, db$s_scale_score + 3, ifelse(
      db$education_years >= 1  & db$education_years <= 4, db$s_scale_score + 2, ifelse(
    db$education_years >= 5  & db$education_years <= 8, db$s_scale_score + 1, ifelse(
      db$education_years >= 9  & db$education_years <= 12, db$s_scale_score, ifelse(
        db$education_years >= 13  & db$education_years <= 15, db$s_scale_score - 1, ifelse(
          db$education_years >= 16  & db$education_years <= 19, db$s_scale_score - 2, ifelse(
            db$education_years > 19  & db$education_years <= 20, db$s_scale_score - 3, ifelse(
            )))))))))
  
  
  # NSSae
  db$NSSae_s <- db$s_scale_score - (0.25277*(db$education_years_adj-12)) 
  
  return(db)
}
