# P

# Casals-Coll M., Sánchez-Benavides G., Quintana M., Manerob R.M., Rognonia T., 
# Calvo L., Palomo R., Aranciva F., Tamayo F. & Peña-Casanova J. (2013) Estudios normativos españoles en población adulta joven
# (proyecto NEURONORMA jóvenes): normas para los test de fluencia verbal. Neurología, 28 (1):33-40.


# 
p <- function(score, age, education_years){
  
  p_db <- data.frame(score = score, age = age, education_years = education_years)
  p_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(p_db)) {
    res <- p_scale_score(score = p_db[i, "score"], 
                              age = p_db[i, "age"],
                              education_years = p_db[i, "education_years"])
    p_new <- rbind(p_new, res)
  }
  
  return(p_new)
}

p_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    db$p_scale_score <- with ( db, ifelse (
      db$score >=  31, 18, ifelse (
        db$score >= 30, 17, ifelse (
          db$score >= 28 , 16, ifelse (
            db$score >= 24 , 15, ifelse (
              db$score >= 23, 14, ifelse (
                db$score >= 21 , 13, ifelse (
                  db$score >= 19 , 12, ifelse (
                    db$score >= 18, 11, ifelse (
                      db$score >= 14 , 10, ifelse (
                        db$score >= 13, 9, ifelse (
                          db$score >= 11 , 8, ifelse (
                            db$score >= 9, 7, ifelse (
                              
                              db$score >= 8, 5, ifelse (
                                db$score >= 7, 4, ifelse (
                                  db$score >= 5, 3, ifelse (
                                    db$score <= 4, 2, NA )))))))))))))))))

# percentile score

db$p_percentil_range <- with (db, ifelse (
  db$score >=  31, "> 99" , ifelse (
    db$score >= 30, "99" , ifelse (
      db$score >= 28 , "98" , ifelse (
        db$score >= 24 , "95-97" , ifelse (
          db$score >= 23, "90-94" , ifelse (
            db$score >= 21 , "82-89" , ifelse (
              db$score >= 19 , "72-81" , ifelse (
                db$score >= 18, "60-71" , ifelse (
                  db$score >= 14 , "41-59" , ifelse (
                    db$score >= 13, "29-40" , ifelse (
                      db$score >= 11 , "19-28" , ifelse (
                        db$score >= 9, "11-18" , ifelse (
                          
                          db$score >= 8, "3-5" , ifelse (
                            db$score >= 7, "2" , ifelse (
                              db$score >= 5, "1" , ifelse (
                                db$score <= 4, "<1" , NA )))))))))))))))))
    
    
    
    
    
  }
  
  
  ##################################TABLE 4##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    db$p_scale_score <- with ( db, ifelse (
      db$score >=  29, 18, ifelse (
        db$score >= 27 , 17, ifelse (
          db$score >= 26, 16, ifelse (
            db$score >= 24 , 15, ifelse (
              db$score >= 22 , 14, ifelse (
                db$score >= 21, 13, ifelse (
                  db$score >= 20, 12, ifelse (
                    db$score >= 17 , 11, ifelse (
                      db$score >= 14 , 10, ifelse (
                        db$score >= 13, 9, ifelse (
                          db$score >= 11 , 8, ifelse (
                            db$score >= 9, 7, ifelse (
                              db$score >= 8, 6, ifelse (
                                db$score >= 5, 5, ifelse (
                                  
                                  db$score >= 4, 3, ifelse (
                                    db$score <= 3, 2, NA )))))))))))))))))

# percentile score

db$p_percentil_range <- with (db, ifelse (
  db$score >=  29, "> 99" , ifelse (
    db$score >= 27 , "99" , ifelse (
      db$score >= 26, "98" , ifelse (
        db$score >= 24 , "95-97" , ifelse (
          db$score >= 22 , "90-94" , ifelse (
            db$score >= 21, "82-89" , ifelse (
              db$score >= 20, "72-81" , ifelse (
                db$score >= 17 , "60-71" , ifelse (
                  db$score >= 14 , "41-59" , ifelse (
                    db$score >= 13, "29-40" , ifelse (
                      db$score >= 11 , "19-28" , ifelse (
                        db$score >= 9, "11-18" , ifelse (
                          db$score >= 8, "6-10" , ifelse (
                            db$score >= 5, "3-5" , ifelse (
                              
                              db$score >= 4, "1" , ifelse (
                                db$score <= 3, "<1" , NA )))))))))))))))))
    
    
    
    
    
  }
  
  
  #############################TABLE 5###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    db$p_scale_score <- with ( db, ifelse (
      db$score >=  25, 18, ifelse (
        db$score >= 24, 17, ifelse (
          
          db$score >= 23, 15, ifelse (
            db$score >= 22, 14, ifelse (
              db$score >= 20 , 13, ifelse (
                db$score >= 18 , 12, ifelse (
                  db$score >= 15 , 11, ifelse (
                    db$score >= 13 , 10, ifelse (
                      db$score >= 11 , 9, ifelse (
                        db$score >= 10, 8, ifelse (
                          db$score >= 9, 7, ifelse (
                            db$score >= 7, 6, ifelse (
                              db$score >= 6, 5, ifelse (
                                db$score >= 5, 4, ifelse (
                                  db$score >= 4, 3, ifelse (
                                    db$score <= 3, 2, NA )))))))))))))))))

# percentile score

db$p_percentil_range <- with (db, ifelse (
  db$score >=  25, "> 99" , ifelse (
    db$score >= 24, "99" , ifelse (
      
      db$score >= 23, "95-97" , ifelse (
        db$score >= 22, "90-94" , ifelse (
          db$score >= 20 , "82-89" , ifelse (
            db$score >= 18 , "72-81" , ifelse (
              db$score >= 15 , "60-71" , ifelse (
                db$score >= 13 , "41-59" , ifelse (
                  db$score >= 11 , "29-40" , ifelse (
                    db$score >= 10, "19-28" , ifelse (
                      db$score >= 9, "11-18" , ifelse (
                        db$score >= 7, "6-10" , ifelse (
                          db$score >= 6, "3-5" , ifelse (
                            db$score >= 5, "2" , ifelse (
                              db$score >= 4, "1" , ifelse (
                                db$score <= 3, "<1" , NA ))))))))))))))))) 
    
    
    
  }
  
  
  ###########################TABLE 6#####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    db$p_scale_score <- with ( db, ifelse (
      db$score >=  25, 18, ifelse (
        db$score >= 24, 17, ifelse (
          db$score >= 23, 16, ifelse (
            db$score >= 22, 15, ifelse (
              
              db$score >= 20 , 13, ifelse (
                db$score >= 18 , 12, ifelse (
                  db$score >= 16 , 11, ifelse (
                    db$score >= 13 , 10, ifelse (
                      db$score >= 11 , 9, ifelse (
                        db$score >= 10, 8, ifelse (
                          db$score >= 8, 7, ifelse (
                            db$score >= 7, 6, ifelse (
                              db$score >= 6, 5, ifelse (
                                
                                db$score >= 5, 3, ifelse (
                                  db$score <= 4, 2, NA ))))))))))))))))

# percentile score

db$p_percentil_range <- with (db, ifelse (
  db$score >=  25, "> 99" , ifelse (
    db$score >= 24, "99" , ifelse (
      db$score >= 23, "98" , ifelse (
        db$score >= 22, "95-97" , ifelse (
          
          db$score >= 20 , "82-89" , ifelse (
            db$score >= 18 , "72-81" , ifelse (
              db$score >= 16 , "60-71" , ifelse (
                db$score >= 13 , "41-59" , ifelse (
                  db$score >= 11 , "29-40" , ifelse (
                    db$score >= 10, "19-28" , ifelse (
                      db$score >= 8, "11-18" , ifelse (
                        db$score >= 7, "6-10" , ifelse (
                          db$score >= 6, "3-5" , ifelse (
                            
                            db$score >= 5, "1" , ifelse (
                              db$score <= 4, "<1" , NA )))))))))))))))) 
    
    
  }
  
  
  ############################TABLE 7####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$p_scale_score <- with ( db, ifelse (
      db$score >=  26, 18, ifelse (
        db$score >= 24 , 17, ifelse (
          db$score >= 23, 16, ifelse (
            db$score >= 21 , 15, ifelse (
              db$score >= 20, 14, ifelse (
                db$score >= 18 , 13, ifelse (
                  db$score >= 17, 12, ifelse (
                    db$score >= 15 , 11, ifelse (
                      db$score >= 12 , 10, ifelse (
                        db$score >= 10 , 9, ifelse (
                          db$score >= 8, 8, ifelse (
                            db$score >= 7, 7, ifelse (
                              db$score >= 6, 6, ifelse (
                                db$score >= 5, 5, ifelse (
                                  db$score >= 4, 4, ifelse (
                                    
                                    db$score <= 3, 2, NA )))))))))))))))))

# percentile score

db$p_percentil_range <- with (db, ifelse (
  db$score >=  26, "> 99" , ifelse (
    db$score >= 24 , "99" , ifelse (
      db$score >= 23, "98" , ifelse (
        db$score >= 21 , "95-97" , ifelse (
          db$score >= 20, "90-94" , ifelse (
            db$score >= 18 , "82-89" , ifelse (
              db$score >= 17, "72-81" , ifelse (
                db$score >= 15 , "60-71" , ifelse (
                  db$score >= 12 , "41-59" , ifelse (
                    db$score >= 10 , "29-40" , ifelse (
                      db$score >= 8, "19-28" , ifelse (
                        db$score >= 7, "11-18" , ifelse (
                          db$score >= 6, "6-10" , ifelse (
                            db$score >= 5, "3-5" , ifelse (
                              db$score >= 4, "2" , ifelse (
                                
                                db$score <= 3, "<1" , NA )))))))))))))))))
    
  }
  
  
  ##############################TABLE 8##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    db$p_scale_score <- with ( db, ifelse (
      db$score >=  26, 18, ifelse (
        db$score >= 25, 17, ifelse (
          db$score >= 24, 16, ifelse (
            db$score >= 22 , 15, ifelse (
              db$score >= 20 , 14, ifelse (
                db$score >= 18 , 13, ifelse (
                  db$score >= 17, 12, ifelse (
                    db$score >= 14 , 11, ifelse (
                      db$score >= 11 , 10, ifelse (
                        db$score >= 10, 9, ifelse (
                          db$score >= 8, 8, ifelse (
                            db$score >= 7, 7, ifelse (
                              db$score >= 6, 6, ifelse (
                                db$score >= 5, 5, ifelse (
                                  
                                  db$score >= 4, 3, ifelse (
                                    db$score <= 3, 2, NA )))))))))))))))))

# percentile score

db$p_percentil_range <- with (db, ifelse (
  db$score >=  26, "> 99" , ifelse (
    db$score >= 25, "99" , ifelse (
      db$score >= 24, "98" , ifelse (
        db$score >= 22 , "95-97" , ifelse (
          db$score >= 20 , "90-94" , ifelse (
            db$score >= 18 , "82-89" , ifelse (
              db$score >= 17, "72-81" , ifelse (
                db$score >= 14 , "60-71" , ifelse (
                  db$score >= 11 , "41-59" , ifelse (
                    db$score >= 10, "29-40" , ifelse (
                      db$score >= 8, "19-28" , ifelse (
                        db$score >= 7, "11-18" , ifelse (
                          db$score >= 6, "6-10" , ifelse (
                            db$score >= 5, "3-5" , ifelse (
                              
                              db$score >= 4, "1" , ifelse (
                                db$score <= 3, "<1" , NA )))))))))))))))))
    
    
  }
  
  
  ##############################TABLE 9##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    db$p_scale_score <- with ( db, ifelse (
      db$score >=  26, 18, ifelse (
        db$score >= 25, 17, ifelse (
          db$score >= 24, 16, ifelse (
            db$score >= 22 , 15, ifelse (
              db$score >= 20 , 14, ifelse (
                db$score >= 18 , 13, ifelse (
                  db$score >= 17, 12, ifelse (
                    db$score >= 14 , 11, ifelse (
                      db$score >= 11 , 10, ifelse (
                        db$score >= 10, 9, ifelse (
                          db$score >= 8, 8, ifelse (
                            db$score >= 7, 7, ifelse (
                              db$score >= 6, 6, ifelse (
                                db$score >= 5, 5, ifelse (
                                  
                                  db$score >= 4, 3, ifelse (
                                    db$score <= 3, 2, NA )))))))))))))))))

# percentile score

db$p_percentil_range <- with (db, ifelse (
  db$score >=  26, "> 99" , ifelse (
    db$score >= 25, "99" , ifelse (
      db$score >= 24, "98" , ifelse (
        db$score >= 22 , "95-97" , ifelse (
          db$score >= 20 , "90-94" , ifelse (
            db$score >= 18 , "82-89" , ifelse (
              db$score >= 17, "72-81" , ifelse (
                db$score >= 14 , "60-71" , ifelse (
                  db$score >= 11 , "41-59" , ifelse (
                    db$score >= 10, "29-40" , ifelse (
                      db$score >= 8, "19-28" , ifelse (
                        db$score >= 7, "11-18" , ifelse (
                          db$score >= 6, "6-10" , ifelse (
                            db$score >= 5, "3-5" , ifelse (
                              
                              db$score >= 4, "1" , ifelse (
                                db$score <= 3, "<1" , NA ))))))))))))))))) 
    
    
  }
  
  
  
  ##############################TABLE 10##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    db$p_scale_score <- with ( db, ifelse (
      db$score >=  24, 18, ifelse (
        db$score >= 22 , 17, ifelse (
          db$score >= 21, 16, ifelse (
            db$score >= 20, 15, ifelse (
              db$score >= 19, 14, ifelse (
                db$score >= 17 , 13, ifelse (
                  db$score >= 14 , 12, ifelse (
                    db$score >= 13, 11, ifelse (
                      db$score >= 10 , 10, ifelse (
                        db$score >= 8, 9, ifelse (
                          
                          db$score >= 7, 7, ifelse (
                            db$score >= 5, 6, ifelse (
                              db$score >= 4, 5, ifelse (
                                
                                db$score >= 3, 3, ifelse (
                                  db$score <= 2, 2, NA ))))))))))))))))

# percentile score

db$p_percentil_range <- with (db, ifelse (
  db$score >=  24, "> 99" , ifelse (
    db$score >= 22 , "99" , ifelse (
      db$score >= 21, "98" , ifelse (
        db$score >= 20, "95-97" , ifelse (
          db$score >= 19, "90-94" , ifelse (
            db$score >= 17 , "82-89" , ifelse (
              db$score >= 14 , "72-81" , ifelse (
                db$score >= 13, "60-71" , ifelse (
                  db$score >= 10 , "41-59" , ifelse (
                    db$score >= 8, "29-40" , ifelse (
                      
                      db$score >= 7, "11-18" , ifelse (
                        db$score >= 5, "6-10" , ifelse (
                          db$score >= 4, "3-5" , ifelse (
                            
                            db$score >= 3, "1" , ifelse (
                              db$score <= 2, "<1" , NA ))))))))))))))))
    
    
    
  }
  
  
  ##############################TABLE 11##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    db$p_scale_score <- with ( db, ifelse (
      db$score >=  21, 18, ifelse (
        
        
        db$score >= 20, 15, ifelse (
          db$score >= 19, 14, ifelse (
            db$score >= 15 , 13, ifelse (
              db$score >= 14, 12, ifelse (
                db$score >= 11 , 11, ifelse (
                  db$score >= 9, 10, ifelse (
                    db$score >= 8, 9, ifelse (
                      db$score >= 7, 8, ifelse (
                        
                        db$score >= 5, 6, ifelse (
                          
                          
                          db$score >= 4, 3, ifelse (
                            db$score <= 3, 2, NA )))))))))))))

# percentile score

db$p_percentil_range <- with (db, ifelse (
  db$score >=  21, "> 99" , ifelse (
    
    
    db$score >= 20, "95-97" , ifelse (
      db$score >= 19, "90-94" , ifelse (
        db$score >= 15 , "82-89" , ifelse (
          db$score >= 14, "72-81" , ifelse (
            db$score >= 11 , "60-71" , ifelse (
              db$score >= 9, "41-59" , ifelse (
                db$score >= 8, "29-40" , ifelse (
                  db$score >= 7, "19-28" , ifelse (
                    
                    db$score >= 5, "6-10" , ifelse (
                      
                      
                      db$score >= 4, "1" , ifelse (
                        db$score <= 3, "<1" , NA )))))))))))))
    
    
  }
  
  
  #############################TABLE 12###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    db$p_scale_score <- with ( db, ifelse (
      db$score >=  19, 18, ifelse (
        
        
        
        db$score >= 17 , 14, ifelse (
          db$score >= 15, 13, ifelse (
            db$score >= 13 , 12, ifelse (
              db$score >= 10 , 11, ifelse (
                db$score >= 9, 10, ifelse (
                  db$score >= 8, 9, ifelse (
                    db$score >= 7, 8, ifelse (
                      
                      db$score >= 6, 6, ifelse (
                        db$score >= 5, 5, ifelse (
                          db$score >= 4, 4, ifelse (
                            
                            db$score <= 3, 2, NA )))))))))))))

# percentile score

db$p_percentil_range <- with (db, ifelse (
  db$score >=  19, "> 99" , ifelse (
    
    
    
    db$score >= 17 , "90-94" , ifelse (
      db$score >= 15, "82-89" , ifelse (
        db$score >= 13 , "72-81" , ifelse (
          db$score >= 10 , "60-71" , ifelse (
            db$score >= 9, "41-59" , ifelse (
              db$score >= 8, "29-40" , ifelse (
                db$score >= 7, "19-28" , ifelse (
                  
                  db$score >= 6, "6-10" , ifelse (
                    db$score >= 5, "3-5" , ifelse (
                      db$score >= 4, "2" , ifelse (
                        
                        db$score <= 3, "<1" , NA ))))))))))))) 
    
    
  }
  
  
  # Educational level adjust 
  db$education_years_adj <- with(db, ifelse(
    db$education_years >= 0  & db$education_years <= 2, db$p_scale_score + 2, ifelse(
    db$education_years >= 3  & db$education_years <= 7, db$p_scale_score + 1, ifelse(
      db$education_years >= 8  & db$education_years <= 12, db$p_scale_score, ifelse(
        db$education_years >= 13  & db$education_years <= 16, db$p_scale_score - 1, ifelse(
          db$education_years >= 17  & db$education_years <= 20, db$p_scale_score - 2, ifelse(
           
            )))))))
  
  
  # NSSae
  db$NSSae_p <- db$p_scale_score - (0.22078*(db$education_years_adj-12)) 
  
  return(db)
}
