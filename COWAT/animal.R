# semantic animal

# Casals-Coll M., Sánchez-Benavides G., Quintana M., Manerob R.M., Rognonia T., 
# Calvo L., Palomo R., Aranciva F., Tamayo F. & Peña-Casanova J. (2013) Estudios normativos españoles en población adulta joven
# (proyecto NEURONORMA jóvenes): normas para los test de fluencia verbal. Neurología, 28 (1):33-40.

# 
COWAT_animal <- function(score, age, education_years){
  
  COWAT_animal_db <- data.frame(score = score, age = age, education_years = education_years)
  COWAT_animal_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(COWAT_animal_db)) {
    res <- COWAT_animal_scale_score(score = COWAT_animal_db[i, "score"], 
                              age = COWAT_animal_db[i, "age"],
                              education_years = COWAT_animal_db[i, "education_years"])
    COWAT_animal_new <- rbind(COWAT_animal_new, res)
  }
  
  return(COWAT_animal_new[,c("COWAT_animal_scale_score", "COWAT_animal_percentil_range", "COWAT_animal_NSSae")])
}

COWAT_animal_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    db$COWAT_animal_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >=  34, 18, ifelse (
        db$score >= 33, 17, ifelse (
          
          db$score >= 32, 15, ifelse (
            db$score >= 30 , 14, ifelse (
              db$score >= 27 , 13, ifelse (
                db$score >= 24 , 12, ifelse (
                  db$score >= 22 , 11, ifelse (
                    db$score >= 20 , 10, ifelse (
                      db$score >= 18 , 9, ifelse (
                        db$score >= 16 , 8, ifelse (
                          db$score >= 15, 7, ifelse (
                            db$score >= 14, 6, ifelse (
                              db$score >= 13, 5, ifelse (
                                db$score >= 11 , 4, ifelse (
                                  
                                  db$score <= 10, 2, NA )))))))))))))))) )

# percentile score

db$COWAT_animal_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >=  34, "> 99" , ifelse (
    db$score >= 33, "99" , ifelse (
      
      db$score >= 32, "95-97" , ifelse (
        db$score >= 30 , "90-94" , ifelse (
          db$score >= 27 , "82-89" , ifelse (
            db$score >= 24 , "72-81" , ifelse (
              db$score >= 22 , "60-71" , ifelse (
                db$score >= 20 , "41-59" , ifelse (
                  db$score >= 18 , "29-40" , ifelse (
                    db$score >= 16 , "19-28" , ifelse (
                      db$score >= 15, "11-18" , ifelse (
                        db$score >= 14, "6-10" , ifelse (
                          db$score >= 13, "3-5" , ifelse (
                            db$score >= 11 , "2" , ifelse (
                              
                              db$score <= 10, "<1" , NA )))))))))))))))) )
    
    
    
    
    
  }
  
  
  ##################################TABLE 4##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    db$COWAT_animal_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >=  35, 18, ifelse (
        db$score >= 34, 17, ifelse (
          db$score >= 33, 16, ifelse (
            
            db$score >= 30 , 14, ifelse (
              db$score >= 27 , 13, ifelse (
                db$score >= 24 , 12, ifelse (
                  db$score >= 22 , 11, ifelse (
                    db$score >= 19 , 10, ifelse (
                      db$score >= 18, 9, ifelse (
                        db$score >= 16 , 8, ifelse (
                          db$score >= 15, 7, ifelse (
                            db$score >= 13, 6, ifelse (
                              db$score >= 11 , 5, ifelse (
                                db$score >= 8, 4, ifelse (
                                  
                                  db$score <= 7, 2, NA )))))))))))))))) )

# percentile score

db$COWAT_animal_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >=  35, "> 99" , ifelse (
    db$score >= 34, "99" , ifelse (
      db$score >= 33, "98" , ifelse (
        
        db$score >= 30 , "90-94" , ifelse (
          db$score >= 27 , "82-89" , ifelse (
            db$score >= 24 , "72-81" , ifelse (
              db$score >= 22 , "60-71" , ifelse (
                db$score >= 19 , "41-59" , ifelse (
                  db$score >= 18, "29-40" , ifelse (
                    db$score >= 16 , "19-28" , ifelse (
                      db$score >= 15, "11-18" , ifelse (
                        db$score >= 13, "6-10" , ifelse (
                          db$score >= 11 , "3-5" , ifelse (
                            db$score >= 8, "2" , ifelse (
                              
                              db$score <= 7, "<1" , NA )))))))))))))))) )
    
    
    
    
    
  }
  
  
  #############################TABLE 5###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    db$COWAT_animal_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 35, 18, ifelse (
        db$score >= 34, 17, ifelse (
          db$score >= 33, 16, ifelse (
            
            db$score >= 30 , 14, ifelse (
              db$score >= 27 , 13, ifelse (
                db$score >= 24 , 12, ifelse (
                  db$score >= 21 , 11, ifelse (
                    db$score >= 19 , 10, ifelse (
                      db$score >= 17 , 9, ifelse (
                        db$score >= 15 , 8, ifelse (
                          db$score >= 14, 7, ifelse (
                            db$score >= 12 , 6, ifelse (
                              db$score >= 11, 5, ifelse (
                                db$score >= 8, 4, ifelse (
                                  
                                  db$score <= 7, 2, NA )))))))))))))))) )

# percentile score

db$COWAT_animal_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >= 35, "> 99" , ifelse (
    db$score >= 34, "99" , ifelse (
      db$score >= 33, "98" , ifelse (
        
        db$score >= 30 , "90-94" , ifelse (
          db$score >= 27 , "82-89" , ifelse (
            db$score >= 24 , "72-81" , ifelse (
              db$score >= 21 , "60-71" , ifelse (
                db$score >= 19 , "41-59" , ifelse (
                  db$score >= 17 , "29-40" , ifelse (
                    db$score >= 15 , "19-28" , ifelse (
                      db$score >= 14, "11-18" , ifelse (
                        db$score >= 12 , "6-10" , ifelse (
                          db$score >= 11, "3-5" , ifelse (
                            db$score >= 8, "2" , ifelse (
                              
                              db$score <= 7, "<1" , NA )))))))))))))))) )
    
    
    
  }
  
  
  ###########################TABLE 6#####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    db$COWAT_animal_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >=  35, 18, ifelse (
        db$score >= 34, 17, ifelse (
          db$score >= 31 , 16, ifelse (
            
            db$score >= 27 , 14, ifelse (
              db$score >= 25 , 13, ifelse (
                db$score >= 23 , 12, ifelse (
                  db$score >= 21 , 11, ifelse (
                    db$score >= 18 , 10, ifelse (
                      db$score >= 17, 9, ifelse (
                        db$score >= 15 , 8, ifelse (
                          db$score >= 13 , 7, ifelse (
                            db$score >= 12, 6, ifelse (
                              db$score >= 11, 5, ifelse (
                                
                                db$score >= 8, 3, ifelse (
                                  db$score <= 7, 2, NA )))))))))))))))) )

# percentile score

db$COWAT_animal_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >=  35, "> 99" , ifelse (
    db$score >= 34, "99" , ifelse (
      db$score >= 31 , "98" , ifelse (
        
        db$score >= 27 , "90-94" , ifelse (
          db$score >= 25 , "82-89" , ifelse (
            db$score >= 23 , "72-81" , ifelse (
              db$score >= 21 , "60-71" , ifelse (
                db$score >= 18 , "41-59" , ifelse (
                  db$score >= 17, "29-40" , ifelse (
                    db$score >= 15 , "19-28" , ifelse (
                      db$score >= 13 , "11-18" , ifelse (
                        db$score >= 12, "6-10" , ifelse (
                          db$score >= 11, "3-5" , ifelse (
                            
                            db$score >= 8, "1" , ifelse (
                              db$score <= 7, "<1" , NA )))))))))))))))) )
    
    
  }
  
  
  ############################TABLE 7####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$COWAT_animal_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >=  34, 18, ifelse (
        db$score >= 30 , 17, ifelse (
          
          db$score >= 28 , 15, ifelse (
            db$score >= 25 , 14, ifelse (
              db$score >= 24, 13, ifelse (
                db$score >= 22 , 12, ifelse (
                  db$score >= 20 , 11, ifelse (
                    db$score >= 17 , 10, ifelse (
                      db$score >= 16, 9, ifelse (
                        db$score >= 14 , 8, ifelse (
                          db$score >= 13, 7, ifelse (
                            db$score >= 12, 6, ifelse (
                              db$score >= 9, 5, ifelse (
                                
                                db$score >= 8, 3, ifelse (
                                  db$score <= 7, 2, NA )))))))))))))))) )

# percentile score

db$COWAT_animal_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >=  34, "> 99" , ifelse (
    db$score >= 30 , "99" , ifelse (
      
      db$score >= 28 , "95-97" , ifelse (
        db$score >= 25 , "90-94" , ifelse (
          db$score >= 24, "82-89" , ifelse (
            db$score >= 22 , "72-81" , ifelse (
              db$score >= 20 , "60-71" , ifelse (
                db$score >= 17 , "41-59" , ifelse (
                  db$score >= 16, "29-40" , ifelse (
                    db$score >= 14 , "19-28" , ifelse (
                      db$score >= 13, "11-18" , ifelse (
                        db$score >= 12, "6-10" , ifelse (
                          db$score >= 9, "3-5" , ifelse (
                            
                            db$score >= 8, "1" , ifelse (
                              db$score <= 7, "<1" , NA )))))))))))))))) )
    
  }
  
  
  ##############################TABLE 8##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    db$COWAT_animal_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >=  34, 18, ifelse (
        db$score >= 31 , 17, ifelse (
          db$score >= 30, 16, ifelse (
            db$score >= 27 , 15, ifelse (
              db$score >= 25 , 14, ifelse (
                db$score >= 24, 13, ifelse (
                  db$score >= 22 , 12, ifelse (
                    db$score >= 19 , 11, ifelse (
                      db$score >= 17 , 10, ifelse (
                        db$score >= 15 , 9, ifelse (
                          db$score >= 14, 8, ifelse (
                            db$score >= 12 , 7, ifelse (
                              db$score >= 11, 6, ifelse (
                                db$score >= 9, 5, ifelse (
                                  
                                  db$score >= 8, 3, ifelse (
                                    db$score <= 7, 2, NA ))))))))))))))))) )

# percentile score

db$COWAT_animal_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >=  34, "> 99" , ifelse (
    db$score >= 31 , "99" , ifelse (
      db$score >= 30, "98" , ifelse (
        db$score >= 27 , "95-97" , ifelse (
          db$score >= 25 , "90-94" , ifelse (
            db$score >= 24, "82-89" , ifelse (
              db$score >= 22 , "72-81" , ifelse (
                db$score >= 19 , "60-71" , ifelse (
                  db$score >= 17 , "41-59" , ifelse (
                    db$score >= 15 , "29-40" , ifelse (
                      db$score >= 14, "19-28" , ifelse (
                        db$score >= 12 , "11-18" , ifelse (
                          db$score >= 11, "6-10" , ifelse (
                            db$score >= 9, "3-5" , ifelse (
                              
                              db$score >= 8, "1" , ifelse (
                                db$score <= 7, "<1" , NA )))))))))))))))))  )
    
    
  }
  
  
  ##############################TABLE 9##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    db$COWAT_animal_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 31, 18, ifelse (
        db$score >= 30, 17, ifelse (
          db$score >= 29, 16, ifelse (
            db$score >= 27, 15, ifelse (
              db$score >= 25, 14, ifelse (
                db$score >= 24, 13, ifelse (
                  db$score >= 21, 12, ifelse (
                    db$score >= 19, 11, ifelse (
                      db$score >= 17, 10, ifelse (
                        db$score >= 15, 9, ifelse (
                          db$score >= 14, 8, ifelse (
                            db$score >= 12, 7, ifelse (
                              db$score >= 11, 6, ifelse (
                                db$score >= 9, 5, ifelse (
                                  db$score >= 8, 4, ifelse (
                                    db$score >= 7, 3, ifelse (
                                      db$score <= 6, 2, NA )))))))))))))))))) )
    
    # percentile score
    
    db$COWAT_animal_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 31, "> 99" , ifelse (
        db$score >= 30, "99" , ifelse (
          db$score >= 29, "98" , ifelse (
            db$score >= 27, "95-97" , ifelse (
              db$score >= 25, "90-94" , ifelse (
                db$score >= 24, "82-89" , ifelse (
                  db$score >= 21, "72-81" , ifelse (
                    db$score >= 19, "60-71" , ifelse (
                      db$score >= 17, "41-59" , ifelse (
                        db$score >= 15, "29-40" , ifelse (
                          db$score >= 14, "19-28" , ifelse (
                            db$score >= 12, "11-18" , ifelse (
                              db$score >= 11, "6-10" , ifelse (
                                db$score >= 9, "3-5" , ifelse (
                                  db$score >= 8, "2" , ifelse (
                                    db$score >= 7, "1" , ifelse (
                                      db$score <= 6, "<1" , NA ))))))))))))))))))  )
    
    
  }
  
  
  
  ##############################TABLE 10##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    db$COWAT_animal_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >=  30, 18, ifelse (
        db$score >= 29, 17, ifelse (
          db$score >= 28, 16, ifelse (
            db$score >= 27, 15, ifelse (
              db$score >= 25 , 14, ifelse (
                db$score >= 22 , 13, ifelse (
                  db$score >= 20 , 12, ifelse (
                    db$score >= 19, 11, ifelse (
                      db$score >= 16 , 10, ifelse (
                        db$score >= 14 , 9, ifelse (
                          db$score >= 13, 8, ifelse (
                            db$score >= 11 , 7, ifelse (
                              db$score >= 10, 6, ifelse (
                                db$score >= 8, 5, ifelse (
                                  db$score >= 7, 4, ifelse (
                                    
                                    db$score <= 6, 2, NA ))))))))))))))))) )

# percentile score

db$COWAT_animal_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >=  30, "> 99" , ifelse (
    db$score >= 29, "99" , ifelse (
      db$score >= 28, "98" , ifelse (
        db$score >= 27, "95-97" , ifelse (
          db$score >= 25 , "90-94" , ifelse (
            db$score >= 22 , "82-89" , ifelse (
              db$score >= 20 , "72-81" , ifelse (
                db$score >= 19, "60-71" , ifelse (
                  db$score >= 16 , "41-59" , ifelse (
                    db$score >= 14 , "29-40" , ifelse (
                      db$score >= 13, "19-28" , ifelse (
                        db$score >= 11 , "11-18" , ifelse (
                          db$score >= 10, "6-10" , ifelse (
                            db$score >= 8, "3-5" , ifelse (
                              db$score >= 7, "2" , ifelse (
                                
                                db$score <= 6, "<1" , NA ))))))))))))))))) )
    
    
    
  }
  
  
  ##############################TABLE 11##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    db$COWAT_animal_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >=  28, 18, ifelse (
        
        
        db$score >= 26 , 15, ifelse (
          db$score >= 22 , 14, ifelse (
            db$score >= 21, 13, ifelse (
              db$score >= 19 , 12, ifelse (
                db$score >= 18, 11, ifelse (
                  db$score >= 15 , 10, ifelse (
                    db$score >= 14, 9, ifelse (
                      db$score >= 12 , 8, ifelse (
                        
                        db$score >= 9, 6, ifelse (
                          db$score >= 7, 5, ifelse (
                            
                            
                            db$score <= 6, 2, NA ))))))))))))) )

# percentile score

db$COWAT_animal_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >=  28, "> 99" , ifelse (
    
    
    db$score >= 26 , "95-97" , ifelse (
      db$score >= 22 , "90-94" , ifelse (
        db$score >= 21, "82-89" , ifelse (
          db$score >= 19 , "72-81" , ifelse (
            db$score >= 18, "60-71" , ifelse (
              db$score >= 15 , "41-59" , ifelse (
                db$score >= 14, "29-40" , ifelse (
                  db$score >= 12 , "19-28" , ifelse (
                    
                    db$score >= 9, "6-10" , ifelse (
                      db$score >= 7, "3-5" , ifelse (
                        
                        
                        db$score <= 6, "<1" , NA ))))))))))))) )
    
    
  }
  
  
  #############################TABLE 12###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    db$COWAT_animal_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 23, 18, ifelse (
        
        db$score >= 21 , 16, ifelse (
          
          db$score >= 20, 14, ifelse (
            db$score >= 19, 13, ifelse (
              
              db$score >= 17 , 11, ifelse (
                db$score >= 15 , 10, ifelse (
                  db$score >= 14, 9, ifelse (
                    db$score >= 12 , 8, ifelse (
                      
                      db$score >= 10 , 6, ifelse (
                        db$score >= 9, 5, ifelse (
                          db$score >= 8, 4, ifelse (
                            
                            db$score <= 7, 2, NA ))))))))))))) )

# percentile score

db$COWAT_animal_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >= 23, "> 99" , ifelse (
    
    db$score >= 21 , "98" , ifelse (
      
      db$score >= 20, "90-94" , ifelse (
        db$score >= 19, "82-89" , ifelse (
          
          db$score >= 17 , "60-71" , ifelse (
            db$score >= 15 , "41-59" , ifelse (
              db$score >= 14, "29-40" , ifelse (
                db$score >= 12 , "19-28" , ifelse (
                  
                  db$score >= 10 , "6-10" , ifelse (
                    db$score >= 9, "3-5" , ifelse (
                      db$score >= 8, "2" , ifelse (
                        
                        db$score <= 7, "<1" , NA ))))))))))))) )
    
    
  }
  
  
  # Educational level adjust 
  db$COWAT_animal_education_years_adj <- with(db, ifelse(
    is.na(db$COWAT_animal_scale_score), NA, ifelse (
    db$education_years >= 0  & db$education_years <= 2, db$COWAT_animal_scale_score + 2, ifelse(
    db$education_years >= 3  & db$education_years <= 7, db$COWAT_animal_scale_score + 1, ifelse(
      db$education_years >= 8  & db$education_years <= 12, db$COWAT_animal_scale_score, ifelse(
        db$education_years >= 13  & db$education_years <= 16, db$COWAT_animal_scale_score - 1, ifelse(
          db$education_years >= 17  & db$education_years <= 20, db$COWAT_animal_scale_score - 2, ifelse(
            
            ))))))) )
  
  
  # NSSae
  db$COWAT_animal_NSSae <- with(db, ifelse(
    is.na(db$COWAT_animal_education_years_adj), NA, ifelse (
      !is.na(db$COWAT_animal_education_years_adj), db$COWAT_animal_scale_score - (0.20588*(db$COWAT_animal_education_years_adj-12)) )))
  
  return(db)
}
