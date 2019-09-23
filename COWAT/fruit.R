# fruit

# Casals-Coll M., Sánchez-Benavides G., Quintana M., Manerob R.M., Rognonia T., 
# Calvo L., Palomo R., Aranciva F., Tamayo F. & Peña-Casanova J. (2013) Estudios normativos españoles en población adulta joven
# (proyecto NEURONORMA jóvenes): normas para los test de fluencia verbal. Neurología, 28 (1):33-40.

# 
COWAT_fruit <- function(score, age, education_years){
  
  COWAT_fruit_db <- data.frame(score = score, age = age, education_years = education_years)
  COWAT_fruit_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(COWAT_fruit_db)) {
    res <- COWAT_fruit_scale_score(score = COWAT_fruit_db[i, "score"], 
                              age = COWAT_fruit_db[i, "age"],
                              education_years = COWAT_fruit_db[i, "education_years"])
    COWAT_fruit_new <- rbind(COWAT_fruit_new, res)
  }
  
  return(COWAT_fruit_new[,c("COWAT_fruit_scale_score", "COWAT_fruit_percentil_range")])
}

COWAT_fruit_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    db$COWAT_fruit_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >=  30, 18, ifelse (
        
        db$score >= 29, 16, ifelse (
          db$score >= 27 , 15, ifelse (
            db$score >= 25 , 14, ifelse (
              db$score >= 23 , 13, ifelse (
                db$score >= 21 , 12, ifelse (
                  db$score >= 20, 11, ifelse (
                    db$score >= 18 , 10, ifelse (
                      db$score >= 17, 9, ifelse (
                        db$score >= 15 , 8, ifelse (
                          db$score >= 14, 7, ifelse (
                            db$score >= 12 , 6, ifelse (
                              
                              db$score >= 10, 4, ifelse (
                                
                                db$score <= 9, 2, NA ))))))))))))))) )

# percentile score

db$COWAT_fruit_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >=  30, "> 99" , ifelse (
    
    db$score >= 29, "98" , ifelse (
      db$score >= 27 , "95-97" , ifelse (
        db$score >= 25 , "90-94" , ifelse (
          db$score >= 23 , "82-89" , ifelse (
            db$score >= 21 , "72-81" , ifelse (
              db$score >= 20, "60-71" , ifelse (
                db$score >= 18 , "41-59" , ifelse (
                  db$score >= 17, "29-40" , ifelse (
                    db$score >= 15 , "19-28" , ifelse (
                      db$score >= 14, "11-18" , ifelse (
                        db$score >= 12 , "6-10" , ifelse (
                          
                          db$score >= 10, "2" , ifelse (
                            
                            db$score <= 9, "<1" , NA ))))))))))))))) )
    
    
    
    
    
  }
  
  
  ##################################TABLE 4##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    db$COWAT_fruit_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >=  31, 18, ifelse (
        db$score >= 30, 17, ifelse (
          db$score >= 29, 16, ifelse (
            db$score >= 27 , 15, ifelse (
              db$score >= 24 , 14, ifelse (
                db$score >= 22 , 13, ifelse (
                  db$score >= 21, 12, ifelse (
                    db$score >= 20, 11, ifelse (
                      db$score >= 18 , 10, ifelse (
                        db$score >= 17, 9, ifelse (
                          db$score >= 16, 8, ifelse (
                            db$score >= 14 , 7, ifelse (
                              db$score >= 12 , 6, ifelse (
                                
                                db$score >= 10 , 4, ifelse (
                                  
                                  db$score <= 9, 2, NA )))))))))))))))) )

# percentile score

db$COWAT_fruit_COWAT_fruit_percentil_rangescale_score <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >=  31, "> 99" , ifelse (
    db$score >= 30, "99" , ifelse (
      db$score >= 29, "98" , ifelse (
        db$score >= 27 , "95-97" , ifelse (
          db$score >= 24 , "90-94" , ifelse (
            db$score >= 22 , "82-89" , ifelse (
              db$score >= 21, "72-81" , ifelse (
                db$score >= 20, "60-71" , ifelse (
                  db$score >= 18 , "41-59" , ifelse (
                    db$score >= 17, "29-40" , ifelse (
                      db$score >= 16, "19-28" , ifelse (
                        db$score >= 14 , "11-18" , ifelse (
                          db$score >= 12 , "6-10" , ifelse (
                            
                            db$score >= 10 , "2" , ifelse (
                              
                              db$score <= 9, "<1" , NA ))))))))))))))))  )
    
    
    
    
    
  }
  
  
  #############################TABLE 5###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    db$COWAT_fruit_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >=  31, 18, ifelse (
        db$score >= 30, 17, ifelse (
          db$score >= 29, 16, ifelse (
            db$score >= 26 , 15, ifelse (
              db$score >= 24 , 14, ifelse (
                db$score >= 22 , 13, ifelse (
                  db$score >= 21, 12, ifelse (
                    db$score >= 20, 11, ifelse (
                      db$score >= 18 , 10, ifelse (
                        db$score >= 17, 9, ifelse (
                          db$score >= 15 , 8, ifelse (
                            db$score >= 13 , 7, ifelse (
                              db$score >= 12, 6, ifelse (
                                db$score >= 9, 5, ifelse (
                                  
                                  db$score >= 8, 3, ifelse (
                                    db$score <= 7, 2, NA ))))))))))))))))) )

# percentile score

db$COWAT_fruit_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >=  31, "> 99" , ifelse (
    db$score >= 30, "99" , ifelse (
      db$score >= 29, "98" , ifelse (
        db$score >= 26 , "95-97" , ifelse (
          db$score >= 24 , "90-94" , ifelse (
            db$score >= 22 , "82-89" , ifelse (
              db$score >= 21, "72-81" , ifelse (
                db$score >= 20, "60-71" , ifelse (
                  db$score >= 18 , "41-59" , ifelse (
                    db$score >= 17, "29-40" , ifelse (
                      db$score >= 15 , "19-28" , ifelse (
                        db$score >= 13 , "11-18" , ifelse (
                          db$score >= 12, "6-10" , ifelse (
                            db$score >= 9, "3-5" , ifelse (
                              
                              db$score >= 8, "1" , ifelse (
                                db$score <= 7, "<1" , NA ))))))))))))))))) )
    
    
    
  }
  
  
  ###########################TABLE 6#####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    db$COWAT_fruit_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >=  31, 18, ifelse (
        db$score >= 30, 17, ifelse (
          db$score >= 28, 16, ifelse (
            db$score >= 26 , 15, ifelse (
              db$score >= 24 , 14, ifelse (
                db$score >= 22 , 13, ifelse (
                  db$score >= 21, 12, ifelse (
                    db$score >= 20, 11, ifelse (
                      db$score >= 18 , 10, ifelse (
                        db$score >= 16 , 9, ifelse (
                          db$score >= 14 , 8, ifelse (
                            db$score >= 13, 7, ifelse (
                              db$score >= 12, 6, ifelse (
                                db$score >= 9, 5, ifelse (
                                  
                                  db$score >= 8, 3, ifelse (
                                    db$score <= 7, 2, NA ))))))))))))))))) )

# percentile score

db$COWAT_fruit_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >=  31, "> 99" , ifelse (
    db$score >= 30, "99" , ifelse (
      db$score >= 28, "98" , ifelse (
        db$score >= 26 , "95-97" , ifelse (
          db$score >= 24 , "90-94" , ifelse (
            db$score >= 22 , "82-89" , ifelse (
              db$score >= 21, "72-81" , ifelse (
                db$score >= 20, "60-71" , ifelse (
                  db$score >= 18 , "41-59" , ifelse (
                    db$score >= 16 , "29-40" , ifelse (
                      db$score >= 14 , "19-28" , ifelse (
                        db$score >= 13, "11-18" , ifelse (
                          db$score >= 12, "6-10" , ifelse (
                            db$score >= 9, "3-5" , ifelse (
                              
                              db$score >= 8, "1" , ifelse (
                                db$score <= 7, "<1" , NA ))))))))))))))))) )
    
    
  }
  
  
  ############################TABLE 7####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$COWAT_fruit_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >=  28, 18, ifelse (
        db$score >= 27, 17, ifelse (
          db$score >= 26, 16, ifelse (
            db$score >= 24 , 15, ifelse (
              db$score >= 23, 14, ifelse (
                db$score >= 22, 13, ifelse (
                  db$score >= 20 , 12, ifelse (
                    db$score >= 19, 11, ifelse (
                      db$score >= 16 , 10, ifelse (
                        db$score >= 15, 9, ifelse (
                          db$score >= 14, 8, ifelse (
                            db$score >= 12 , 7, ifelse (
                              db$score >= 10 , 6, ifelse (
                                db$score >= 8, 5, ifelse (
                                  
                                  db$score >= 7, 3, ifelse (
                                    db$score <= 6, 2, NA ))))))))))))))))) )

# percentile score

db$COWAT_fruit_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >=  28, "> 99" , ifelse (
    db$score >= 27, "99" , ifelse (
      db$score >= 26, "98" , ifelse (
        db$score >= 24 , "95-97" , ifelse (
          db$score >= 23, "90-94" , ifelse (
            db$score >= 22, "82-89" , ifelse (
              db$score >= 20 , "72-81" , ifelse (
                db$score >= 19, "60-71" , ifelse (
                  db$score >= 16 , "41-59" , ifelse (
                    db$score >= 15, "29-40" , ifelse (
                      db$score >= 14, "19-28" , ifelse (
                        db$score >= 12 , "11-18" , ifelse (
                          db$score >= 10 , "6-10" , ifelse (
                            db$score >= 8, "3-5" , ifelse (
                              
                              db$score >= 7, "1" , ifelse (
                                db$score <= 6, "<1" , NA ))))))))))))))))) )
    
  }
  
  
  ##############################TABLE 8##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    db$COWAT_fruit_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >=  28, 18, ifelse (
        db$score >= 27, 17, ifelse (
          db$score >= 26, 16, ifelse (
            db$score >= 24 , 15, ifelse (
              db$score >= 22 , 14, ifelse (
                db$score >= 21, 13, ifelse (
                  db$score >= 19 , 12, ifelse (
                    db$score >= 17 , 11, ifelse (
                      db$score >= 16, 10, ifelse (
                        db$score >= 14 , 9, ifelse (
                          db$score >= 13, 8, ifelse (
                            db$score >= 11 , 7, ifelse (
                              db$score >= 10, 6, ifelse (
                                db$score >= 9, 5, ifelse (
                                  db$score >= 7, 4, ifelse (
                                    
                                    db$score <= 6, 2, NA ))))))))))))))))) )

# percentile score

db$COWAT_fruit_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >=  28, "> 99" , ifelse (
    db$score >= 27, "99" , ifelse (
      db$score >= 26, "98" , ifelse (
        db$score >= 24 , "95-97" , ifelse (
          db$score >= 22 , "90-94" , ifelse (
            db$score >= 21, "82-89" , ifelse (
              db$score >= 19 , "72-81" , ifelse (
                db$score >= 17 , "60-71" , ifelse (
                  db$score >= 16, "41-59" , ifelse (
                    db$score >= 14 , "29-40" , ifelse (
                      db$score >= 13, "19-28" , ifelse (
                        db$score >= 11 , "11-18" , ifelse (
                          db$score >= 10, "6-10" , ifelse (
                            db$score >= 9, "3-5" , ifelse (
                              db$score >= 7, "2" , ifelse (
                                
                                db$score <= 6, "<1" , NA )))))))))))))))))  )
    
    
  }
  
  
  ##############################TABLE 9##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    db$COWAT_fruit_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >=  27, 18, ifelse (
        db$score >= 26, 17, ifelse (
          
          db$score >= 22 , 15, ifelse (
            db$score >= 21, 14, ifelse (
              db$score >= 19 , 13, ifelse (
                db$score >= 18, 12, ifelse (
                  db$score >= 17, 11, ifelse (
                    db$score >= 15 , 10, ifelse (
                      db$score >= 14, 9, ifelse (
                        db$score >= 13, 8, ifelse (
                          db$score >= 10 , 7, ifelse (
                            db$score >= 9, 6, ifelse (
                              db$score >= 8, 5, ifelse (
                                db$score >= 7, 4, ifelse (
                                  db$score >= 6, 3, ifelse (
                                    db$score <= 5, 2, NA ))))))))))))))))) )

# percentile score

db$COWAT_fruit_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >=  27, "> 99" , ifelse (
    db$score >= 26, "99" , ifelse (
      
      db$score >= 22 , "95-97" , ifelse (
        db$score >= 21, "90-94" , ifelse (
          db$score >= 19 , "82-89" , ifelse (
            db$score >= 18, "72-81" , ifelse (
              db$score >= 17, "60-71" , ifelse (
                db$score >= 15 , "41-59" , ifelse (
                  db$score >= 14, "29-40" , ifelse (
                    db$score >= 13, "19-28" , ifelse (
                      db$score >= 10 , "11-18" , ifelse (
                        db$score >= 9, "6-10" , ifelse (
                          db$score >= 8, "3-5" , ifelse (
                            db$score >= 7, "2" , ifelse (
                              db$score >= 6, "1" , ifelse (
                                db$score <= 5, "<1" , NA ))))))))))))))))) )
    
    
  }
  
  
  
  ##############################TABLE 10##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    db$COWAT_fruit_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >=  26, 18, ifelse (
        db$score >= 24 , 17, ifelse (
          
          db$score >= 22 , 15, ifelse (
            db$score >= 20 , 14, ifelse (
              db$score >= 19, 13, ifelse (
                db$score >= 18, 12, ifelse (
                  db$score >= 16 , 11, ifelse (
                    db$score >= 14 , 10, ifelse (
                      db$score >= 13, 9, ifelse (
                        db$score >= 11 , 8, ifelse (
                          db$score >= 10, 7, ifelse (
                            db$score >= 8, 6, ifelse (
                              db$score >= 7, 5, ifelse (
                                db$score >= 6, 4, ifelse (
                                  
                                  db$score <= 5, 2, NA )))))))))))))))) )

# percentile score

db$COWAT_fruit_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >=  26, "> 99" , ifelse (
    db$score >= 24 , "99" , ifelse (
      
      db$score >= 22 , "95-97" , ifelse (
        db$score >= 20 , "90-94" , ifelse (
          db$score >= 19, "82-89" , ifelse (
            db$score >= 18, "72-81" , ifelse (
              db$score >= 16 , "60-71" , ifelse (
                db$score >= 14 , "41-59" , ifelse (
                  db$score >= 13, "29-40" , ifelse (
                    db$score >= 11 , "19-28" , ifelse (
                      db$score >= 10, "11-18" , ifelse (
                        db$score >= 8, "6-10" , ifelse (
                          db$score >= 7, "3-5" , ifelse (
                            db$score >= 6, "2" , ifelse (
                              
                              db$score <= 5, "<1" , NA )))))))))))))))) )
    
    
    
  }
  
  
  ##############################TABLE 11##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    db$COWAT_fruit_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >=  20, 18, ifelse (
        
        db$score >= 19, 16, ifelse (
          
          db$score >= 18, 14, ifelse (
            
            db$score >= 16 , 12, ifelse (
              db$score >= 15, 11, ifelse (
                db$score >= 13 , 10, ifelse (
                  
                  db$score >= 11 , 8, ifelse (
                    db$score >= 10, 7, ifelse (
                      db$score >= 8, 6, ifelse (
                        db$score >= 7, 5, ifelse (
                          
                          db$score >= 6, 3, ifelse (
                            db$score <= 5, 2, NA ))))))))))))) )

# percentile score

db$COWAT_fruit_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >=  20, "> 99" , ifelse (
    
    db$score >= 19, "98" , ifelse (
      
      db$score >= 18, "90-94" , ifelse (
        
        db$score >= 16 , "72-81" , ifelse (
          db$score >= 15, "60-71" , ifelse (
            db$score >= 13 , "41-59" , ifelse (
              
              db$score >= 11 , "19-28" , ifelse (
                db$score >= 10, "11-18" , ifelse (
                  db$score >= 8, "6-10" , ifelse (
                    db$score >= 7, "3-5" , ifelse (
                      
                      db$score >= 6, "1" , ifelse (
                        db$score <= 5, "<1" , NA ))))))))))))) )
    
    
  }
  
  
  #############################TABLE 12###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    db$COWAT_fruit_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >=  20, 18, ifelse (
        
        db$score >= 19, 16, ifelse (
          
          db$score >= 18, 14, ifelse (
            db$score >= 17, 13, ifelse (
              db$score >= 16, 12, ifelse (
                db$score >= 14 , 11, ifelse (
                  db$score >= 13, 10, ifelse (
                    db$score >= 12, 9, ifelse (
                      db$score >= 10 , 8, ifelse (
                        db$score >= 9, 7, ifelse (
                          db$score >= 8, 6, ifelse (
                            db$score >= 7, 5, ifelse (
                              
                              
                              db$score <= 6, 2, NA )))))))))))))) )

# percentile score

db$COWAT_fruit_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >=  20, "> 99" , ifelse (
    
    db$score >= 19, "98" , ifelse (
      
      db$score >= 18, "90-94" , ifelse (
        db$score >= 17, "82-89" , ifelse (
          db$score >= 16, "72-81" , ifelse (
            db$score >= 14 , "60-71" , ifelse (
              db$score >= 13, "41-59" , ifelse (
                db$score >= 12, "29-40" , ifelse (
                  db$score >= 10 , "19-28" , ifelse (
                    db$score >= 9, "11-18" , ifelse (
                      db$score >= 8, "6-10" , ifelse (
                        db$score >= 7, "3-5" , ifelse (
                          
                          
                          db$score <= 6, "<1" , NA )))))))))))))) )
    
    
  }
  
  
  # 
  db$COWAT_fruit_sex_adj <- with(db, ifelse(
    db$education_years >= 0  & db$education_years <= 20, db$COWAT_fruit_scale_score + 2, ifelse(
    db$education_years >= 0  & db$education_years <= 3, db$COWAT_fruit_scale_score + 1, ifelse(
      db$education_years >= 4  & db$education_years <= 8, db$COWAT_fruit_scale_score, ifelse(
        db$education_years >= 9  & db$education_years <= 12, db$COWAT_fruit_scale_score - 1, ifelse(
          db$education_years >= 13  & db$education_years <= 17, db$COWAT_fruit_scale_score - 2, ifelse(
            
            )))))))
  
  
  # NSSae
 db$COWAT_fruit_NSSas <- db$COWAT_fruit_scale_score - (-0.21832*(db$COWAT_fruit_education_years_adj-12)) ####CAMBIAR
  
  return(db)
}
