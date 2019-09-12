# symbol digit modalities test
#
# Reference:
# Peña-Casanova, J., Quiñones-Ubeda, S., Quintana-Aparicio, M., et al., 2009. Neuronorma
# study team. Spanish multicenter normative studies (neuronorma project): norms for verbal span, 
# visuospatial span, letter and number sequencing, trail making test, and symbol digit modalities test. 
# Arch. Clin. Neuropsychol. 24 (4), 321–341.
# 


SDMT <- function(score, age, education_years){
  
  SDMT_db <- data.frame(score = score, age = age, education_years = education_years)
  SDMT_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(SDMT_db)) {
    res <- SDMT_scale_score(score = SDMT_db[i, "score"], 
                              age = SDMT_db[i, "age"],
                              education_years = SDMT_db[i, "education_years"])
    SDMT_new <- rbind(SDMT_new, res)
  }
  
  return(SDMT_new[,c("SDMT_scale_score", "SDMT_percentil_range", "SDMT_NSSae")])
}

SDMT_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    db$SDMT_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 70, 18, ifelse (
        db$score >= 69, 17, ifelse (
          db$score >= 67, 16, ifelse (
            db$score >= 63, 15, ifelse (
              db$score >= 59, 14, ifelse (
                db$score >= 56, 13, ifelse (
                  db$score >= 54, 12, ifelse (
                    db$score >= 52, 11, ifelse (
                      db$score >= 48, 10, ifelse (
                        db$score >= 36, 9, ifelse (
                          db$score >= 33, 8, ifelse (
                            db$score >= 27, 7, ifelse (
                              db$score >= 21, 6, ifelse (
                                db$score >= 17, 5, ifelse (
                                  db$score >= 12, 4, ifelse (
                                    db$score >= 6, 3, ifelse (
                                      db$score <= 5, 2, NA )))))))))))))))))))
    
    # percentile score
    
    db$SDMT_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 70, "> 99" , ifelse (
        db$score >= 69, "99" , ifelse (
          db$score >= 67, "98" , ifelse (
            db$score >= 63, "95-97" , ifelse (
              db$score >= 59, "90-94" , ifelse (
                db$score >= 56, "82-89" , ifelse (
                  db$score >= 54, "72-81" , ifelse (
                    db$score >= 52, "60-71" , ifelse (
                      db$score >= 48, "41-59" , ifelse (
                        db$score >= 36, "29-40" , ifelse (
                          db$score >= 33, "19-28" , ifelse (
                            db$score >= 27, "11-18" , ifelse (
                              db$score >= 21, "6-10" , ifelse (
                                db$score >= 17, "3-5" , ifelse (
                                  db$score >= 12, "2" , ifelse (
                                    db$score >= 6, "1" , ifelse (
                                      db$score <= 5, "<1" , NA )))))))))))))))))))
    
    
    
    
    
  }
  
  
  ################################## TABLE 4 ##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    db$SDMT_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 68, 18, ifelse (
        db$score >= 67, 17, ifelse (
          db$score >= 63, 16, ifelse (
            db$score >= 59, 15, ifelse (
              db$score >= 57, 14, ifelse (
                db$score >= 55, 13, ifelse (
                  db$score >= 52, 12, ifelse (
                    db$score >= 49, 11, ifelse (
                      db$score >= 44, 10, ifelse (
                        db$score >= 34, 9, ifelse (
                          db$score >= 30, 8, ifelse (
                            db$score >= 24, 7, ifelse (
                              db$score >= 19, 6, ifelse (
                                db$score >= 16, 5, ifelse (
                                  db$score >= 11, 4, ifelse (
                                    db$score >= 6, 3, ifelse (
                                      db$score <= 5, 2, NA )))))))))))))))))))
    
    # percentile score
    
    db$SDMT_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 68, "> 99" , ifelse (
        db$score >= 67, "99" , ifelse (
          db$score >= 63, "98" , ifelse (
            db$score >= 59, "95-97" , ifelse (
              db$score >= 57, "90-94" , ifelse (
                db$score >= 55, "82-89" , ifelse (
                  db$score >= 52, "72-81" , ifelse (
                    db$score >= 49, "60-71" , ifelse (
                      db$score >= 44, "41-59" , ifelse (
                        db$score >= 34, "29-40" , ifelse (
                          db$score >= 30, "19-28" , ifelse (
                            db$score >= 24, "11-18" , ifelse (
                              db$score >= 19, "6-10" , ifelse (
                                db$score >= 16, "3-5" , ifelse (
                                  db$score >= 11, "2" , ifelse (
                                    db$score >= 6, "1" , ifelse (
                                      db$score <= 5, "<1" , NA ))))))))))))))))))) 
    
    
    
    
    
  }
  
  
  ############################# TABLE 5 ###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    db$SDMT_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 64, 18, ifelse (
        db$score >= 63, 17, ifelse (
          db$score >= 59, 16, ifelse (
            db$score >= 58, 15, ifelse (
              db$score >= 56, 14, ifelse (
                db$score >= 53, 13, ifelse (
                  db$score >= 49, 12, ifelse (
                    db$score >= 46, 11, ifelse (
                      db$score >= 37, 10, ifelse (
                        db$score >= 33, 9, ifelse (
                          db$score >= 28, 8, ifelse (
                            db$score >= 21, 7, ifelse (
                              db$score >= 18, 6, ifelse (
                                db$score >= 15, 5, ifelse (
                                  db$score >= 11, 4, ifelse (
                                    db$score >= 6, 3, ifelse (
                                      db$score <= 5, 2, NA )))))))))))))))))))
    
    # percentile score
    
    db$SDMT_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 64, "> 99" , ifelse (
        db$score >= 63, "99" , ifelse (
          db$score >= 59, "98" , ifelse (
            db$score >= 58, "95-97" , ifelse (
              db$score >= 56, "90-94" , ifelse (
                db$score >= 53, "82-89" , ifelse (
                  db$score >= 49, "72-81" , ifelse (
                    db$score >= 46, "60-71" , ifelse (
                      db$score >= 37, "41-59" , ifelse (
                        db$score >= 33, "29-40" , ifelse (
                          db$score >= 28, "19-28" , ifelse (
                            db$score >= 21, "11-18" , ifelse (
                              db$score >= 18, "6-10" , ifelse (
                                db$score >= 15, "3-5" , ifelse (
                                  db$score >= 11, "2" , ifelse (
                                    db$score >= 6, "1" , ifelse (
                                      db$score <= 5, "<1" , NA )))))))))))))))))))
    
    
    
  }
  
  
  ########################### TABLE 6 #####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    db$SDMT_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 58, 18, ifelse (
        db$score >= 57, 17, ifelse (
          db$score >= 56, 16, ifelse (
            db$score >= 55, 15, ifelse (
              db$score >= 54, 14, ifelse (
                db$score >= 50, 13, ifelse (
                  db$score >= 46, 12, ifelse (
                    db$score >= 40, 11, ifelse (
                      db$score >= 36, 10, ifelse (
                        db$score >= 30, 9, ifelse (
                          db$score >= 24, 8, ifelse (
                            db$score >= 20, 7, ifelse (
                              db$score >= 16, 6, ifelse (
                                db$score >= 14, 5, ifelse (
                                  db$score >= 11, 4, ifelse (
                                    
                                    db$score <= 6, 2, NA ))))))))))))))))))

# percentile score

db$SDMT_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >= 58, "> 99" , ifelse (
    db$score >= 57, "99" , ifelse (
      db$score >= 56, "98" , ifelse (
        db$score >= 55, "95-97" , ifelse (
          db$score >= 54, "90-94" , ifelse (
            db$score >= 50, "82-89" , ifelse (
              db$score >= 46, "72-81" , ifelse (
                db$score >= 40, "60-71" , ifelse (
                  db$score >= 36, "41-59" , ifelse (
                    db$score >= 30, "29-40" , ifelse (
                      db$score >= 24, "19-28" , ifelse (
                        db$score >= 20, "11-18" , ifelse (
                          db$score >= 16, "6-10" , ifelse (
                            db$score >= 14, "3-5" , ifelse (
                              db$score >= 11, "2" , ifelse (
                                
                                db$score <= 6, "<1" , NA ))))))))))))))))) )
    
    
  }
  
  
  ############################ TABLE 7 ####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$SDMT_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 59, 18, ifelse (
        db$score >= 58, 17, ifelse (
          db$score >= 57, 16, ifelse (
            db$score >= 56, 15, ifelse (
              db$score >= 50, 14, ifelse (
                db$score >= 48, 13, ifelse (
                  db$score >= 44, 12, ifelse (
                    db$score >= 39, 11, ifelse (
                      db$score >= 34, 10, ifelse (
                        db$score >= 27, 9, ifelse (
                          db$score >= 24, 8, ifelse (
                            db$score >= 19, 7, ifelse (
                              db$score >= 15, 6, ifelse (
                                db$score >= 12, 5, ifelse (
                                  db$score >= 9, 4, ifelse (
                                    
                                    db$score <= 8, 2, NA ))))))))))))))))))

# percentile score

db$SDMT_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >= 59, "> 99" , ifelse (
    db$score >= 58, "99" , ifelse (
      db$score >= 57, "98" , ifelse (
        db$score >= 56, "95-97" , ifelse (
          db$score >= 50, "90-94" , ifelse (
            db$score >= 48, "82-89" , ifelse (
              db$score >= 44, "72-81" , ifelse (
                db$score >= 39, "60-71" , ifelse (
                  db$score >= 34, "41-59" , ifelse (
                    db$score >= 27, "29-40" , ifelse (
                      db$score >= 24, "19-28" , ifelse (
                        db$score >= 19, "11-18" , ifelse (
                          db$score >= 15, "6-10" , ifelse (
                            db$score >= 12, "3-5" , ifelse (
                              db$score >= 9, "2" , ifelse (
                                
                                db$score <= 8, "<1" , NA ))))))))))))))))) )
    
  }
  
  
  ############################## TABLE 8 ##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    db$SDMT_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 59, 18, ifelse (
        db$score >= 58, 17, ifelse (
          
          db$score >= 55, 15, ifelse (
            db$score >= 50, 14, ifelse (
              db$score >= 47, 13, ifelse (
                db$score >= 40, 12, ifelse (
                  db$score >= 35, 11, ifelse (
                    db$score >= 31, 10, ifelse (
                      db$score >= 26, 9, ifelse (
                        db$score >= 22, 8, ifelse (
                          db$score >= 19, 7, ifelse (
                            db$score >= 14, 6, ifelse (
                              db$score >= 11, 5, ifelse (
                                
                                
                                db$score <= 8, 2, NA ))))))))))))))))

# percentile score

db$SDMT_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >= 59, "> 99" , ifelse (
    db$score >= 58, "99" , ifelse (
      
      db$score >= 55, "95-97" , ifelse (
        db$score >= 50, "90-94" , ifelse (
          db$score >= 47, "82-89" , ifelse (
            db$score >= 40, "72-81" , ifelse (
              db$score >= 35, "60-71" , ifelse (
                db$score >= 31, "41-59" , ifelse (
                  db$score >= 26, "29-40" , ifelse (
                    db$score >= 22, "19-28" , ifelse (
                      db$score >= 19, "11-18" , ifelse (
                        db$score >= 14, "6-10" , ifelse (
                          db$score >= 11, "3-5" , ifelse (
                            
                            
                            db$score <= 8, "<1" , NA )))))))))))))))  )
    
    
  }
  
  
  ############################## TABLE 9 ##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    db$SDMT_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 59, 18, ifelse (
        db$score >= 58, 17, ifelse (
          
          db$score >= 55, 15, ifelse (
            db$score >= 50, 14, ifelse (
              db$score >= 42, 13, ifelse (
                db$score >= 38, 12, ifelse (
                  db$score >= 33, 11, ifelse (
                    db$score >= 30, 10, ifelse (
                      db$score >= 24, 9, ifelse (
                        db$score >= 21, 8, ifelse (
                          db$score >= 17, 7, ifelse (
                            db$score >= 13, 6, ifelse (
                              db$score >= 9, 5, ifelse (
                                
                                
                                db$score <= 8, 2, NA ))))))))))))))) )

# percentile score

db$SDMT_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >= 59, "> 99" , ifelse (
    db$score >= 58, "99" , ifelse (
      
      db$score >= 55, "95-97" , ifelse (
        db$score >= 50, "90-94" , ifelse (
          db$score >= 42, "82-89" , ifelse (
            db$score >= 38, "72-81" , ifelse (
              db$score >= 33, "60-71" , ifelse (
                db$score >= 30, "41-59" , ifelse (
                  db$score >= 24, "29-40" , ifelse (
                    db$score >= 21, "19-28" , ifelse (
                      db$score >= 17, "11-18" , ifelse (
                        db$score >= 13, "6-10" , ifelse (
                          db$score >= 9, "3-5" , ifelse (
                            
                            
                            db$score <= 8, "<1" , NA ))))))))))))))) )
    
    
  }
  
  
  
  ############################## TABLE 10 ##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    db$SDMT_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 59, 18, ifelse (
        db$score >= 58, 17, ifelse (
          
          db$score >= 55, 15, ifelse (
            db$score >= 49, 14, ifelse (
              db$score >= 41, 13, ifelse (
                db$score >= 34, 12, ifelse (
                  db$score >= 31, 11, ifelse (
                    db$score >= 28, 10, ifelse (
                      db$score >= 22, 9, ifelse (
                        db$score >= 19, 8, ifelse (
                          db$score >= 15, 7, ifelse (
                            db$score >= 12, 6, ifelse (
                              db$score >= 10, 5, ifelse (
                                
                                
                                db$score <= 8, 2, NA ))))))))))))))) )

# percentile score

db$SDMT_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >= 59, "> 99" , ifelse (
    db$score >= 58, "99" , ifelse (
      
      db$score >= 55, "95-97" , ifelse (
        db$score >= 49, "90-94" , ifelse (
          db$score >= 41, "82-89" , ifelse (
            db$score >= 34, "72-81" , ifelse (
              db$score >= 31, "60-71" , ifelse (
                db$score >= 28, "41-59" , ifelse (
                  db$score >= 22, "29-40" , ifelse (
                    db$score >= 19, "19-28" , ifelse (
                      db$score >= 15, "11-18" , ifelse (
                        db$score >= 12, "6-10" , ifelse (
                          db$score >= 10, "3-5" , ifelse (
                            
                            
                            db$score <= 8, "<1" , NA ))))))))))))))) )
    
    
    
  }
  
  
  ############################## TABLE 11 ##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    db$SDMT_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 47, 18, ifelse (
        
        db$score >= 46, 16, ifelse (
          db$score >= 42, 15, ifelse (
            db$score >= 40, 14, ifelse (
              db$score >= 35, 13, ifelse (
                db$score >= 33, 12, ifelse (
                  db$score >= 29, 11, ifelse (
                    db$score >= 26, 10, ifelse (
                      db$score >= 21, 9, ifelse (
                        db$score >= 19, 8, ifelse (
                          db$score >= 14, 7, ifelse (
                            db$score >= 11, 6, ifelse (
                              db$score >= 10, 5, ifelse (
                                
                                db$score >= 8, 3, ifelse (
                                  db$score <= 7, 2, NA )))))))))))))))) )

# percentile score

db$SDMT_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >= 47, "> 99" , ifelse (
    
    db$score >= 46, "98" , ifelse (
      db$score >= 42, "95-97" , ifelse (
        db$score >= 40, "90-94" , ifelse (
          db$score >= 35, "82-89" , ifelse (
            db$score >= 33, "72-81" , ifelse (
              db$score >= 29, "60-71" , ifelse (
                db$score >= 26, "41-59" , ifelse (
                  db$score >= 21, "29-40" , ifelse (
                    db$score >= 19, "19-28" , ifelse (
                      db$score >= 14, "11-18" , ifelse (
                        db$score >= 11, "6-10" , ifelse (
                          db$score >= 10, "3-5" , ifelse (
                            
                            db$score >= 8, "1" , ifelse (
                              db$score <= 7, "<1" , NA )))))))))))))))) )
    
    
  }
  
  
  ############################# TABLE 12 ###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    db$SDMT_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 43, 18, ifelse (
        
        
        db$score >= 42, 15, ifelse (
          db$score >= 40, 14, ifelse (
            db$score >= 33, 13, ifelse (
              db$score >= 30, 12, ifelse (
                db$score >= 27, 11, ifelse (
                  db$score >= 24, 10, ifelse (
                    db$score >= 20, 9, ifelse (
                      db$score >= 18, 8, ifelse (
                        db$score >= 15, 7, ifelse (
                          db$score >= 12, 6, ifelse (
                            db$score >= 11, 5, ifelse (
                              db$score >= 10, 4, ifelse (
                                
                                db$score <= 9, 2, NA ))))))))))))))) )

# percentile score

db$SDMT_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >= 43, "> 99" , ifelse (
    
    
    db$score >= 42, "95-97" , ifelse (
      db$score >= 40, "90-94" , ifelse (
        db$score >= 33, "82-89" , ifelse (
          db$score >= 30, "72-81" , ifelse (
            db$score >= 27, "60-71" , ifelse (
              db$score >= 24, "41-59" , ifelse (
                db$score >= 20, "29-40" , ifelse (
                  db$score >= 18, "19-28" , ifelse (
                    db$score >= 15, "11-18" , ifelse (
                      db$score >= 12, "6-10" , ifelse (
                        db$score >= 11, "3-5" , ifelse (
                          db$score >= 10, "2" , ifelse (
                            
                            db$score <= 9, "<1" , NA ))))))))))))))) )
    
    
  }
  
  
  # Educational level adjust 
  db$SDMT_education_years_adj <- with(db, ifelse(
    is.na(db$SDMT_scale_score), NA, ifelse (
    db$education_years >= 0  & db$education_years <= 2, db$SDMT_scale_score + 3, ifelse(
      db$education_years >= 3  & db$education_years <= 5, db$SDMT_scale_score + 2, ifelse(
    db$education_years >= 6  & db$education_years <= 8, db$SDMT_scale_score + 1, ifelse(
      db$education_years >= 9  & db$education_years <= 12, db$SDMT_scale_score, ifelse(
        db$education_years >= 13  & db$education_years <= 15, db$SDMT_scale_score - 1, ifelse(
          db$education_years >= 16  & db$education_years <= 18, db$SDMT_scale_score - 2, ifelse(
            db$education_years >= 19  & db$education_years <= 20, db$SDMT_scale_score - 3, ifelse(
            ))))))))) )
  
  
  # NSSae
  db$SDMT_NSSae <- with(db, ifelse(
    is.na(db$SDMT_education_years_adj), NA, ifelse (
      !is.na(db$SDMT_education_years_adj), db$SDMT_scale_score - (0.32136*(db$SDMT_education_years_adj-12)) )))
  
  
  return(db)
}
