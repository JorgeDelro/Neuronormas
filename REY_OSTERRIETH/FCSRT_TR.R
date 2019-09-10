# Free and Cued Selective Reminding Test (FCSRT) Total Recall

# Reference:
# Peña-Casanova, J., Gramunt-Fombuena, N., Quiñones-Ubeda, M., et al., 2009. Neuronorma
# study team. Spanish Multicenter Normative Studies (NEURONORMA Project): Norms for the Rey–Osterrieth Complex 
# Figure (Copy and Memory), and Free and Cued Selective Reminding Test
# Arch. Clin. Neuropsychol. 24 (4), 371–393.
# 
# 

# 
FCSRT_TR <- function(score, age, education_years){
  
  FCSRT_TR_db <- data.frame(score = score, age = age, education_years = education_years)
  FCSRT_TR_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(FCSRT_TR_db)) {
    res <- FCSRT_TR_scale_score(score = FCSRT_TR_db[i, "score"], 
                              age = FCSRT_TR_db[i, "age"],
                              education_years = FCSRT_TR_db[i, "education_years"])
    FCSRT_TR_new <- rbind(FCSRT_TR_new, res)
  }
  
  return(FCSRT_TR_new[,c("FCSRT_TR_scale_score", "FCSRT_TR_percentil_range", "FCSRT_TR_NSSae")])
}

FCSRT_TR_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    db$FCSRT_TR_scale_score <- with ( db, ifelse (
      db$score >= 48, 18, ifelse (
        
        
        
        db$score >= 47, 14, ifelse (
          
          db$score >= 46, 12, ifelse (
            db$score >= 45, 11, ifelse (
              db$score >= 43 , 10, ifelse (
                db$score >= 41 , 9, ifelse (
                  db$score >= 39 , 8, ifelse (
                    db$score >= 36 , 7, ifelse (
                      db$score >= 35, 6, ifelse (
                        db$score >= 33 , 5, ifelse (
                          db$score >= 31 , 4, ifelse (
                            db$score >= 29 , 3, ifelse (
                              db$score <=  28, 2, NA ))))))))))))))

# percentile score

db$FCSRT_TR_percentil_range <- with (db, ifelse (
  db$score >= 48, "> 99" , ifelse (
    
    
    
    db$score >= 47, "90-94" , ifelse (
      
      db$score >= 46, "72-81" , ifelse (
        db$score >= 45, "60-71" , ifelse (
          db$score >= 43 , "41-59" , ifelse (
            db$score >= 41 , "29-40" , ifelse (
              db$score >= 39 , "19-28" , ifelse (
                db$score >= 36 , "11-18" , ifelse (
                  db$score >= 35, "6-10" , ifelse (
                    db$score >= 33 , "3-5" , ifelse (
                      db$score >= 31 , "2" , ifelse (
                        db$score >= 29 , "1" , ifelse (
                          db$score <=  28, "<1" , NA ))))))))))))))
    
    
    
    
    
  }
  
  
  ##################################TABLE 4##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    db$FCSRT_TR_scale_score <- with ( db, ifelse (
      db$score >= 48, 18, ifelse (
        
        
        
        db$score >= 47, 14, ifelse (
          db$score >= 46, 13, ifelse (
            db$score >= 45, 12, ifelse (
              db$score >= 44, 11, ifelse (
                db$score >= 42 , 10, ifelse (
                  db$score >= 40 , 9, ifelse (
                    db$score >= 38 , 8, ifelse (
                      db$score >= 36 , 7, ifelse (
                        db$score >= 33 , 6, ifelse (
                          db$score >= 31 , 5, ifelse (
                            db$score >= 29 , 4, ifelse (
                              
                              db$score <=  28, 2, NA ))))))))))))))

# percentile score

db$FCSRT_TR_percentil_range <- with (db, ifelse (
  db$score >= 48, "> 99" , ifelse (
    
    
    
    db$score >= 47, "90-94" , ifelse (
      db$score >= 46, "82-89" , ifelse (
        db$score >= 45, "72-81" , ifelse (
          db$score >= 44, "60-71" , ifelse (
            db$score >= 42 , "41-59" , ifelse (
              db$score >= 40 , "29-40" , ifelse (
                db$score >= 38 , "19-28" , ifelse (
                  db$score >= 36 , "11-18" , ifelse (
                    db$score >= 33 , "6-10" , ifelse (
                      db$score >= 31 , "3-5" , ifelse (
                        db$score >= 29 , "2" , ifelse (
                          
                          db$score <=  28, "<1" , NA ))))))))))))))
    
    
    
    
    
  }
  
  
  #############################TABLE 5###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    db$FCSRT_TR_scale_score <- with ( db, ifelse (
      db$score >= 48, 18, ifelse (
        
        
        
        db$score >= 47, 14, ifelse (
          db$score >= 46, 13, ifelse (
            db$score >= 45, 12, ifelse (
              db$score >= 44, 11, ifelse (
                db$score >= 41 , 10, ifelse (
                  db$score >= 40, 9, ifelse (
                    db$score >= 38 , 8, ifelse (
                      db$score >= 36 , 7, ifelse (
                        db$score >= 33 , 6, ifelse (
                          db$score >= 32, 5, ifelse (
                            db$score >= 31, 4, ifelse (
                              db$score >= 27 , 3, ifelse (
                                db$score <=  26, 2, NA )))))))))))))))

# percentile score

db$FCSRT_TR_percentil_range <- with (db, ifelse (
  db$score >= 48, "> 99" , ifelse (
    
    
    
    db$score >= 47, "90-94" , ifelse (
      db$score >= 46, "82-89" , ifelse (
        db$score >= 45, "72-81" , ifelse (
          db$score >= 44, "60-71" , ifelse (
            db$score >= 41 , "41-59" , ifelse (
              db$score >= 40, "29-40" , ifelse (
                db$score >= 38 , "19-28" , ifelse (
                  db$score >= 36 , "11-18" , ifelse (
                    db$score >= 33 , "6-10" , ifelse (
                      db$score >= 32, "3-5" , ifelse (
                        db$score >= 31, "2" , ifelse (
                          db$score >= 27 , "1" , ifelse (
                            db$score <=  26, "<1" , NA )))))))))))))))
    
    
    
  }
  
  
  ###########################TABLE 6#####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    db$FCSRT_TR_scale_score <- with ( db, ifelse (
      db$score >= 48, 18, ifelse (
        
        
        
        db$score >= 46 , 14, ifelse (
          db$score >= 45, 13, ifelse (
            db$score >= 44, 12, ifelse (
              db$score >= 43, 11, ifelse (
                db$score >= 41 , 10, ifelse (
                  db$score >= 39 , 9, ifelse (
                    db$score >= 36 , 8, ifelse (
                      db$score >= 34 , 7, ifelse (
                        db$score >= 32 , 6, ifelse (
                          db$score >= 27 , 5, ifelse (
                            
                            db$score >= 26, 3, ifelse (
                              db$score <=  25, 2, NA ))))))))))))))

# percentile score

db$FCSRT_TR_percentil_range <- with (db, ifelse (
  db$score >= 48, "> 99" , ifelse (
    
    
    
    db$score >= 46 , "90-94" , ifelse (
      db$score >= 45, "82-89" , ifelse (
        db$score >= 44, "72-81" , ifelse (
          db$score >= 43, "60-71" , ifelse (
            db$score >= 41 , "41-59" , ifelse (
              db$score >= 39 , "29-40" , ifelse (
                db$score >= 36 , "19-28" , ifelse (
                  db$score >= 34 , "11-18" , ifelse (
                    db$score >= 32 , "6-10" , ifelse (
                      db$score >= 27 , "3-5" , ifelse (
                        
                        db$score >= 26, "1" , ifelse (
                          db$score <=  25, "<1" , NA ))))))))))))))
    
    
  }
  
  
  ############################TABLE 7####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$FCSRT_TR_scale_score <- with ( db, ifelse (
      db$score >= 48, 18, ifelse (
        
        
        db$score >= 47, 15, ifelse (
          db$score >= 46, 14, ifelse (
            db$score >= 45, 13, ifelse (
              db$score >= 43 , 12, ifelse (
                db$score >= 42, 11, ifelse (
                  db$score >= 39 , 10, ifelse (
                    db$score >= 36 , 9, ifelse (
                      db$score >= 34 , 8, ifelse (
                        db$score >= 30 , 7, ifelse (
                          db$score >= 28 , 6, ifelse (
                            db$score >= 26 , 5, ifelse (
                              db$score >= 20 , 4, ifelse (
                                db$score >= 14 , 3, ifelse (
                                  db$score <=  13, 2, NA ))))))))))))))))

# percentile score

db$FCSRT_TR_percentil_range <- with (db, ifelse (
  db$score >= 48, "> 99" , ifelse (
    
    
    db$score >= 47, "95-97" , ifelse (
      db$score >= 46, "90-94" , ifelse (
        db$score >= 45, "82-89" , ifelse (
          db$score >= 43 , "72-81" , ifelse (
            db$score >= 42, "60-71" , ifelse (
              db$score >= 39 , "41-59" , ifelse (
                db$score >= 36 , "29-40" , ifelse (
                  db$score >= 34 , "19-28" , ifelse (
                    db$score >= 30 , "11-18" , ifelse (
                      db$score >= 28 , "6-10" , ifelse (
                        db$score >= 26 , "3-5" , ifelse (
                          db$score >= 20 , "2" , ifelse (
                            db$score >= 14 , "1" , ifelse (
                              db$score <=  13, "<1" , NA ))))))))))))))))
    
  }
  
  
  ##############################TABLE 8##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    db$FCSRT_TR_scale_score <- with ( db, ifelse (
      db$score >= 48, 18, ifelse (
        
        
        db$score >= 47, 15, ifelse (
          db$score >= 46, 14, ifelse (
            db$score >= 45, 13, ifelse (
              db$score >= 43 , 12, ifelse (
                db$score >= 41 , 11, ifelse (
                  db$score >= 37 , 10, ifelse (
                    db$score >= 35 , 9, ifelse (
                      db$score >= 33 , 8, ifelse (
                        db$score >= 29 , 7, ifelse (
                          db$score >= 27 , 6, ifelse (
                            db$score >= 23 , 5, ifelse (
                              db$score >= 20 , 4, ifelse (
                                db$score >= 14 , 3, ifelse (
                                  db$score <=  13, 2, NA ))))))))))))))))

# percentile score

db$FCSRT_TR_percentil_range <- with (db, ifelse (
  db$score >= 48, "> 99" , ifelse (
    
    
    db$score >= 47, "95-97" , ifelse (
      db$score >= 46, "90-94" , ifelse (
        db$score >= 45, "82-89" , ifelse (
          db$score >= 43 , "72-81" , ifelse (
            db$score >= 41 , "60-71" , ifelse (
              db$score >= 37 , "41-59" , ifelse (
                db$score >= 35 , "29-40" , ifelse (
                  db$score >= 33 , "19-28" , ifelse (
                    db$score >= 29 , "11-18" , ifelse (
                      db$score >= 27 , "6-10" , ifelse (
                        db$score >= 23 , "3-5" , ifelse (
                          db$score >= 20 , "2" , ifelse (
                            db$score >= 14 , "1" , ifelse (
                              db$score <=  13, "<1" , NA )))))))))))))))) 
    
    
  }
  
  
  ##############################TABLE 9##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    db$FCSRT_TR_scale_score <- with ( db, ifelse (
      db$score >= 48, 18, ifelse (
        
        
        db$score >= 46 , 15, ifelse (
          db$score >= 45, 14, ifelse (
            db$score >= 44, 13, ifelse (
              db$score >= 43, 12, ifelse (
                db$score >= 40 , 11, ifelse (
                  db$score >= 36 , 10, ifelse (
                    db$score >= 34 , 9, ifelse (
                      db$score >= 32 , 8, ifelse (
                        db$score >= 29 , 7, ifelse (
                          db$score >= 27 , 6, ifelse (
                            db$score >= 23 , 5, ifelse (
                              db$score >= 20 , 4, ifelse (
                                db$score >= 14 , 3, ifelse (
                                  db$score <=  13, 2, NA ))))))))))))))))

# percentile score

db$FCSRT_TR_percentil_range <- with (db, ifelse (
  db$score >= 48, "> 99" , ifelse (
    
    
    db$score >= 46 , "95-97" , ifelse (
      db$score >= 45, "90-94" , ifelse (
        db$score >= 44, "82-89" , ifelse (
          db$score >= 43, "72-81" , ifelse (
            db$score >= 40 , "60-71" , ifelse (
              db$score >= 36 , "41-59" , ifelse (
                db$score >= 34 , "29-40" , ifelse (
                  db$score >= 32 , "19-28" , ifelse (
                    db$score >= 29 , "11-18" , ifelse (
                      db$score >= 27 , "6-10" , ifelse (
                        db$score >= 23 , "3-5" , ifelse (
                          db$score >= 20 , "2" , ifelse (
                            db$score >= 14 , "1" , ifelse (
                              db$score <=  13, "<1" , NA ))))))))))))))))
    
    
  }
  
  
  
  ##############################TABLE 10##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    db$FCSRT_TR_scale_score <- with ( db, ifelse (
      db$score >= 48, 18, ifelse (
        
        
        db$score >= 47, 15, ifelse (
          db$score >= 45 , 14, ifelse (
            db$score >= 43 , 13, ifelse (
              db$score >= 42, 12, ifelse (
                db$score >= 39 , 11, ifelse (
                  db$score >= 35 , 10, ifelse (
                    db$score >= 32 , 9, ifelse (
                      db$score >= 29 , 8, ifelse (
                        db$score >= 26 , 7, ifelse (
                          db$score >= 23 , 6, ifelse (
                            db$score >= 20 , 5, ifelse (
                              db$score >= 14 , 4, ifelse (
                                db$score >= 13, 3, ifelse (
                                  db$score <=  12, 2, NA ))))))))))))))))

# percentile score

db$FCSRT_TR_percentil_range <- with (db, ifelse (
  db$score >= 48, "> 99" , ifelse (
    
    
    db$score >= 47, "95-97" , ifelse (
      db$score >= 45 , "90-94" , ifelse (
        db$score >= 43 , "82-89" , ifelse (
          db$score >= 42, "72-81" , ifelse (
            db$score >= 39 , "60-71" , ifelse (
              db$score >= 35 , "41-59" , ifelse (
                db$score >= 32 , "29-40" , ifelse (
                  db$score >= 29 , "19-28" , ifelse (
                    db$score >= 26 , "11-18" , ifelse (
                      db$score >= 23 , "6-10" , ifelse (
                        db$score >= 20 , "3-5" , ifelse (
                          db$score >= 14 , "2" , ifelse (
                            db$score >= 13, "1" , ifelse (
                              db$score <=  12, "<1" , NA ))))))))))))))))
    
    
    
  }
  
  
  ##############################TABLE 11##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    db$FCSRT_TR_scale_score <- with ( db, ifelse (
      db$score >= 47 , 18, ifelse (
        
        db$score >= 46, 16, ifelse (
          db$score >= 44 , 15, ifelse (
            db$score >= 43, 14, ifelse (
              db$score >= 42, 13, ifelse (
                db$score >= 39 , 12, ifelse (
                  db$score >= 37 , 11, ifelse (
                    db$score >= 34 , 10, ifelse (
                      db$score >= 32 , 9, ifelse (
                        db$score >= 29 , 8, ifelse (
                          db$score >= 26 , 7, ifelse (
                            db$score >= 23 , 6, ifelse (
                              db$score >= 22, 5, ifelse (
                                
                                db$score >= 21, 3, ifelse (
                                  db$score <=  20, 2, NA ))))))))))))))))

# percentile score

db$FCSRT_TR_percentil_range <- with (db, ifelse (
  db$score >= 47 , "> 99" , ifelse (
    
    db$score >= 46, "98" , ifelse (
      db$score >= 44 , "95-97" , ifelse (
        db$score >= 43, "90-94" , ifelse (
          db$score >= 42, "82-89" , ifelse (
            db$score >= 39 , "72-81" , ifelse (
              db$score >= 37 , "60-71" , ifelse (
                db$score >= 34 , "41-59" , ifelse (
                  db$score >= 32 , "29-40" , ifelse (
                    db$score >= 29 , "19-28" , ifelse (
                      db$score >= 26 , "11-18" , ifelse (
                        db$score >= 23 , "6-10" , ifelse (
                          db$score >= 22, "3-5" , ifelse (
                            
                            db$score >= 21, "1" , ifelse (
                              db$score <=  20, "<1" , NA )))))))))))))))) 
    
    
  }
  
  
  #############################TABLE 12###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    db$FCSRT_TR_scale_score <- with ( db, ifelse (
      db$score >= 43, 18, ifelse (
        
        
        db$score >= 41, 15, ifelse (
          
          db$score >= 40, 13, ifelse (
            db$score >= 39, 12, ifelse (
              db$score >= 36, 11, ifelse (
                db$score >= 35, 10, ifelse (
                  db$score >= 29, 9, ifelse (
                    db$score >= 27, 8, ifelse (
                      db$score >= 26, 7, ifelse (
                        db$score >= 23, 6, ifelse (
                          db$score >= 22, 5, ifelse (
                            db$score >= 21, 4, ifelse (
                              
                              db$score <= 20, 2, NA ))))))))))))))

# percentile score

db$FCSRT_TR_percentil_range <- with (db, ifelse (
  db$score >= 43, "> 99" , ifelse (
    
    
    db$score >= 41, "95-97" , ifelse (
      
      db$score >= 40, "82-89" , ifelse (
        db$score >= 39, "72-81" , ifelse (
          db$score >= 36, "60-71" , ifelse (
            db$score >= 35, "41-59" , ifelse (
              db$score >= 29, "29-40" , ifelse (
                db$score >= 27, "19-28" , ifelse (
                  db$score >= 26, "11-18" , ifelse (
                    db$score >= 23, "6-10" , ifelse (
                      db$score >= 22, "3-5" , ifelse (
                        db$score >= 21, "2" , ifelse (
                          
                          db$score <= 20, "<1" , NA ))))))))))))))
    
    
  }
  
  
  # Educational level adjust 
  db$FCSRT_TR_education_years_adj <- with(db, ifelse(
    db$education_years >= 0  & db$education_years <= 1, db$FCSRT_TR_scale_score + 2, ifelse( 
    db$education_years >= 2  & db$education_years <= 6, db$FCSRT_TR_scale_score + 1, ifelse(
      db$education_years >= 7  & db$education_years <= 12, db$FCSRT_TR_scale_score, ifelse(
        db$education_years >= 13  & db$education_years <= 17, db$FCSRT_TR_scale_score - 1, ifelse(
          db$education_years >= 18  & db$education_years <= 20, db$FCSRT_TR_scale_score - 2, ifelse(
            
            )))))))
  
  
  # NSSae
  db$FCSRT_TR_NSSae <- db$FCSRT_TR_scale_score - (0.19386*(db$FCSRT_TR_education_years_adj-12)) 
  
  return(db)
}
