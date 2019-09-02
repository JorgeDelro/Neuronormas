# Rey-Osterrieth complex figure (ROCF) Memory Delayed Recall
#
# Reference:
# Peña-Casanova, J., Gramunt-Fombuena, N., Quiñones-Ubeda, M., et al., 2009. Neuronorma
# study team. Spanish Multicenter Normative Studies (NEURONORMA Project): Norms for the Rey–Osterrieth Complex 
# Figure (Copy and Memory), and Free and Cued Selective Reminding Test
# Arch. Clin. Neuropsychol. 24 (4), 371–393.
# 
# 

# 
ROCF_M_DR <- function(score, age, education_years){
  
  ROCF_M_DR_db <- data.frame(score = score, age = age, education_years = education_years)
  ROCF_M_DR_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(ROCF_M_DR_db)) {
    res <- ROCF_M_DR_scale_score(score = ROCF_M_DR_db[i, "score"], 
                              age = ROCF_M_DR_db[i, "age"],
                              education_years = ROCF_M_DR_db[i, "education_years"])
    ROCF_M_DR_new <- rbind(ROCF_M_DR_new, res)
  }
  
  return(ROCF_M_DR_new)
}

ROCF_M_DR_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    db$ROCF_M_DR_scale_score <- with ( db, ifelse (
      db$score >= 34.5, 18, ifelse (
        db$score >= 33.5, 17, ifelse (
          db$score >= 31.5, 16, ifelse (
            db$score >= 29.5, 15, ifelse (
              db$score >= 27.5, 14, ifelse (
                db$score >= 24, 13, ifelse (
                  db$score >= 21.5, 12, ifelse (
                    db$score >= 19, 11, ifelse (
                      db$score >= 16, 10, ifelse (
                        db$score >= 13, 9, ifelse (
                          db$score >= 10.5, 8, ifelse (
                            db$score >= 8, 7, ifelse (
                              db$score >= 6.5 , 6, ifelse (
                                db$score >= 4, 5, ifelse (
                                  
                                  db$score >= 3,5, 3, ifelse (
                                    db$score <=  3, 2, NA )))))))))))))))))


# percentile score

db$ROCF_M_DR_percentil_range <- with (db, ifelse (
  db$score >= 34.5, "> 99" , ifelse (
    db$score >= 33.5, "99" , ifelse (
      db$score >= 31.5, "98" , ifelse (
        db$score >= 29.5, "95-97" , ifelse (
          db$score >= 27.5, "90-94" , ifelse (
            db$score >= 24, "82-89" , ifelse (
              db$score >= 21.5, "72-81" , ifelse (
                db$score >= 19, "60-71" , ifelse (
                  db$score >= 16, "41-59" , ifelse (
                    db$score >= 13, "29-40" , ifelse (
                      db$score >= 10.5, "19-28" , ifelse (
                        db$score >= 8, "11-18" , ifelse (
                          db$score >= 6.5 , "6-10" , ifelse (
                            db$score >= 4, "3-5" , ifelse (
                              
                              db$score >= 3,5, "1" , ifelse (
                                db$score <=  3, "<1" , NA )))))))))))))))))
    
    
    
    
    
  }
  
  
  ##################################TABLE 4##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    db$ROCF_M_DR_scale_score <- with ( db, ifelse (
      db$score >= 34.5, 18, ifelse (
        db$score >= 33.5, 17, ifelse (
          db$score >= 31.5, 16, ifelse (
            db$score >= 28.5, 15, ifelse (
              db$score >= 24.5, 14, ifelse (
                db$score >= 22, 13, ifelse (
                  db$score >= 20, 12, ifelse (
                    db$score >= 18, 11, ifelse (
                      db$score >= 14.5, 10, ifelse (
                        db$score >= 11.5, 9, ifelse (
                          db$score >= 9.5 , 8, ifelse (
                            db$score >= 7, 7, ifelse (
                              db$score >= 6, 6, ifelse (
                                db$score >= 4, 5, ifelse (
                                  
                                  db$score >= 3.5, 3, ifelse (
                                    db$score <=  3, 2, NA )))))))))))))))))

# percentile score

db$ROCF_M_DR_percentil_range <- with (db, ifelse (
  db$score >= 34.5, "> 99" , ifelse (
    db$score >= 33.5, "99" , ifelse (
      db$score >= 31.5, "98" , ifelse (
        db$score >= 28.5, "95-97" , ifelse (
          db$score >= 24.5, "90-94" , ifelse (
            db$score >= 22, "82-89" , ifelse (
              db$score >= 20, "72-81" , ifelse (
                db$score >= 18, "60-71" , ifelse (
                  db$score >= 14.5, "41-59" , ifelse (
                    db$score >= 11.5, "29-40" , ifelse (
                      db$score >= 9.5 , "19-28" , ifelse (
                        db$score >= 7, "11-18" , ifelse (
                          db$score >= 6, "6-10" , ifelse (
                            db$score >= 4, "3-5" , ifelse (
                              
                              db$score >= 3.5, "1" , ifelse (
                                db$score <=  3, "<1" , NA )))))))))))))))))
    
    
    
    
    
  }
  
  
  #############################TABLE 5###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    db$ROCF_M_DR_scale_score <- with ( db, ifelse (
      db$score >= 34.5, 18, ifelse (
        db$score >= 29.5, 17, ifelse (
          db$score >= 29, 16, ifelse (
            db$score >= 26.5, 15, ifelse (
              db$score >= 23.5, 14, ifelse (
                db$score >= 21, 13, ifelse (
                  db$score >= 19, 12, ifelse (
                    db$score >= 18, 11, ifelse (
                      db$score >= 14, 10, ifelse (
                        db$score >= 12, 9, ifelse (
                          db$score >= 8.5 , 8, ifelse (
                            db$score >= 7, 7, ifelse (
                              db$score >= 5.5 , 6, ifelse (
                                db$score >= 4, 5, ifelse (
                                  
                                  db$score >= 3.5, 3, ifelse (
                                    db$score <=  3, 2, NA )))))))))))))))))

# percentile score

db$ROCF_M_DR_percentil_range <- with (db, ifelse (
  db$score >= 34.5, "> 99" , ifelse (
    db$score >= 29.5, "99" , ifelse (
      db$score >= 29, "98" , ifelse (
        db$score >= 26.5, "95-97" , ifelse (
          db$score >= 23.5, "90-94" , ifelse (
            db$score >= 21, "82-89" , ifelse (
              db$score >= 19, "72-81" , ifelse (
                db$score >= 18, "60-71" , ifelse (
                  db$score >= 14, "41-59" , ifelse (
                    db$score >= 12, "29-40" , ifelse (
                      db$score >= 8.5 , "19-28" , ifelse (
                        db$score >= 7, "11-18" , ifelse (
                          db$score >= 5.5 , "6-10" , ifelse (
                            db$score >= 4, "3-5" , ifelse (
                              
                              db$score >= 3.5, "1" , ifelse (
                                db$score <=  3, "<1" , NA )))))))))))))))))
    
    
    
  }
  
  
  ###########################TABLE 6#####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    db$ROCF_M_DR_scale_score <- with ( db, ifelse (
      db$score >= 29, 18, ifelse (
        
        db$score >= 28.5, 16, ifelse (
          db$score >= 24.5, 15, ifelse (
            db$score >= 22.5, 14, ifelse (
              db$score >= 22, 13, ifelse (
                db$score >= 19.5, 12, ifelse (
                  db$score >= 18, 11, ifelse (
                    db$score >= 15, 10, ifelse (
                      db$score >= 11.5, 9, ifelse (
                        db$score >= 9.5 , 8, ifelse (
                          db$score >= 7, 7, ifelse (
                            db$score >= 5.5 , 6, ifelse (
                              db$score >= 3.5 , 5, ifelse (
                                
                                db$score >= 3, 3, ifelse (
                                  db$score <=  2.5, 2, NA ))))))))))))))))

# percentile score

db$ROCF_M_DR_percentil_range <- with (db, ifelse (
  db$score >= 29, "> 99" , ifelse (
    
    db$score >= 28.5, "98" , ifelse (
      db$score >= 24.5, "95-97" , ifelse (
        db$score >= 22.5, "90-94" , ifelse (
          db$score >= 22, "82-89" , ifelse (
            db$score >= 19.5, "72-81" , ifelse (
              db$score >= 18, "60-71" , ifelse (
                db$score >= 15, "41-59" , ifelse (
                  db$score >= 11.5, "29-40" , ifelse (
                    db$score >= 9.5 , "19-28" , ifelse (
                      db$score >= 7, "11-18" , ifelse (
                        db$score >= 5.5 , "6-10" , ifelse (
                          db$score >= 3.5 , "3-5" , ifelse (
                            
                            db$score >= 3, "1" , ifelse (
                              db$score <=  2.5, "<1" , NA ))))))))))))))))
    
    
  }
  
  
  ############################TABLE 7####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$ROCF_M_DR_scale_score <- with ( db, ifelse (
      db$score >= 31.5, 18, ifelse (
        db$score >= 29.5, 17, ifelse (
          db$score >= 29, 16, ifelse (
            db$score >= 28, 15, ifelse (
              db$score >= 23.5, 14, ifelse (
                db$score >= 21.5, 13, ifelse (
                  db$score >= 19, 12, ifelse (
                    db$score >= 17, 11, ifelse (
                      db$score >= 14, 10, ifelse (
                        db$score >= 10.5, 9, ifelse (
                          db$score >= 8.5 , 8, ifelse (
                            db$score >= 6.5 , 7, ifelse (
                              db$score >= 5.5 , 6, ifelse (
                                db$score >= 3.5 , 5, ifelse (
                                  
                                  db$score >= 2.5 , 3, ifelse (
                                    db$score <=  2, 2, NA )))))))))))))))))

# percentile score

db$ROCF_M_DR_percentil_range <- with (db, ifelse (
  db$score >= 31.5, "> 99" , ifelse (
    db$score >= 29.5, "99" , ifelse (
      db$score >= 29, "98" , ifelse (
        db$score >= 28, "95-97" , ifelse (
          db$score >= 23.5, "90-94" , ifelse (
            db$score >= 21.5, "82-89" , ifelse (
              db$score >= 19, "72-81" , ifelse (
                db$score >= 17, "60-71" , ifelse (
                  db$score >= 14, "41-59" , ifelse (
                    db$score >= 10.5, "29-40" , ifelse (
                      db$score >= 8.5 , "19-28" , ifelse (
                        db$score >= 6.5 , "11-18" , ifelse (
                          db$score >= 5.5 , "6-10" , ifelse (
                            db$score >= 3.5 , "3-5" , ifelse (
                              
                              db$score >= 2.5 , "1" , ifelse (
                                db$score <=  2, "<1" , NA )))))))))))))))))
    
  }
  
  
  ##############################TABLE 8##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    db$ROCF_M_DR_scale_score <- with ( db, ifelse (
      db$score >= 31.5, 18, ifelse (
        db$score >= 30.5, 17, ifelse (
          db$score >= 29.5, 16, ifelse (
            db$score >= 28, 15, ifelse (
              db$score >= 24, 14, ifelse (
                db$score >= 22.5, 13, ifelse (
                  db$score >= 19.5, 12, ifelse (
                    db$score >= 17, 11, ifelse (
                      db$score >= 12.5, 10, ifelse (
                        db$score >= 10, 9, ifelse (
                          db$score >= 8, 8, ifelse (
                            db$score >= 6.5 , 7, ifelse (
                              db$score >= 4.5 , 6, ifelse (
                                db$score >= 3.5 , 5, ifelse (
                                  db$score >= 2.5 , 4, ifelse (
                                    db$score >= 2, 3, ifelse (
                                      db$score <=  1.5, 2, NA ))))))))))))))))))
    
    # percentile score
    
    db$ROCF_M_DR_percentil_range <- with (db, ifelse (
      db$score >= 31.5, "> 99" , ifelse (
        db$score >= 30.5, "99" , ifelse (
          db$score >= 29.5, "98" , ifelse (
            db$score >= 28, "95-97" , ifelse (
              db$score >= 24, "90-94" , ifelse (
                db$score >= 22.5, "82-89" , ifelse (
                  db$score >= 19.5, "72-81" , ifelse (
                    db$score >= 17, "60-71" , ifelse (
                      db$score >= 12.5, "41-59" , ifelse (
                        db$score >= 10, "29-40" , ifelse (
                          db$score >= 8, "19-28" , ifelse (
                            db$score >= 6.5 , "11-18" , ifelse (
                              db$score >= 4.5 , "6-10" , ifelse (
                                db$score >= 3.5 , "3-5" , ifelse (
                                  db$score >= 2.5 , "2" , ifelse (
                                    db$score >= 2, "1" , ifelse (
                                      db$score <=  1.5, "<1" , NA )))))))))))))))))) 
    
    
  }
  
  
  ##############################TABLE 9##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    db$ROCF_M_DR_scale_score <- with ( db, ifelse (
      db$score >= 31.5, 18, ifelse (
        db$score >= 30.5, 17, ifelse (
          db$score >= 29.5, 16, ifelse (
            db$score >= 28, 15, ifelse (
              db$score >= 23.5, 14, ifelse (
                db$score >= 21.5, 13, ifelse (
                  db$score >= 18.5, 12, ifelse (
                    db$score >= 15, 11, ifelse (
                      db$score >= 12, 10, ifelse (
                        db$score >= 9.5 , 9, ifelse (
                          db$score >= 8, 8, ifelse (
                            db$score >= 6.5 , 7, ifelse (
                              db$score >= 4.5 , 6, ifelse (
                                db$score >= 3.5 , 5, ifelse (
                                  db$score >= 2.5 , 4, ifelse (
                                    db$score >= 2, 3, ifelse (
                                      db$score <=  1.5, 2, NA ))))))))))))))))))
    
    # percentile score
    
    db$ROCF_M_DR_percentil_range <- with (db, ifelse (
      db$score >= 31.5, "> 99" , ifelse (
        db$score >= 30.5, "99" , ifelse (
          db$score >= 29.5, "98" , ifelse (
            db$score >= 28, "95-97" , ifelse (
              db$score >= 23.5, "90-94" , ifelse (
                db$score >= 21.5, "82-89" , ifelse (
                  db$score >= 18.5, "72-81" , ifelse (
                    db$score >= 15, "60-71" , ifelse (
                      db$score >= 12, "41-59" , ifelse (
                        db$score >= 9.5 , "29-40" , ifelse (
                          db$score >= 8, "19-28" , ifelse (
                            db$score >= 6.5 , "11-18" , ifelse (
                              db$score >= 4.5 , "6-10" , ifelse (
                                db$score >= 3.5 , "3-5" , ifelse (
                                  db$score >= 2.5 , "2" , ifelse (
                                    db$score >= 2, "1" , ifelse (
                                      db$score <=  1.5, "<1" , NA ))))))))))))))))))
    
    
  }
  
  
  
  ##############################TABLE 10##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    db$ROCF_M_DR_scale_score <- with ( db, ifelse (
      db$score >= 32.5, 18, ifelse (
        db$score >= 30.5, 17, ifelse (
          db$score >= 28.5, 16, ifelse (
            db$score >= 25.5, 15, ifelse (
              db$score >= 22.5, 14, ifelse (
                db$score >= 19, 13, ifelse (
                  db$score >= 17, 12, ifelse (
                    db$score >= 14, 11, ifelse (
                      db$score >= 12, 10, ifelse (
                        db$score >= 9, 9, ifelse (
                          db$score >= 7.5 , 8, ifelse (
                            db$score >= 5.5 , 7, ifelse (
                              db$score >= 4, 6, ifelse (
                                db$score >= 2.5 , 5, ifelse (
                                  db$score >= 2, 4, ifelse (
                                    
                                    db$score <=  1.5, 2, NA )))))))))))))))))

# percentile score

db$ROCF_M_DR_percentil_range <- with (db, ifelse (
  db$score >= 32.5, "> 99" , ifelse (
    db$score >= 30.5, "99" , ifelse (
      db$score >= 28.5, "98" , ifelse (
        db$score >= 25.5, "95-97" , ifelse (
          db$score >= 22.5, "90-94" , ifelse (
            db$score >= 19, "82-89" , ifelse (
              db$score >= 17, "72-81" , ifelse (
                db$score >= 14, "60-71" , ifelse (
                  db$score >= 12, "41-59" , ifelse (
                    db$score >= 9, "29-40" , ifelse (
                      db$score >= 7.5 , "19-28" , ifelse (
                        db$score >= 5.5 , "11-18" , ifelse (
                          db$score >= 4, "6-10" , ifelse (
                            db$score >= 2.5 , "3-5" , ifelse (
                              db$score >= 2, "2" , ifelse (
                                
                                db$score <=  1.5, "<1" , NA )))))))))))))))))
    
    
    
  }
  
  
  ##############################TABLE 11##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    db$ROCF_M_DR_scale_score <- with ( db, ifelse (
      db$score >= 25.5, 18, ifelse (
        
        db$score >= 23.5, 16, ifelse (
          db$score >= 20.5, 15, ifelse (
            db$score >= 19, 14, ifelse (
              db$score >= 17, 13, ifelse (
                db$score >= 14.5, 12, ifelse (
                  db$score >= 13, 11, ifelse (
                    db$score >= 11.5, 10, ifelse (
                      db$score >= 9, 9, ifelse (
                        db$score >= 7.5 , 8, ifelse (
                          db$score >= 4.5 , 7, ifelse (
                            db$score >= 3, 6, ifelse (
                              db$score >= 2, 5, ifelse (
                                
                                
                                db$score <=  1.5, 2, NA )))))))))))))))

# percentile score

db$ROCF_M_DR_percentil_range <- with (db, ifelse (
  db$score >= 25.5, "> 99" , ifelse (
    
    db$score >= 23.5, "98" , ifelse (
      db$score >= 20.5, "95-97" , ifelse (
        db$score >= 19, "90-94" , ifelse (
          db$score >= 17, "82-89" , ifelse (
            db$score >= 14.5, "72-81" , ifelse (
              db$score >= 13, "60-71" , ifelse (
                db$score >= 11.5, "41-59" , ifelse (
                  db$score >= 9, "29-40" , ifelse (
                    db$score >= 7.5 , "19-28" , ifelse (
                      db$score >= 4.5 , "11-18" , ifelse (
                        db$score >= 3, "6-10" , ifelse (
                          db$score >= 2, "3-5" , ifelse (
                            
                            
                            db$score <=  1.5, "<1" , NA )))))))))))))))
    
    
  }
  
  
  #############################TABLE 12###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    db$ROCF_M_DR_scale_score <- with ( db, ifelse (
      db$score >= 19, 18, ifelse (
        
        
        
        db$score >= 18.5, 14, ifelse (
          db$score >= 16, 13, ifelse (
            db$score >= 14, 12, ifelse (
              db$score >= 12.5, 11, ifelse (
                db$score >= 10.5, 10, ifelse (
                  db$score >= 9, 9, ifelse (
                    db$score >= 7, 8, ifelse (
                      db$score >= 4, 7, ifelse (
                        db$score >= 2.5 , 6, ifelse (
                          db$score >= 1, 5, ifelse (
                            db$score >= 0.5, 4, ifelse (
                              
                              db$score <= 0, 2, NA ))))))))))))))

# percentile score

db$ROCF_M_DR_percentil_range <- with (db, ifelse (
  db$score >= 19, "> 99" , ifelse (
    
    
    
    db$score >= 18.5, "90-94" , ifelse (
      db$score >= 16, "82-89" , ifelse (
        db$score >= 14, "72-81" , ifelse (
          db$score >= 12.5, "60-71" , ifelse (
            db$score >= 10.5, "41-59" , ifelse (
              db$score >= 9, "29-40" , ifelse (
                db$score >= 7, "19-28" , ifelse (
                  db$score >= 4, "11-18" , ifelse (
                    db$score >= 2.5 , "6-10" , ifelse (
                      db$score >= 1, "3-5" , ifelse (
                        db$score >= 0.5, "2" , ifelse (
                          
                          db$score <= 0, "<1" , NA ))))))))))))))
    
    
  }
  
  
  # Educational level adjust 
  db$education_years_adj <- with(db, ifelse(
    db$education_years >= 0  & db$education_years <= 4, db$ROCF_M_DR_scale_score + 1, ifelse(
      db$education_years >= 5  & db$education_years <= 12, db$ROCF_M_DR_scale_score, ifelse(
        db$education_years >= 13  & db$education_years <= 19, db$ROCF_M_DR_scale_score - 1, ifelse(
          db$education_years > 19  & db$education_years <= 20, db$ROCF_M_DR_scale_score - 2, ifelse(
            
            ))))))
  
  
  # NSSae
  db$NSSae_ROCF_M_DR <- db$ROCF_M_DR_scale_score - (0.13346*(db$education_years_adj-12)) 
  
  return(db)
}
