# Rey-Osterrieth complex figure (ROCF) Copy RAW
#
# Reference:
# Peña-Casanova, J., Gramunt-Fombuena, N., Quiñones-Ubeda, M., et al., 2009. Neuronorma
# study team. Spanish Multicenter Normative Studies (NEURONORMA Project): Norms for the Rey–Osterrieth Complex 
# Figure (Copy and Memory), and Free and Cued Selective Reminding Test
# Arch. Clin. Neuropsychol. 24 (4), 371–393.
# 
# 

ROCF_C_RAW <- function(score, age, education_years){
  
  ROCF_C_RAW_db <- data.frame(score = score, age = age, education_years = education_years)
  ROCF_C_RAW_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(ROCF_C_RAW_db)) {
    res <- ROCF_C_RAW_scale_score(score = ROCF_C_RAW_db[i, "score"], 
                              age = ROCF_C_RAW_db[i, "age"],
                              education_years = ROCF_C_RAW_db[i, "education_years"])
    ROCF_C_RAW_new <- rbind(ROCF_C_RAW_new, res)
  }
  
  return(ROCF_C_RAW_new)
}

ROCF_C_RAW_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    db$ROCF_C_RAW_scale_score <- with ( db, ifelse (
      db$score >= 35.5, 18, ifelse (
        
        
        
        
        
        db$score >= 34.5, 12, ifelse (
          db$score >= 33.5, 11, ifelse (
            db$score >= 31.5, 10, ifelse (
              db$score >= 29.5, 9, ifelse (
                db$score >= 27.5, 8, ifelse (
                  db$score >= 23.5, 7, ifelse (
                    db$score >= 19.5, 6, ifelse (
                      db$score >= 17.5, 5, ifelse (
                        db$score >= 14.5, 4, ifelse (
                          db$score >= 13.5, 3, ifelse (
                            db$score <= 13, 2, NA )))))))))))))


# percentile score

db$ROCF_C_RAW_percentil_range <- with (db, ifelse (
  db$score >= 35.5, "> 99" , ifelse (
    
    
    
    
    
    db$score >= 34.5, "72-81" , ifelse (
      db$score >= 33.5, "60-71" , ifelse (
        db$score >= 31.5, "41-59" , ifelse (
          db$score >= 29.5, "29-40" , ifelse (
            db$score >= 27.5, "19-28" , ifelse (
              db$score >= 23.5, "11-18" , ifelse (
                db$score >= 19.5, "6-10" , ifelse (
                  db$score >= 17.5, "3-5" , ifelse (
                    db$score >= 14.5, "2" , ifelse (
                      db$score >= 13.5, "1" , ifelse (
                        db$score <= 13, "<1" , NA )))))))))))))
    
    
    
    
    
  }
  
  
  ##################################TABLE 4##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    db$ROCF_C_RAW_scale_score <- with ( db, ifelse (
      db$score >= 35.5, 18, ifelse (
        
        
        
        
        
        db$score >= 34.5, 12, ifelse (
          db$score >= 33.5, 11, ifelse (
            db$score >= 30.5, 10, ifelse (
              db$score >= 28.5, 9, ifelse (
                db$score >= 25.5, 8, ifelse (
                  db$score >= 23.5, 7, ifelse (
                    db$score >= 19.5, 6, ifelse (
                      db$score >= 17.5, 5, ifelse (
                        db$score >= 14.5, 4, ifelse (
                          db$score >= 13.5, 3, ifelse (
                            db$score <= 13, 2, NA )))))))))))))

# percentile score

db$ROCF_C_RAW_percentil_range <- with (db, ifelse (
  db$score >= 35.5, "> 99" , ifelse (
    
    
    
    
    
    db$score >= 34.5, "72-81" , ifelse (
      db$score >= 33.5, "60-71" , ifelse (
        db$score >= 30.5, "41-59" , ifelse (
          db$score >= 28.5, "29-40" , ifelse (
            db$score >= 25.5, "19-28" , ifelse (
              db$score >= 23.5, "11-18" , ifelse (
                db$score >= 19.5, "6-10" , ifelse (
                  db$score >= 17.5, "3-5" , ifelse (
                    db$score >= 14.5, "2" , ifelse (
                      db$score >= 13.5, "1" , ifelse (
                        db$score <= 13, "<1" , NA )))))))))))))
    
    
    
    
    
  }
  
  
  #############################TABLE 5###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    db$ROCF_C_RAW_scale_score <- with ( db, ifelse (
      db$score >= 35.5, 18, ifelse (
        
        
        
        
        db$score >= 34.5, 13, ifelse (
          db$score >= 33.5, 12, ifelse (
            db$score >= 32.5, 11, ifelse (
              db$score >= 29.5, 10, ifelse (
                db$score >= 27.5, 9, ifelse (
                  db$score >= 25.5, 8, ifelse (
                    db$score >= 22.5, 7, ifelse (
                      db$score >= 20, 6, ifelse (
                        db$score >= 17.5, 5, ifelse (
                          db$score >= 14.5, 4, ifelse (
                            db$score >= 13.5, 3, ifelse (
                              db$score <= 13, 2, NA ))))))))))))))

# percentile score

db$ROCF_C_RAW_percentil_range <- with (db, ifelse (
  db$score >= 35.5, "> 99" , ifelse (
    
    
    
    
    db$score >= 34.5, "82-89" , ifelse (
      db$score >= 33.5, "72-81" , ifelse (
        db$score >= 32.5, "60-71" , ifelse (
          db$score >= 29.5, "41-59" , ifelse (
            db$score >= 27.5, "29-40" , ifelse (
              db$score >= 25.5, "19-28" , ifelse (
                db$score >= 22.5, "11-18" , ifelse (
                  db$score >= 20, "6-10" , ifelse (
                    db$score >= 17.5, "3-5" , ifelse (
                      db$score >= 14.5, "2" , ifelse (
                        db$score >= 13.5, "1" , ifelse (
                          db$score <= 13, "<1" , NA )))))))))))))) 
    
    
    
  }
  
  
  ###########################TABLE 6#####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    db$ROCF_C_RAW_scale_score <- with ( db, ifelse (
      db$score >= 35.5, 18, ifelse (
        
        
        
        
        db$score >= 34.5, 13, ifelse (
          db$score >= 33.5, 12, ifelse (
            
            db$score >= 30.5, 10, ifelse (
              db$score >= 28.5, 9, ifelse (
                db$score >= 26, 8, ifelse (
                  db$score >= 23, 7, ifelse (
                    db$score >= 21, 6, ifelse (
                      db$score >= 18, 5, ifelse (
                        db$score >= 17.5, 4, ifelse (
                          db$score >= 13.5, 3, ifelse (
                            db$score <= 13, 2, NA )))))))))))))

# percentile score

db$ROCF_C_RAW_percentil_range <- with (db, ifelse (
  db$score >= 35.5, "> 99" , ifelse (
    
    
    
    
    db$score >= 34.5, "82-89" , ifelse (
      db$score >= 33.5, "72-81" , ifelse (
        
        db$score >= 30.5, "41-59" , ifelse (
          db$score >= 28.5, "29-40" , ifelse (
            db$score >= 26, "19-28" , ifelse (
              db$score >= 23, "11-18" , ifelse (
                db$score >= 21, "6-10" , ifelse (
                  db$score >= 18, "3-5" , ifelse (
                    db$score >= 17.5, "2" , ifelse (
                      db$score >= 13.5, "1" , ifelse (
                        db$score <= 13, "<1" , NA )))))))))))))
    
    
  }
  
  
  ############################TABLE 7####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$ROCF_C_RAW_scale_score <- with ( db, ifelse (
      db$score >= 35.5, 18, ifelse (
        
        
        
        
        db$score >= 34.5, 13, ifelse (
          db$score >= 33.5, 12, ifelse (
            db$score >= 32.5, 11, ifelse (
              db$score >= 30.5, 10, ifelse (
                db$score >= 29, 9, ifelse (
                  db$score >= 26, 8, ifelse (
                    db$score >= 23, 7, ifelse (
                      db$score >= 21.5, 6, ifelse (
                        db$score >= 15.5, 5, ifelse (
                          db$score >= 14.5, 4, ifelse (
                            db$score >= 12.5, 3, ifelse (
                              db$score <= 12, 2, NA ))))))))))))))

# percentile score

db$ROCF_C_RAW_percentil_range <- with (db, ifelse (
  db$score >= 35.5, "> 99" , ifelse (
    
    
    
    
    db$score >= 34.5, "82-89" , ifelse (
      db$score >= 33.5, "72-81" , ifelse (
        db$score >= 32.5, "60-71" , ifelse (
          db$score >= 30.5, "41-59" , ifelse (
            db$score >= 29, "29-40" , ifelse (
              db$score >= 26, "19-28" , ifelse (
                db$score >= 23, "11-18" , ifelse (
                  db$score >= 21.5, "6-10" , ifelse (
                    db$score >= 15.5, "3-5" , ifelse (
                      db$score >= 14.5, "2" , ifelse (
                        db$score >= 12.5, "1" , ifelse (
                          db$score <= 12, "<1" , NA ))))))))))))))
    
  }
  
  
  ##############################TABLE 8##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    db$ROCF_C_RAW_scale_score <- with ( db, ifelse (
      db$score >= 35.5, 18, ifelse (
        
        
        
        
        db$score >= 34.5, 13, ifelse (
          
          db$score >= 32.5, 11, ifelse (
            db$score >= 30.5, 10, ifelse (
              db$score >= 28.5, 9, ifelse (
                db$score >= 25.5, 8, ifelse (
                  db$score >= 22.5, 7, ifelse (
                    db$score >= 21.5, 6, ifelse (
                      db$score >= 15.5, 5, ifelse (
                        db$score >= 14.5, 4, ifelse (
                          db$score >= 12.5, 3, ifelse (
                            db$score <= 12, 2, NA )))))))))))))

# percentile score

db$ROCF_C_RAW_percentil_range <- with (db, ifelse (
  db$score >= 35.5, "> 99" , ifelse (
    
    
    
    
    db$score >= 34.5, "82-89" , ifelse (
      
      db$score >= 32.5, "60-71" , ifelse (
        db$score >= 30.5, "41-59" , ifelse (
          db$score >= 28.5, "29-40" , ifelse (
            db$score >= 25.5, "19-28" , ifelse (
              db$score >= 22.5, "11-18" , ifelse (
                db$score >= 21.5, "6-10" , ifelse (
                  db$score >= 15.5, "3-5" , ifelse (
                    db$score >= 14.5, "2" , ifelse (
                      db$score >= 12.5, "1" , ifelse (
                        db$score <= 12, "<1" , NA )))))))))))))
    
    
  }
  
  
  ##############################TABLE 9##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    db$ROCF_C_RAW_scale_score <- with ( db, ifelse (
      db$score >= 35.5, 18, ifelse (
        
        
        
        
        
        db$score >= 34.5, 12, ifelse (
          db$score >= 32.5, 11, ifelse (
            db$score >= 30.5, 10, ifelse (
              db$score >= 28, 9, ifelse (
                db$score >= 25, 8, ifelse (
                  db$score >= 22.5, 7, ifelse (
                    db$score >= 19, 6, ifelse (
                      db$score >= 14.5, 5, ifelse (
                        db$score >= 13.5, 4, ifelse (
                          db$score >= 12.5, 3, ifelse (
                            db$score <= 12, 2, NA )))))))))))))

# percentile score

db$ROCF_C_RAW_percentil_range <- with (db, ifelse (
  db$score >= 35.5, "> 99" , ifelse (
    
    
    
    
    
    db$score >= 34.5, "72-81" , ifelse (
      db$score >= 32.5, "60-71" , ifelse (
        db$score >= 30.5, "41-59" , ifelse (
          db$score >= 28, "29-40" , ifelse (
            db$score >= 25, "19-28" , ifelse (
              db$score >= 22.5, "11-18" , ifelse (
                db$score >= 19, "6-10" , ifelse (
                  db$score >= 14.5, "3-5" , ifelse (
                    db$score >= 13.5, "2" , ifelse (
                      db$score >= 12.5, "1" , ifelse (
                        db$score <= 12, "<1" , NA )))))))))))))  
    
    
  }
  
  
  
  ##############################TABLE 10##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    db$ROCF_C_RAW_scale_score <- with ( db, ifelse (
      db$score >= 36, 18, ifelse (
        db$score >= 35.5, 17, ifelse (
          
          
          
          db$score >= 34.5, 13, ifelse (
            db$score >= 33.5, 12, ifelse (
              db$score >= 32.5, 11, ifelse (
                db$score >= 29.5, 10, ifelse (
                  
                  db$score >= 22.5, 8, ifelse (
                    db$score >= 21.5, 7, ifelse (
                      db$score >= 15.5, 6, ifelse (
                        db$score >= 12.5, 5, ifelse (
                          db$score >= 11.5, 4, ifelse (
                            db$score >= 11, 3, ifelse (
                              db$score <= 10.5, 2, NA ))))))))))))))

# percentile score

db$ROCF_C_RAW_percentil_range <- with (db, ifelse (
  db$score >= 36, "> 99" , ifelse (
    db$score >= 35.5, "99" , ifelse (
      
      
      
      db$score >= 34.5, "82-89" , ifelse (
        db$score >= 33.5, "72-81" , ifelse (
          db$score >= 32.5, "60-71" , ifelse (
            db$score >= 29.5, "41-59" , ifelse (
              
              db$score >= 22.5, "19-28" , ifelse (
                db$score >= 21.5, "11-18" , ifelse (
                  db$score >= 15.5, "6-10" , ifelse (
                    db$score >= 12.5, "3-5" , ifelse (
                      db$score >= 11.5, "2" , ifelse (
                        db$score >= 11, "1" , ifelse (
                          db$score <= 10.5, "<1" , NA ))))))))))))))
    
    
    
  }
  
  
  ##############################TABLE 11##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    db$ROCF_C_RAW_scale_score <- with ( db, ifelse (
      db$score >= 36, 18, ifelse (
        
        
        
        
        db$score >= 34.5, 13, ifelse (
          db$score >= 33.5, 12, ifelse (
            db$score >= 31.5, 11, ifelse (
              db$score >= 29.5, 10, ifelse (
                db$score >= 26, 9, ifelse (
                  db$score >= 22.5, 8, ifelse (
                    db$score >= 21.5, 7, ifelse (
                      db$score >= 16.5, 6, ifelse (
                        db$score >= 11.5, 5, ifelse (
                          
                          db$score >= 11, 3, ifelse (
                            db$score <=  10.5, 2, NA )))))))))))))

# percentile score

db$ROCF_C_RAW_percentil_range <- with (db, ifelse (
  db$score >= 36, "> 99" , ifelse (
    
    
    
    
    db$score >= 34.5, "82-89" , ifelse (
      db$score >= 33.5, "72-81" , ifelse (
        db$score >= 31.5, "60-71" , ifelse (
          db$score >= 29.5, "41-59" , ifelse (
            db$score >= 26, "29-40" , ifelse (
              db$score >= 22.5, "19-28" , ifelse (
                db$score >= 21.5, "11-18" , ifelse (
                  db$score >= 16.5, "6-10" , ifelse (
                    db$score >= 11.5, "3-5" , ifelse (
                      
                      db$score >= 11, "1" , ifelse (
                        db$score <=  10.5, "<1" , NA ))))))))))))) 
    
    
  }
  
  
  #############################TABLE 12###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    db$ROCF_C_RAW_scale_score <- with ( db, ifelse (
      db$score >= 35.5, 18, ifelse (
        
        
        
        
        db$score >= 33.5, 13, ifelse (
          
          db$score >= 30.5, 11, ifelse (
            db$score >= 29.5, 10, ifelse (
              db$score >= 22, 9, ifelse (
                db$score >= 21.5, 8, ifelse (
                  db$score >= 13.5, 7, ifelse (
                    db$score >= 11.5, 6, ifelse (
                      db$score >= 10.5, 5, ifelse (
                        db$score >= 10, 4, ifelse (
                          
                          db$score <=  9.5, 2, NA ))))))))))))

# percentile score

db$ROCF_C_RAW_percentil_range <- with (db, ifelse (
  db$score >= 35.5, "> 99" , ifelse (
    
    
    
    
    db$score >= 33.5, "82-89" , ifelse (
      
      db$score >= 30.5, "60-71" , ifelse (
        db$score >= 29.5, "41-59" , ifelse (
          db$score >= 22, "29-40" , ifelse (
            db$score >= 21.5, "19-28" , ifelse (
              db$score >= 13.5, "11-18" , ifelse (
                db$score >= 11.5, "6-10" , ifelse (
                  db$score >= 10.5, "3-5" , ifelse (
                    db$score >= 10, "2" , ifelse (
                      
                      db$score <=  9.5, "<1" , NA ))))))))))))
    
    
  }
  
  
  # Educational level adjust 
  db$education_years_adj <- with(db, ifelse(
    db$education_years >= 0  & db$education_years <= 2, db$ROCF_C_RAW_scale_score + 2, ifelse(
    db$education_years >= 3  & db$education_years <= 7, db$ROCF_C_RAW_scale_score + 1, ifelse(
      db$education_years >= 8  & db$education_years <= 12, db$ROCF_C_RAW_scale_score, ifelse(
        db$education_years >= 13  & db$education_years <= 16, db$ROCF_C_RAW_scale_score - 1, ifelse(
          db$education_years >= 17  & db$education_years <= 20, db$ROCF_C_RAW_scale_score - 2, ifelse(
            
            )))))))
  
  
  # NSSae
  db$NSSae_ROCF_C_RAW <- db$ROCF_C_RAW_scale_score - (0.21285*(db$education_years_adj-12)) 
  
  return(db)
}
