# Free and Cued Selective Reminding Test (FCSRT) Trial 1

# Reference:
# Peña-Casanova, J., Gramunt-Fombuena, N., Quiñones-Ubeda, M., et al., 2009. Neuronorma
# study team. Spanish Multicenter Normative Studies (NEURONORMA Project): Norms for the Rey–Osterrieth Complex 
# Figure (Copy and Memory), and Free and Cued Selective Reminding Test
# Arch. Clin. Neuropsychol. 24 (4), 371–393.
# 
# 

FCSRT_TRIAL1 <- function(score, age, education_years){
  
  FCSRT_TRIAL1_db <- data.frame(score = score, age = age, education_years = education_years)
  FCSRT_TRIAL1_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(FCSRT_TRIAL1_db)) {
    res <- FCSRT_TRIAL1_scale_score(score = FCSRT_TRIAL1_db[i, "score"], 
                              age = FCSRT_TRIAL1_db[i, "age"],
                              education_years = FCSRT_TRIAL1_db[i, "education_years"])
    FCSRT_TRIAL1_new <- rbind(FCSRT_TRIAL1_new, res)
  }
  
  return(FCSRT_TRIAL1_new[,c("FCSRT_TRIAL1_scale_score", "FCSRT_TRIAL1_percentil_range", "FCSRT_TRIAL1_NSSae")])
}

FCSRT_TRIAL1_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    db$FCSRT_TRIAL1_scale_score <- with ( db, ifelse (
      db$score >= 11, 18, ifelse (
        
        
        db$score >= 10, 15, ifelse (
          
          db$score >= 9, 13, ifelse (
            db$score >= 8, 12, ifelse (
              db$score >= 7, 11, ifelse (
                db$score >= 6, 10, ifelse (
                  
                  db$score >= 5, 8, ifelse (
                    db$score >= 4, 7, ifelse (
                      
                      db$score >= 3, 5, ifelse (
                        db$score >= 2, 4, ifelse (
                          
                          db$score <= 1 , 2, NA ))))))))))))


# percentile score

db$FCSRT_TRIAL1_percentil_range <- with (db, ifelse (
  db$score >= 11, "> 99" , ifelse (
    
    
    db$score >= 10, "95-97" , ifelse (
      
      db$score >= 9, "82-89" , ifelse (
        db$score >= 8, "72-81" , ifelse (
          db$score >= 7, "60-71" , ifelse (
            db$score >= 6, "41-59" , ifelse (
              
              db$score >= 5, "19-28" , ifelse (
                db$score >= 4, "11-18" , ifelse (
                  
                  db$score >= 3, "3-5" , ifelse (
                    db$score >= 2, "2" , ifelse (
                      
                      db$score <= 1 , "<1" , NA ))))))))))))
    
    
    
    
    
  }
  
  
  ##################################TABLE 4##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    db$FCSRT_TRIAL1_scale_score <- with ( db, ifelse (
      db$score >= 12, 18, ifelse (
        db$score >= 11, 17, ifelse (
          
          db$score >= 10, 15, ifelse (
            
            db$score >= 9, 13, ifelse (
              db$score >= 8, 12, ifelse (
                db$score >= 7, 11, ifelse (
                  db$score >= 6, 10, ifelse (
                    db$score >= 5, 9, ifelse (
                      
                      db$score >= 4, 7, ifelse (
                        db$score >= 3, 6, ifelse (
                          db$score >= 2, 5, ifelse (
                            
                            
                            db$score <= 1 , 2, NA )))))))))))))

# percentile score

db$FCSRT_TRIAL1_percentil_range <- with (db, ifelse (
  db$score >= 12, "> 99" , ifelse (
    db$score >= 11, "99" , ifelse (
      
      db$score >= 10, "95-97" , ifelse (
        
        db$score >= 9, "82-89" , ifelse (
          db$score >= 8, "72-81" , ifelse (
            db$score >= 7, "60-71" , ifelse (
              db$score >= 6, "41-59" , ifelse (
                db$score >= 5, "29-40" , ifelse (
                  
                  db$score >= 4, "11-18" , ifelse (
                    db$score >= 3, "6-10" , ifelse (
                      db$score >= 2, "3-5" , ifelse (
                        
                        
                        db$score <= 1 , "<1" , NA ))))))))))))) 
    
    
    
    
    
  }
  
  
  #############################TABLE 5###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    db$FCSRT_TRIAL1_scale_score <- with ( db, ifelse (
      db$score >= 12, 18, ifelse (
        db$score >= 11, 17, ifelse (
          
          db$score >= 10, 15, ifelse (
            
            db$score >= 9, 13, ifelse (
              db$score >= 8, 12, ifelse (
                db$score >= 7, 11, ifelse (
                  db$score >= 6, 10, ifelse (
                    db$score >= 5, 9, ifelse (
                      
                      db$score >= 4, 7, ifelse (
                        db$score >= 3, 6, ifelse (
                          db$score >= 2, 5, ifelse (
                            
                            
                            db$score <= 1 , 2, NA )))))))))))))

# percentile score

db$FCSRT_TRIAL1_percentil_range <- with (db, ifelse (
  db$score >= 12, "> 99" , ifelse (
    db$score >= 11, "99" , ifelse (
      
      db$score >= 10, "95-97" , ifelse (
        
        db$score >= 9, "82-89" , ifelse (
          db$score >= 8, "72-81" , ifelse (
            db$score >= 7, "60-71" , ifelse (
              db$score >= 6, "41-59" , ifelse (
                db$score >= 5, "29-40" , ifelse (
                  
                  db$score >= 4, "11-18" , ifelse (
                    db$score >= 3, "6-10" , ifelse (
                      db$score >= 2, "3-5" , ifelse (
                        
                        
                        db$score <= 1 , "<1" , NA )))))))))))))
    
    
    
  }
  
  
  ###########################TABLE 6#####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    db$FCSRT_TRIAL1_scale_score <- with ( db, ifelse (
      db$score >= 12, 18, ifelse (
        db$score >= 11, 17, ifelse (
          
          db$score >= 10, 15, ifelse (
            db$score >= 9, 14, ifelse (
              db$score >= 8, 13, ifelse (
                db$score >= 7, 12, ifelse (
                  db$score >= 6, 11, ifelse (
                    
                    db$score >= 5, 9, ifelse (
                      db$score >= 4, 8, ifelse (
                        db$score >= 3, 7, ifelse (
                          db$score >= 2, 6, ifelse (
                            
                            
                            
                            db$score <= 1 , 2, NA )))))))))))))

# percentile score

db$FCSRT_TRIAL1_percentil_range <- with (db, ifelse (
  db$score >= 12, "> 99" , ifelse (
    db$score >= 11, "99" , ifelse (
      
      db$score >= 10, "95-97" , ifelse (
        db$score >= 9, "90-94" , ifelse (
          db$score >= 8, "82-89" , ifelse (
            db$score >= 7, "72-81" , ifelse (
              db$score >= 6, "60-71" , ifelse (
                
                db$score >= 5, "29-40" , ifelse (
                  db$score >= 4, "19-28" , ifelse (
                    db$score >= 3, "11-18" , ifelse (
                      db$score >= 2, "6-10" , ifelse (
                        
                        
                        
                        db$score <= 1 , "<1" , NA )))))))))))))
    
    
  }
  
  
  ############################TABLE 7####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$FCSRT_TRIAL1_scale_score <- with ( db, ifelse (
      db$score >= 11, 18, ifelse (
        db$score >= 10, 17, ifelse (
          
          db$score >= 9, 15, ifelse (
            
            db$score >= 8, 13, ifelse (
              db$score >= 7, 12, ifelse (
                db$score >= 6, 11, ifelse (
                  db$score >= 5, 10, ifelse (
                    
                    db$score >= 4, 8, ifelse (
                      db$score >= 3, 7, ifelse (
                        db$score >= 2, 6, ifelse (
                          
                          
                          
                          db$score <= 1 , 2, NA ))))))))))))

# percentile score

db$FCSRT_TRIAL1_percentil_range <- with (db, ifelse (
  db$score >= 11, "> 99" , ifelse (
    db$score >= 10, "99" , ifelse (
      
      db$score >= 9, "95-97" , ifelse (
        
        db$score >= 8, "82-89" , ifelse (
          db$score >= 7, "72-81" , ifelse (
            db$score >= 6, "60-71" , ifelse (
              db$score >= 5, "41-59" , ifelse (
                
                db$score >= 4, "19-28" , ifelse (
                  db$score >= 3, "11-18" , ifelse (
                    db$score >= 2, "6-10" , ifelse (
                      
                      
                      
                      db$score <= 1 , "<1" , NA ))))))))))))
    
  }
  
  
  ##############################TABLE 8##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    db$FCSRT_TRIAL1_scale_score <- with ( db, ifelse (
      db$score >= 11, 18, ifelse (
        db$score >= 10, 17, ifelse (
          
          db$score >= 9, 15, ifelse (
            db$score >= 8, 14, ifelse (
              db$score >= 7, 13, ifelse (
                db$score >= 6, 12, ifelse (
                  
                  db$score >= 5, 10, ifelse (
                    db$score >= 4, 9, ifelse (
                      db$score >= 3, 8, ifelse (
                        db$score >= 2, 7, ifelse (
                          
                          
                          
                          db$score >= 1, 3, ifelse (
                            db$score <= 0, 2, NA )))))))))))))

# percentile score

db$FCSRT_TRIAL1_percentil_range <- with (db, ifelse (
  db$score >= 11, "> 99" , ifelse (
    db$score >= 10, "99" , ifelse (
      
      db$score >= 9, "95-97" , ifelse (
        db$score >= 8, "90-94" , ifelse (
          db$score >= 7, "82-89" , ifelse (
            db$score >= 6, "72-81" , ifelse (
              
              db$score >= 5, "41-59" , ifelse (
                db$score >= 4, "29-40" , ifelse (
                  db$score >= 3, "19-28" , ifelse (
                    db$score >= 2, "11-18" , ifelse (
                      
                      
                      
                      db$score >= 1, "1" , ifelse (
                        db$score <= 0, "<1" , NA )))))))))))))
    
    
  }
  
  
  ##############################TABLE 9##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    db$FCSRT_TRIAL1_scale_score <- with ( db, ifelse (
      db$score >= 11, 18, ifelse (
        
        db$score >= 10, 16, ifelse (
          db$score >= 9, 15, ifelse (
            db$score >= 8, 14, ifelse (
              db$score >= 7, 13, ifelse (
                db$score >= 6, 12, ifelse (
                  
                  db$score >= 5, 10, ifelse (
                    db$score >= 4, 9, ifelse (
                      db$score >= 3, 8, ifelse (
                        
                        db$score >= 2, 6, ifelse (
                          
                          
                          db$score >= 1, 3, ifelse (
                            db$score <= 0, 2, NA )))))))))))))

# percentile score

db$FCSRT_TRIAL1_percentil_range <- with (db, ifelse (
  db$score >= 11, "> 99" , ifelse (
    
    db$score >= 10, "98" , ifelse (
      db$score >= 9, "95-97" , ifelse (
        db$score >= 8, "90-94" , ifelse (
          db$score >= 7, "82-89" , ifelse (
            db$score >= 6, "72-81" , ifelse (
              
              db$score >= 5, "41-59" , ifelse (
                db$score >= 4, "29-40" , ifelse (
                  db$score >= 3, "19-28" , ifelse (
                    
                    db$score >= 2, "6-10" , ifelse (
                      
                      
                      db$score >= 1, "1" , ifelse (
                        db$score <= 0, "<1" , NA )))))))))))))
    
    
  }
  
  
  
  ##############################TABLE 10##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    db$FCSRT_TRIAL1_scale_score <- with ( db, ifelse (
      db$score >= 11, 18, ifelse (
        
        db$score >= 10, 16, ifelse (
          db$score >= 9, 15, ifelse (
            
            db$score >= 7 , 13, ifelse (
              db$score >= 6, 12, ifelse (
                db$score >= 5, 11, ifelse (
                  db$score >= 4, 10, ifelse (
                    db$score >= 3, 9, ifelse (
                      
                      db$score >= 2, 7, ifelse (
                        
                        db$score >= 1, 5, ifelse (
                          
                          
                          db$score <= 0, 2, NA ))))))))))))

# percentile score

db$FCSRT_TRIAL1_percentil_range <- with (db, ifelse (
  db$score >= 11, "> 99" , ifelse (
    
    db$score >= 10, "98" , ifelse (
      db$score >= 9, "95-97" , ifelse (
        
        db$score >= 7 , "82-89" , ifelse (
          db$score >= 6, "72-81" , ifelse (
            db$score >= 5, "60-71" , ifelse (
              db$score >= 4, "41-59" , ifelse (
                db$score >= 3, "29-40" , ifelse (
                  
                  db$score >= 2, "11-18" , ifelse (
                    
                    db$score >= 1, "3-5" , ifelse (
                      
                      
                      db$score <= 0, "<1" , NA ))))))))))))
    
    
    
  }
  
  
  ##############################TABLE 11##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    db$FCSRT_TRIAL1_scale_score <- with ( db, ifelse (
      db$score >= 11, 18, ifelse (
        
        db$score >= 9, 16, ifelse (
          db$score >= 8, 15, ifelse (
            
            db$score >= 7, 13, ifelse (
              db$score >= 6, 12, ifelse (
                db$score >= 5, 11, ifelse (
                  db$score >= 4, 10, ifelse (
                    db$score >= 3, 9, ifelse (
                      
                      db$score >= 2, 7, ifelse (
                        
                        db$score >= 1, 5, ifelse (
                          
                          
                          db$score <= 0, 2, NA ))))))))))))

# percentile score

db$FCSRT_TRIAL1_percentil_range <- with (db, ifelse (
  db$score >= 11, "> 99" , ifelse (
    
    db$score >= 9, "98" , ifelse (
      db$score >= 8, "95-97" , ifelse (
        
        db$score >= 7, "82-89" , ifelse (
          db$score >= 6, "72-81" , ifelse (
            db$score >= 5, "60-71" , ifelse (
              db$score >= 4, "41-59" , ifelse (
                db$score >= 3, "29-40" , ifelse (
                  
                  db$score >= 2, "11-18" , ifelse (
                    
                    db$score >= 1, "3-5" , ifelse (
                      
                      
                      db$score <= 0, "<1" , NA ))))))))))))
    
    
  }
  
  
  #############################TABLE 12###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    db$FCSRT_TRIAL1_scale_score <- with ( db, ifelse (
      db$score >= 9, 18, ifelse (
        
        
        db$score >= 8, 15, ifelse (
          db$score >= 7, 14, ifelse (
            db$score >= 6, 13, ifelse (
              
              db$score >= 5, 11, ifelse (
                db$score >= 4, 10, ifelse (
                  db$score >= 3, 9, ifelse (
                    
                    db$score >= 2, 7, ifelse (
                      
                      db$score >= 1, 5, ifelse (
                        
                        
                        db$score <= 0, 2, NA )))))))))))

# percentile score

db$FCSRT_TRIAL1_percentil_range <- with (db, ifelse (
  db$score >= 9, "> 99" , ifelse (
    
    
    db$score >= 8, "95-97" , ifelse (
      db$score >= 7, "90-94" , ifelse (
        db$score >= 6, "82-89" , ifelse (
          
          db$score >= 5, "60-71" , ifelse (
            db$score >= 4, "41-59" , ifelse (
              db$score >= 3, "29-40" , ifelse (
                
                db$score >= 2, "11-18" , ifelse (
                  
                  db$score >= 1, "3-5" , ifelse (
                    
                    
                    db$score <= 0, "<1" , NA )))))))))))
    
    
  }
  
  
  # Educational level adjust 
  db$FCSRT_TRIAL1_education_years_adj <- with(db, ifelse(
    db$education_years >= 0  & db$education_years <= 5, db$FCSRT_TRIAL1_scale_score + 1, ifelse(
      db$education_years >= 6  & db$education_years <= 12, db$FCSRT_TRIAL1_scale_score, ifelse(
        db$education_years >= 13  & db$education_years <= 18, db$FCSRT_TRIAL1_scale_score - 1, ifelse(
          db$education_years >= 19  & db$education_years <= 20, db$FCSRT_TRIAL1_scale_score - 2, ifelse(
            
            ))))))
  
  
  # NSSae
  db$FCSRT_TRIAL1_NSSae <- db$FCSRT_TRIAL1_scale_score - (0.15266*(db$FCSRT_TRIAL1_education_years_adj-12)) 
  
  return(db)
}
