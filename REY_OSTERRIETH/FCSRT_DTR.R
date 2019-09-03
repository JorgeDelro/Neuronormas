# Free and Cued Selective Reminding Test (FCSRT) Delayed Total Recall

# Reference:
# Peña-Casanova, J., Gramunt-Fombuena, N., Quiñones-Ubeda, M., et al., 2009. Neuronorma
# study team. Spanish Multicenter Normative Studies (NEURONORMA Project): Norms for the Rey–Osterrieth Complex 
# Figure (Copy and Memory), and Free and Cued Selective Reminding Test
# Arch. Clin. Neuropsychol. 24 (4), 371–393.
# 
# 

# 
FCSRT_DTR <- function(score, age, education_years){
  
  FCSRT_DTR_db <- data.frame(score = score, age = age, education_years = education_years)
  FCSRT_DTR_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(FCSRT_DTR_db)) {
    res <- FCSRT_DTR_scale_score(score = FCSRT_DTR_db[i, "score"], 
                              age = FCSRT_DTR_db[i, "age"],
                              education_years = FCSRT_DTR_db[i, "education_years"])
    FCSRT_DTR_new <- rbind(FCSRT_DTR_new, res)
  }
  
  return(FCSRT_DTR_new)
}

FCSRT_DTR_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    db$FCSRT_DTR_scale_score <- with ( db, ifelse (
      db$score >= 16, 18, ifelse (
        
        
        
        
        
        
        
        db$score >= 15, 10, ifelse (
          db$score >= 14, 9, ifelse (
            
            db$score >= 12, 7, ifelse (
              db$score >= 11, 6, ifelse (
                
                db$score >= 9 , 4, ifelse (
                  db$score >= 8, 3, ifelse (
                    db$score <= 7 , 2, NA )))))))))


# percentile score

db$FCSRT_DTR_percentil_range <- with (db, ifelse (
  db$score >= 16, "> 99" , ifelse (
    
    
    
    
    
    
    
    db$score >= 15, "41-59" , ifelse (
      db$score >= 14, "29-40" , ifelse (
        
        db$score >= 12, "11-18" , ifelse (
          db$score >= 11, "6-10" , ifelse (
            
            db$score >= 9 , "2" , ifelse (
              db$score >= 8, "1" , ifelse (
                db$score <= 7 , "<1" , NA )))))))))
    
    
    
    
    
  }
  
  
  ##################################TABLE 4##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    db$FCSRT_DTR_scale_score <- with ( db, ifelse (
      db$score >= 16, 18, ifelse (
        
        
        
        
        
        
        db$score >= 15, 11, ifelse (
          
          db$score >= 14, 9, ifelse (
            db$score >= 13, 8, ifelse (
              db$score >= 12, 7, ifelse (
                db$score >= 11, 6, ifelse (
                  db$score >= 10, 5, ifelse (
                    
                    db$score >= 8, 3, ifelse (
                      db$score <= 7 , 2, NA ))))))))))

# percentile score

db$FCSRT_DTR_percentil_range <- with (db, ifelse (
  db$score >= 16, "> 99" , ifelse (
    
    
    
    
    
    
    db$score >= 15, "60-71" , ifelse (
      
      db$score >= 14, "29-40" , ifelse (
        db$score >= 13, "19-28" , ifelse (
          db$score >= 12, "11-18" , ifelse (
            db$score >= 11, "6-10" , ifelse (
              db$score >= 10, "3-5" , ifelse (
                
                db$score >= 8, "1" , ifelse (
                  db$score <= 7 , "<1" , NA ))))))))))
    
    
    
    
    
  }
  
  
  #############################TABLE 5###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    db$FCSRT_DTR_scale_score <- with ( db, ifelse (
      db$score >= 16, 18, ifelse (
        
        
        
        
        
        
        db$score >= 15, 11, ifelse (
          db$score >= 14, 10, ifelse (
            
            db$score >= 13, 8, ifelse (
              db$score >= 11, 7, ifelse (
                
                db$score >= 10, 5, ifelse (
                  db$score >= 9, 4, ifelse (
                    db$score >= 8, 3, ifelse (
                      db$score <= 7 , 2, NA ))))))))))

# percentile score

db$FCSRT_DTR_percentil_range <- with (db, ifelse (
  db$score >= 16, "> 99" , ifelse (
    
    
    
    
    
    
    db$score >= 15, "60-71" , ifelse (
      db$score >= 14, "41-59" , ifelse (
        
        db$score >= 13, "19-28" , ifelse (
          db$score >= 11, "11-18" , ifelse (
            
            db$score >= 10, "3-5" , ifelse (
              db$score >= 9, "2" , ifelse (
                db$score >= 8, "1" , ifelse (
                  db$score <= 7 , "<1" , NA ))))))))))
    
    
    
  }
  
  
  ###########################TABLE 6#####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    db$FCSRT_DTR_scale_score <- with ( db, ifelse (
      db$score >= 16, 18, ifelse (
        
        
        
        
        
        
        db$score >= 15, 11, ifelse (
          db$score >= 14, 10, ifelse (
            db$score >= 13, 9, ifelse (
              
              db$score >= 11, 7, ifelse (
                db$score >= 10, 6, ifelse (
                  
                  db$score >= 9, 4, ifelse (
                    db$score >= 8, 3, ifelse (
                      db$score <= 7 , 2, NA ))))))))))

# percentile score

db$FCSRT_DTR_percentil_range <- with (db, ifelse (
  db$score >= 16, "> 99" , ifelse (
    
    
    
    
    
    
    db$score >= 15, "60-71" , ifelse (
      db$score >= 14, "41-59" , ifelse (
        db$score >= 13, "29-40" , ifelse (
          
          db$score >= 11, "11-18" , ifelse (
            db$score >= 10, "6-10" , ifelse (
              
              db$score >= 9, "2" , ifelse (
                db$score >= 8, "1" , ifelse (
                  db$score <= 7 , "<1" , NA ))))))))))
    
    
  }
  
  
  ############################TABLE 7####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$FCSRT_DTR_scale_score <- with ( db, ifelse (
      db$score >= 16, 18, ifelse (
        
        
        
        
        
        db$score >= 15, 12, ifelse (
          
          db$score >= 14, 10, ifelse (
            db$score >= 13, 9, ifelse (
              db$score >= 12, 8, ifelse (
                db$score >= 11, 7, ifelse (
                  db$score >= 10, 6, ifelse (
                    db$score >= 8 , 5, ifelse (
                      
                      db$score >= 7, 3, ifelse (
                        db$score <= 6 , 2, NA )))))))))))

# percentile score

db$FCSRT_DTR_percentil_range <- with (db, ifelse (
  db$score >= 16, "> 99" , ifelse (
    
    
    
    
    
    db$score >= 15, "72-81" , ifelse (
      
      db$score >= 14, "41-59" , ifelse (
        db$score >= 13, "29-40" , ifelse (
          db$score >= 12, "19-28" , ifelse (
            db$score >= 11, "11-18" , ifelse (
              db$score >= 10, "6-10" , ifelse (
                db$score >= 8 , "3-5" , ifelse (
                  
                  db$score >= 7, "1" , ifelse (
                    db$score <= 6 , "<1" , NA )))))))))))
    
  }
  
  
  ##############################TABLE 8##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    db$FCSRT_DTR_scale_score <- with ( db, ifelse (
      db$score >= 16, 18, ifelse (
        
        
        
        
        
        db$score >= 15, 12, ifelse (
          
          db$score >= 13, 10, ifelse (
            db$score >= 12, 9, ifelse (
              db$score >= 11, 8, ifelse (
                db$score >= 10, 7, ifelse (
                  db$score >= 9, 6, ifelse (
                    db$score >= 8, 5, ifelse (
                      db$score >= 7, 4, ifelse (
                        
                        db$score <= 6 , 2, NA )))))))))))

# percentile score

db$FCSRT_DTR_percentil_range <- with (db, ifelse (
  db$score >= 16, "> 99" , ifelse (
    
    
    
    
    
    db$score >= 15, "72-81" , ifelse (
      
      db$score >= 13, "41-59" , ifelse (
        db$score >= 12, "29-40" , ifelse (
          db$score >= 11, "19-28" , ifelse (
            db$score >= 10, "11-18" , ifelse (
              db$score >= 9, "6-10" , ifelse (
                db$score >= 8, "3-5" , ifelse (
                  db$score >= 7, "2" , ifelse (
                    
                    db$score <= 6 , "<1" , NA )))))))))))
    
    
  }
  
  
  ##############################TABLE 9##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    db$FCSRT_DTR_scale_score <- with ( db, ifelse (
      db$score >= 16, 18, ifelse (
        
        
        
        
        
        db$score >= 15, 12, ifelse (
          db$score >= 14, 11, ifelse (
            db$score >= 13, 10, ifelse (
              db$score >= 12, 9, ifelse (
                db$score >= 11, 8, ifelse (
                  db$score >= 9 , 7, ifelse (
                    db$score >= 8, 6, ifelse (
                      db$score >= 7, 5, ifelse (
                        
                        
                        db$score <= 6 , 2, NA )))))))))))

# percentile score

db$FCSRT_DTR_percentil_range <- with (db, ifelse (
  db$score >= 16, "> 99" , ifelse (
    
    
    
    
    
    db$score >= 15, "72-81" , ifelse (
      db$score >= 14, "60-71" , ifelse (
        db$score >= 13, "41-59" , ifelse (
          db$score >= 12, "29-40" , ifelse (
            db$score >= 11, "19-28" , ifelse (
              db$score >= 9 , "11-18" , ifelse (
                db$score >= 8, "6-10" , ifelse (
                  db$score >= 7, "3-5" , ifelse (
                    
                    
                    db$score <= 6 , "<1" , NA )))))))))))
    
    
  }
  
  
  
  ##############################TABLE 10##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    db$FCSRT_DTR_scale_score <- with ( db, ifelse (
      db$score >= 16, 18, ifelse (
        
        
        
        
        db$score >= 15, 13, ifelse (
          db$score >= 14, 12, ifelse (
            
            db$score >= 12, 10, ifelse (
              db$score >= 11, 9, ifelse (
                db$score >= 10, 8, ifelse (
                  db$score >= 8 , 7, ifelse (
                    db$score >= 7, 6, ifelse (
                      
                      
                      db$score >= 1 , 3, ifelse (
                        db$score <= 0, 2, NA )))))))))))

# percentile score

db$FCSRT_DTR_percentil_range <- with (db, ifelse (
  db$score >= 16, "> 99" , ifelse (
    
    
    
    
    db$score >= 15, "82-89" , ifelse (
      db$score >= 14, "72-81" , ifelse (
        
        db$score >= 12, "41-59" , ifelse (
          db$score >= 11, "29-40" , ifelse (
            db$score >= 10, "19-28" , ifelse (
              db$score >= 8 , "11-18" , ifelse (
                db$score >= 7, "6-10" , ifelse (
                  
                  
                  db$score >= 1 , "1" , ifelse (
                    db$score <= 0, "<1" , NA )))))))))))
    
    
    
  }
  
  
  ##############################TABLE 11##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    db$FCSRT_DTR_scale_score <- with ( db, ifelse (
      db$score >= 16, 18, ifelse (
        
        
        
        db$score >= 15, 14, ifelse (
          db$score >= 14, 13, ifelse (
            
            db$score >= 13, 11, ifelse (
              db$score >= 12, 10, ifelse (
                db$score >= 10, 9, ifelse (
                  db$score >= 9, 8, ifelse (
                    db$score >= 8, 7, ifelse (
                      db$score >= 7, 6, ifelse (
                        
                        
                        db$score >= 6, 3, ifelse (
                          db$score <= 5, 2, NA ))))))))))))

# percentile score

db$FCSRT_DTR_percentil_range <- with (db, ifelse (
  db$score >= 16, "> 99" , ifelse (
    
    
    
    db$score >= 15, "90-94" , ifelse (
      db$score >= 14, "82-89" , ifelse (
        
        db$score >= 13, "60-71" , ifelse (
          db$score >= 12, "41-59" , ifelse (
            db$score >= 10, "29-40" , ifelse (
              db$score >= 9, "19-28" , ifelse (
                db$score >= 8, "11-18" , ifelse (
                  db$score >= 7, "6-10" , ifelse (
                    
                    
                    db$score >= 6, "1" , ifelse (
                      db$score <= 5, "<1" , NA ))))))))))))
    
    
  }
  
  
  #############################TABLE 12###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    db$FCSRT_DTR_scale_score <- with ( db, ifelse (
      db$score >= 16, 18, ifelse (
        
        
        db$score >= 15, 15, ifelse (
          
          db$score >= 14, 13, ifelse (
            
            db$score >= 13, 11, ifelse (
              db$score >= 12, 10, ifelse (
                db$score >= 10, 9, ifelse (
                  db$score >= 9, 8, ifelse (
                    db$score >= 7, 7, ifelse (
                      
                      
                      db$score >= 6, 4, ifelse (
                        
                        db$score <= 5, 2, NA )))))))))))

# percentile score

db$FCSRT_DTR_percentil_range <- with (db, ifelse (
  db$score >= 16, "> 99" , ifelse (
    
    
    db$score >= 15, "95-97" , ifelse (
      
      db$score >= 14, "82-89" , ifelse (
        
        db$score >= 13, "60-71" , ifelse (
          db$score >= 12, "41-59" , ifelse (
            db$score >= 10, "29-40" , ifelse (
              db$score >= 9, "19-28" , ifelse (
                db$score >= 7, "11-18" , ifelse (
                  
                  
                  db$score >= 6, "2" , ifelse (
                    
                    db$score <= 5, "<1" , NA )))))))))))
    
    
  }
  
  
  # Educational level adjust 
  db$education_years_adj <- with(db, ifelse(
    db$education_years >= 0  & db$education_years < 1, db$FCSRT_DTR_scale_score + 3, ifelse(
      db$education_years >= 1  & db$education_years <= 4, db$FCSRT_DTR_scale_score + 2, ifelse(
    db$education_years >= 5  & db$education_years <= 8, db$FCSRT_DTR_scale_score + 1, ifelse(
      db$education_years >= 9  & db$education_years <= 12, db$FCSRT_DTR_scale_score, ifelse(
        db$education_years >= 13  & db$education_years <= 15, db$FCSRT_DTR_scale_score - 1, ifelse(
          db$education_years >= 16  & db$education_years <= 19, db$FCSRT_DTR_scale_score - 2, ifelse(
            db$education_years > 19  & db$education_years <= 20, db$FCSRT_DTR_scale_score - 3, ifelse(
            )))))))))
  
  
  # NSSae
  db$NSSae_FCSRT_DTR <- db$FCSRT_DTR_scale_score - (0.26748*(db$education_years_adj-12)) 
  
  return(db)
}
