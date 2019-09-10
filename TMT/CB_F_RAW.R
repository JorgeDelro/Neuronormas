# coris blocks forward raw score
#
# Reference:
# Peña-Casanova, J., Quiñones-Ubeda, S., Quintana-Aparicio, M., et al., 2009. Neuronorma
# study team. Spanish multicenter normative studies (neuronorma project): norms for verbal span, 
# visuospatial span, letter and number sequencing, trail making test, and symbol digit modalities test. 
# Arch. Clin. Neuropsychol. 24 (4), 321–341.
# 
# 
CB_F_RAW <- function(score, age, education_years){
  
  CB_F_RAW_db <- data.frame(score = score, age = age, education_years = education_years)
  CB_F_RAW_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(CB_F_RAW_db)) {
    res <- CB_F_RAW_scale_score(score = CB_F_RAW_db[i, "score"], 
                              age = CB_F_RAW_db[i, "age"],
                              education_years = CB_F_RAW_db[i, "education_years"])
    CB_F_RAW_new <- rbind(CB_F_RAW_new, res)
  }
  
  return(CB_F_RAW_new[,c("CB_F_RAW_scale_score", "CB_F_RAW_percentil_range", "CB_F_RAW_NSSae")])
}

CB_F_RAW_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    db$CB_F_RAW_scale_score <- with ( db, ifelse (
      db$score >= 13, 18, ifelse (
        db$score >= 12, 17, ifelse (
          
          db$score >= 11, 15, ifelse (
            db$score >= 10, 14, ifelse (
              db$score >= 9, 13, ifelse (
                
                db$score >= 8, 11, ifelse (
                  db$score >= 6, 10, ifelse (
                    
                    
                    db$score >= 5, 7, ifelse (
                      
                      db$score >= 4, 5, ifelse (
                        db$score >= 3, 4, ifelse (
                          
                          db$score <= 2, 2, NA ))))))))))))


# percentile score

db$CB_F_RAW_percentil_range <- with (db, ifelse (
  db$score >= 13, "> 99" , ifelse (
    db$score >= 12, "99" , ifelse (
      
      db$score >= 11, "95-97" , ifelse (
        db$score >= 10, "90-94" , ifelse (
          db$score >= 9, "82-89" , ifelse (
            
            db$score >= 8, "60-71" , ifelse (
              db$score >= 6, "41-59" , ifelse (
                
                
                db$score >= 5, "11-18" , ifelse (
                  
                  db$score >= 4, "3-5" , ifelse (
                    db$score >= 3, "2" , ifelse (
                      
                      db$score <= 2, "<1" , NA )))))))))))) 
    
    
    
    
    
  }
  
  
  ##################################TABLE 4##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    db$CB_F_RAW_scale_score <- with ( db, ifelse (
      db$score >= 12, 18, ifelse (
        db$score >= 11, 17, ifelse (
          
          
          db$score >= 10, 14, ifelse (
            
            db$score >= 9, 12, ifelse (
              db$score >= 8, 11, ifelse (
                db$score >= 6, 10, ifelse (
                  
                  db$score >= 5, 8, ifelse (
                    
                    db$score >= 4, 6, ifelse (
                      db$score >= 3, 5, ifelse (
                        
                        
                        db$score <= 2, 2, NA )))))))))))

# percentile score

db$CB_F_RAW_percentil_range <- with (db, ifelse (
  db$score >= 12, "> 99" , ifelse (
    db$score >= 11, "99" , ifelse (
      
      
      db$score >= 10, "90-94" , ifelse (
        
        db$score >= 9, "72-81" , ifelse (
          db$score >= 8, "60-71" , ifelse (
            db$score >= 6, "41-59" , ifelse (
              
              db$score >= 5, "19-28" , ifelse (
                
                db$score >= 4, "6-10" , ifelse (
                  db$score >= 3, "3-5" , ifelse (
                    
                    
                    db$score <= 2, "<1" , NA )))))))))))
    
    
    
    
    
  }
  
  
  #############################TABLE 5###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    db$CB_F_RAW_scale_score <- with ( db, ifelse (
      db$score >= 12, 18, ifelse (
        db$score >= 11, 17, ifelse (
          
          db$score >= 10, 15, ifelse (
            db$score >= 9, 14, ifelse (
              
              db$score >= 8, 12, ifelse (
                
                db$score >= 6, 10, ifelse (
                  
                  db$score >= 5, 8, ifelse (
                    
                    db$score >= 4, 6, ifelse (
                      db$score >= 3, 5, ifelse (
                        
                        
                        db$score <= 2, 2, NA )))))))))))

# percentile score

db$CB_F_RAW_percentil_range <- with (db, ifelse (
  db$score >= 12, "> 99" , ifelse (
    db$score >= 11, "99" , ifelse (
      
      db$score >= 10, "95-97" , ifelse (
        db$score >= 9, "90-94" , ifelse (
          
          db$score >= 8, "72-81" , ifelse (
            
            db$score >= 6, "41-59" , ifelse (
              
              db$score >= 5, "19-28" , ifelse (
                
                db$score >= 4, "6-10" , ifelse (
                  db$score >= 3, "3-5" , ifelse (
                    
                    
                    db$score <= 2, "<1" , NA )))))))))))
    
    
    
  }
  
  
  ###########################TABLE 6#####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    db$CB_F_RAW_scale_score <- with ( db, ifelse (
      db$score >= 11, 18, ifelse (
        db$score >= 10, 17, ifelse (
          
          
          db$score >= 9, 14, ifelse (
            
            db$score >= 8, 12, ifelse (
              db$score >= 7, 11, ifelse (
                db$score >= 6, 10, ifelse (
                  
                  db$score >= 5, 8, ifelse (
                    
                    db$score >= 4, 6, ifelse (
                      db$score >= 3, 5, ifelse (
                        
                        
                        db$score <= 2, 2, NA )))))))))))

# percentile score

db$CB_F_RAW_percentil_range <- with (db, ifelse (
  db$score >= 11, "> 99" , ifelse (
    db$score >= 10, "99" , ifelse (
      
      
      db$score >= 9, "90-94" , ifelse (
        
        db$score >= 8, "72-81" , ifelse (
          db$score >= 7, "60-71" , ifelse (
            db$score >= 6, "41-59" , ifelse (
              
              db$score >= 5, "19-28" , ifelse (
                
                db$score >= 4, "6-10" , ifelse (
                  db$score >= 3, "3-5" , ifelse (
                    
                    
                    db$score <= 2, "<1" , NA )))))))))))
    
    
  }
  
  
  ############################TABLE 7####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$CB_F_RAW_scale_score <- with ( db, ifelse (
      db$score >= 10, 18, ifelse (
        
        
        
        db$score >= 9, 14, ifelse (
          
          db$score >= 8, 12, ifelse (
            
            db$score >= 7, 10, ifelse (
              db$score >= 6, 9, ifelse (
                db$score >= 5, 8, ifelse (
                  
                  db$score >= 4, 6, ifelse (
                    db$score >= 3, 5, ifelse (
                      
                      
                      db$score <= 2, 2, NA ))))))))))

# percentile score

db$CB_F_RAW_percentil_range <- with (db, ifelse (
  db$score >= 10, "> 99" , ifelse (
    
    
    
    db$score >= 9, "90-94" , ifelse (
      
      db$score >= 8, "72-81" , ifelse (
        
        db$score >= 7, "41-59" , ifelse (
          db$score >= 6, "29-40" , ifelse (
            db$score >= 5, "19-28" , ifelse (
              
              db$score >= 4, "6-10" , ifelse (
                db$score >= 3, "3-5" , ifelse (
                  
                  
                  db$score <= 2, "<1" , NA ))))))))))
    
  }
  
  
  ##############################TABLE 8##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    db$CB_F_RAW_scale_score <- with ( db, ifelse (
      db$score >= 10, 18, ifelse (
        
        
        
        db$score >= 9, 14, ifelse (
          
          db$score >= 8, 12, ifelse (
            db$score >= 7, 11, ifelse (
              db$score >= 6, 10, ifelse (
                
                db$score >= 5, 8, ifelse (
                  
                  db$score >= 4, 6, ifelse (
                    db$score >= 3, 5, ifelse (
                      
                      
                      db$score <= 2, 2, NA ))))))))))

# percentile score

db$CB_F_RAW_percentil_range <- with (db, ifelse (
  db$score >= 10, "> 99" , ifelse (
    
    
    
    db$score >= 9, "90-94" , ifelse (
      
      db$score >= 8, "72-81" , ifelse (
        db$score >= 7, "60-71" , ifelse (
          db$score >= 6, "41-59" , ifelse (
            
            db$score >= 5, "19-28" , ifelse (
              
              db$score >= 4, "6-10" , ifelse (
                db$score >= 3, "3-5" , ifelse (
                  
                  
                  db$score <= 2, "<1" , NA ))))))))))
    
    
  }
  
  
  ##############################TABLE 9##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    db$CB_F_RAW_scale_score <- with ( db, ifelse (
      db$score >= 10, 18, ifelse (
        
        
        
        db$score >= 9, 14, ifelse (
          
          db$score >= 8, 12, ifelse (
            db$score >= 7, 11, ifelse (
              db$score >= 6, 10, ifelse (
                
                db$score >= 5, 8, ifelse (
                  
                  db$score >= 4, 6, ifelse (
                    db$score >= 3, 5, ifelse (
                      
                      
                      db$score <= 2, 2, NA ))))))))))

# percentile score

db$CB_F_RAW_percentil_range <- with (db, ifelse (
  db$score >= 10, "> 99" , ifelse (
    
    
    
    db$score >= 9, "90-94" , ifelse (
      
      db$score >= 8, "72-81" , ifelse (
        db$score >= 7, "60-71" , ifelse (
          db$score >= 6, "41-59" , ifelse (
            
            db$score >= 5, "19-28" , ifelse (
              
              db$score >= 4, "6-10" , ifelse (
                db$score >= 3, "3-5" , ifelse (
                  
                  
                  db$score <= 2, "<1" , NA ))))))))))
    
    
  }
  
  
  
  ##############################TABLE 10##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    db$CB_F_RAW_scale_score <- with ( db, ifelse (
      db$score >= 10, 18, ifelse (
        
        
        db$score >= 9, 15, ifelse (
          
          db$score >= 8, 13, ifelse (
            db$score >= 7, 12, ifelse (
              
              db$score >= 6, 10, ifelse (
                
                db$score >= 5, 8, ifelse (
                  
                  db$score >= 4, 6, ifelse (
                    db$score >= 3, 5, ifelse (
                      
                      
                      db$score <= 2, 2, NA ))))))))))

# percentile score

db$CB_F_RAW_percentil_range <- with (db, ifelse (
  db$score >= 10, "> 99" , ifelse (
    
    
    db$score >= 9, "95-97" , ifelse (
      
      db$score >= 8, "82-89" , ifelse (
        db$score >= 7, "72-81" , ifelse (
          
          db$score >= 6, "41-59" , ifelse (
            
            db$score >= 5, "19-28" , ifelse (
              
              db$score >= 4, "6-10" , ifelse (
                db$score >= 3, "3-5" , ifelse (
                  
                  
                  db$score <= 2, "<1" , NA ))))))))))
    
    
    
  }
  
  
  ##############################TABLE 11##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    db$CB_F_RAW_scale_score <- with ( db, ifelse (
      db$score >= 11, 18, ifelse (
        db$score >= 10, 17, ifelse (
          
          
          db$score >= 9, 14, ifelse (
            db$score >= 8, 13, ifelse (
              db$score >= 7, 12, ifelse (
                
                db$score >= 6, 10, ifelse (
                  
                  db$score >= 5, 8, ifelse (
                    
                    db$score >= 4, 6, ifelse (
                      db$score >= 3, 5, ifelse (
                        
                        db$score >= 2, 3, ifelse (
                          db$score <= 1, 2, NA ))))))))))))

# percentile score

db$CB_F_RAW_percentil_range <- with (db, ifelse (
  db$score >= 11, "> 99" , ifelse (
    db$score >= 10, "99" , ifelse (
      
      
      db$score >= 9, "90-94" , ifelse (
        db$score >= 8, "82-89" , ifelse (
          db$score >= 7, "72-81" , ifelse (
            
            db$score >= 6, "41-59" , ifelse (
              
              db$score >= 5, "19-28" , ifelse (
                
                db$score >= 4, "6-10" , ifelse (
                  db$score >= 3, "3-5" , ifelse (
                    
                    db$score >= 2, "1" , ifelse (
                      db$score <= 1, "<1" , NA )))))))))))) 
    
    
  }
  
  
  #############################TABLE 12###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    db$CB_F_RAW_scale_score <- with ( db, ifelse (
      db$score >= 11, 18, ifelse (
        
        db$score >= 10, 16, ifelse (
          db$score >= 9, 15, ifelse (
            
            db$score >= 8, 13, ifelse (
              db$score >= 7, 12, ifelse (
                
                db$score >= 6, 10, ifelse (
                  
                  db$score >= 5, 8, ifelse (
                    db$score >= 4, 7, ifelse (
                      db$score >= 3, 6, ifelse (
                        
                        db$score >= 2, 4, ifelse (
                          
                          db$score <= 1, 2, NA ))))))))))))

# percentile score

db$CB_F_RAW_percentil_range <- with (db, ifelse (
  db$score >= 11, "> 99" , ifelse (
    
    db$score >= 10, "98" , ifelse (
      db$score >= 9, "95-97" , ifelse (
        
        db$score >= 8, "82-89" , ifelse (
          db$score >= 7, "72-81" , ifelse (
            
            db$score >= 6, "41-59" , ifelse (
              
              db$score >= 5, "19-28" , ifelse (
                db$score >= 4, "11-18" , ifelse (
                  db$score >= 3, "6-10" , ifelse (
                    
                    db$score >= 2, "2" , ifelse (
                      
                      db$score <= 1, "<1" , NA )))))))))))) 
    
    
  }
  
  
  # Educational level adjust 
  db$CB_F_RAW_education_years_adj <- with(db, ifelse(
    db$education_years >= 0  & db$education_years <= 5, db$CB_F_RAW_scale_score + 1, ifelse(
      db$education_years >= 6  & db$education_years <= 12, db$CB_F_RAW_scale_score, ifelse(
        db$education_years >= 13  & db$education_years <= 18, db$CB_F_RAW_scale_score - 1, ifelse(
          db$education_years >= 19  & db$education_years <= 20, db$CB_F_RAW_scale_score - 2, ifelse(
        
            ))))))
  
  
  # NSSae
  db$CB_F_RAW_NSSae <- db$CB_F_RAW_scale_score - (0.14886*(db$CB_F_RAW_education_years_adj-12)) 
  
  return(db)
}
